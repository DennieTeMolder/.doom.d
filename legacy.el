;;; legacy.el -*- lexical-binding: t; -*-
;; This file contains unused functions I could not bear to delete

;;* Python
(defun dtm/python-shell-send-statment-and-step ()
  "Send statement to python shell and move to next"
  (interactive)
  (python-shell-send-region
   (save-excursion (python-nav-beginning-of-statement))
   (save-excursion (python-nav-end-of-statement)))
  (python-nav-forward-statement))

(defun dtm/python-shell-send-block-and-step ()
  "Send block to python shell and move to next statement"
  (interactive)
  (python-shell-send-region
   (save-excursion (python-nav-beginning-of-block))
   (save-excursion (python-nav-end-of-block)))
  (python-nav-end-of-block)
  (python-nav-forward-statement))

(defun dtm/python-send-current-and-step ()
  "Sends statement under point to python shell, if the statement starts a code
block, send the entire code block."
  (interactive)
  ;; Check for region, start of block, or other and act accordingly
  (cond ((region-active-p)
         (call-interactively #'python-shell-send-region))
        ((python-info-statement-starts-block-p)
         (call-interactively #'dtm/python-shell-send-block-and-step))
        (t
         (call-interactively #'dtm/python-shell-send-statment-and-step))))

;;* Conda
(defun dtm-conda-call-json-a (orig-fn &rest args)
  "Advice that forces `json-parse-string' to use nil to represent false.
Intended as around advice for `conda--call-json'"
  (require 'json)
  (letf! ((defun json-parse-string (str &rest options)
            (apply json-parse-string str
                   (plist-put options :false-object nil))))
    (apply orig-fn args)))

;;* Window management
(defun dtm/window-half-height ()
  "Halves height of active window"
  (interactive)
  (enlarge-window (/ (window-height) -2)))

;;* ESS/R
(defun dtm-ess-switch-maybe-a (orig-fn &rest args)
  "Only switch to the REPL if it was already visible.
Use as `ess-switch-to-inferior-or-script-buffer' :around advice"
  (let ((win-start (selected-window))
        (ibuf-visible (get-buffer-window (ess-get-process-buffer))))
    (apply orig-fn args)
    (evil-normal-state)
    (unless ibuf-visible (select-window win-start))))

;;* Perspectives/workspaces
(defun dtm-display-buffer-in-workspace (buffer alist)
  "Display BUFFER in (workspace . name) defined in ALIST.
Intended for use in `display-buffer-alist'."
  (let ((name (cdr (assq 'workspace alist))))
    (let ((alist (remove (assq 'workspace alist) alist)))
      (dtm/buffer-move-to-workspace name alist))))

(defun dtm-set-workspace-rule (predicate name)
  "Move buffers matching PREDICATE to workspace NAME.
This is achieved by adding a rule to `display-buffer-alist'."
  (let ((rule `(,predicate (dtm-display-buffer-in-workspace)
                           (workspace . ,name))))
    (push rule display-buffer-alist)
    ;; HACK prevent rule from being overridden by `set-popup-rule!'
    (when (boundp '+popup--display-buffer-alist)
      (push rule +popup--display-buffer-alist)))
  t)

;;* Org-mode
(defun dtm-org-at-keyword-p ()
  "Return non-nil if point is at a #+KEYWORD: line."
  (string-match org-keyword-regexp (buffer-substring-no-properties (bol) (eol))))

(defun dtm-org-edit-keyword ()
  "Narrow to keyword value and fill. This makes it easy to edit long lines."
  (when (dtm-org-at-keyword-p)
    (beginning-of-line)
    (search-forward ":")
    (forward-to-word 1)
    (insert "\n")
    (narrow-to-region (bol) (eol))
    (org-fill-paragraph)
    (add-hook 'org-ctrl-c-ctrl-c-hook #'dtm-org-edit-keyword-finalize nil 'local)
    (message (substitute-command-keys
              "Press \\[org-ctrl-c-ctrl-c] to commit your changes."))))

(defun dtm-org-edit-keyword-finalize ()
  "Collect new keyword value and widen buffer."
  (when (buffer-narrowed-p)
    (join-line nil (point-min) (point-max))
    (widen)
    (join-line)
    (recenter)
    (remove-hook 'org-ctrl-c-ctrl-c-hook #'dtm-org-edit-keyword-finalize 'local)))

(defun dtm/org-edit-special ()
  "Modified version of `org-edit-special' that also works on #+KEYWORDS:."
  (interactive)
  (unless (dtm-org-edit-keyword)
    (call-interactively #'org-edit-special)))

;;* Org-roam
(defvar dtm-org-roam-old-slug nil)

(defun dtm-org-roam-update-slug-h ()
  "Rename the current file if #+title has changed.
Will ask for confirmation if the new filename already exists.
Ref: https://github.com/hlissner/.doom.d"
  (when (org-roam-buffer-p)
    (when-let* ((node (org-roam-node-at-point))
                (new-slug (org-roam-node-slug node))
                (old-slug dtm-org-roam-old-slug)
                (old-slug-re (concat "/[^/]*\\(" (regexp-quote old-slug) "\\)[^/]*\\.org$"))
                (file-name (org-roam-node-file node))
                ((not (equal old-slug new-slug)))
                ((string-match-p old-slug-re file-name)))
      (setq dtm-org-roam-old-slug new-slug)
      (condition-case _
          (let ((new-file-name
                 (replace-regexp-in-string
                  old-slug-re (regexp-quote new-slug)
                  file-name nil nil 1)))
            (message "Updating slug in filename (%S -> %S)" old-slug new-slug)
            (rename-file file-name new-file-name 1)
            (set-visited-file-name new-file-name t t)
            (org-roam-db-autosync--setup-file-h))
        (error
         (setq dtm-org-roam-old-slug old-slug))))))

(defun dtm-org-roam-update-slug-on-save-h ()
  "Set up auto-updating for the current node's filename.
Calls `dtm-org-roam-update-slug-h' on `after-save-hook'.
Ref: https://github.com/hlissner/.doom.d"
  (setq-local dtm-org-roam-old-slug (ignore-errors (org-roam-node-slug (org-roam-node-at-point))))
  (add-hook 'after-save-hook #'dtm-org-roam-update-slug-h 'append 'local))

;;* Visual-line-mode
(defun dtm-visual-line-sync-fringe (symbol newval operation where)
  "Show a left fringe continuation indicator if line numbers are hidden.
Use with `add-variable-watcher' on `display-line-numbers'"
  (when (and (eq symbol 'display-line-numbers)
             (eq operation 'set)
             (buffer-local-value 'visual-line-mode where))
    (setcar (cdr (cl-find 'continuation
                          (buffer-local-value 'fringe-indicator-alist where)
                          :key #'car))
            (when (memq newval '(nil visual)) 'left-curly-arrow))))

(defun dtm-visual-line-fix-linum-h ()
  "Ensure appropriate `display-line-numbers' and `display-line-numbers-type'.
Use for `visual-line-mode-hook'."
  (let ((wrong-type (if visual-line-mode 'relative 'visual))
        (correct-type (if visual-line-mode 'visual 'relative)))
    (when (eq display-line-numbers wrong-type)
      (setq-local display-line-numbers correct-type))
    (when (eq display-line-numbers-type wrong-type)
      (setq-local display-line-numbers-type correct-type))))

;;* Company
(defun dtm/company-files-continue ()
  "Call `company-files' and prompt to continue completion using \"/\".
For use when `company-idle-delay' is nil."
  (interactive)
  (call-interactively #'company-files)
  (add-hook 'company-after-completion-hook #'dtm-company-files-continue-h))

(defun dtm-company-files-continue-h (candidate)
  "Prompt user to call `dtm/company-files-continue' if CANDIDATE is a directory.
Intended as a transient for `company-after-completion-hook'."
  (remove-hook 'company-after-completion-hook #'dtm-company-files-continue-h)
  (when (and (stringp candidate)
             (directory-name-p candidate))
    (when company-files-chop-trailing-slash
      (insert (substring candidate -1)))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "/") #'dtm/company-files-continue)
       map))
    (message "%s" (concat "Press " (propertize "/" 'face 'help-key-binding)
                          " to continue completion."))))

(defun dtm-ispell-fu-ensure-dicts ()
  "Sorts `spell-fu-dictionaries' and returns the corresponding files.
Ensures compatibility with `ispell-complete-word-dict' (and linux look)."
  (unless spell-fu-mode
    (mapc #'spell-fu--dictionary-ensure-update spell-fu-dictionaries))
  (mapcar (lambda (dict)
            (let ((file (spell-fu--words-file dict))
                  (cache (spell-fu--cache-file dict))
                  timestamp status)
              (setq timestamp
                    (file-name-concat
                     (file-name-directory file)
                     (concat "." (file-name-nondirectory file) ".last_sorted")))
              (unless (and (file-exists-p timestamp)
                           (file-newer-than-file-p timestamp file))
                ;; Sort only on [[:alnum:] ] -> required for look's binary search
                (setq status
                      (call-process "sort" nil nil nil
                                    "-f" "-d" file "-o" file))
                (unless (and (numberp status) (= 0 status))
                  (warn "Ispell-fu: 'sort' process for '%s' returned %s" file status))
                (call-process "touch" nil nil nil cache)
                ;; Update timestamp
                (write-region "" nil timestamp))
              file))
          spell-fu-dictionaries))

(defun dtm-ispell-fu-lookup-words (word &rest _)
  "Lookup word in `spell-fu-dictionaries' if `company-ispell-dictionary' is unset.
Can be used to replace `company-ispell--lookup-words' (i.e. via `defalias')."
  (require 'spell-fu)
  (apply #'nconc (mapcar (lambda (dict) (when dict (ispell-lookup-words word dict)))
                         (or (and company-ispell-dictionary
                                  (list company-ispell-dictionary))
                             (and spell-fu-dictionaries
                                  (dtm-ispell-fu-ensure-dicts))
                             (list (or ispell-complete-word-dict
                                       ispell-alternate-dictionary))))))

(defun dtm/company-manual-dict-ispell ()
  "Call `company-dict' and `company-ispell', based on `spell-fu-faces-include'."
  (interactive)
  (require 'spell-fu)
  (let ((company-backends (list (if (spell-fu--check-faces-at-point (point))
                                    '(company-ispell company-dict)
                                  '(company-dict :separate company-ispell)))))
    (unless (company-manual-begin)
      (message "No completions found in %s" company-backends))))
