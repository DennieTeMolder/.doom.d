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

;;* Org-roam-dailies
(defun dtm-org-roam-dailies-file-to-absolute (file)
  "Convert file name (with gregorian date format) to absolute time"
  (calendar-absolute-from-gregorian (org-roam-dailies-calendar--file-to-date file)))

(defun dtm-org-roam-dailies-active-files ()
  "Return list of daily files corresponding to TODAY or later"
  (require 'org-roam-dailies)
  (let ((files (org-roam-dailies--list-files))
        (today (calendar-absolute-from-gregorian (calendar-current-date))))
    (while (and files
                (< (dtm-org-roam-dailies-file-to-absolute (car files))
                   today))
      (pop files))
    files))

(defun dtm-org-roam-dailies-sync-agenda (&rest _)
  "Scan the dailies-directory and add current and future dates to agenda."
  (mapc (lambda (x) (cl-pushnew x org-agenda-files :test #'string=))
        (dtm-org-roam-dailies-active-files)))

(defun dtm/org-roam-dailies-schedule-time ()
  "Wrapper around `org-schedule' that only prompts for time.
The DATE is derived from the #+title which must match the Org date format."
  (interactive)
  (unless (org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (let ((date (file-name-base (buffer-file-name)))
        (time (read-string "Schedule headline at (HH:MM): ")))
    (org-schedule nil (concat date " " time (when (length< time 3) ":00")))))

(defun dtm/org-roam-dailies-insert-timeblock ()
  "Inserts an org roam headline for each hour in START to END with a timestamp.
The DATE is derived from the #+title which must match the Org date format."
  (interactive)
  (let ((date (dtm-org-get-title-value))
        (start (read-number "Start time (hour): " 8))
        (end (- (read-number "End time (hour): " 17) 1)))
    (end-of-line)
    (newline)
    (insert "* Schedule")
    (dolist (hour (number-sequence start end))
      (newline)
      (insert "** EMPTY BLOCK")
      (org-schedule nil (concat date " " (number-to-string hour) ":00"))
      (line-move 1)
      (end-of-line))))

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

;;* Ispell-fu
(defun dtm/ispell-fu-change-dictionary (&optional dict)
  "Set `ispell-local-dictionary' & `spell-fu-dictionaries' to DICT and reload.
Also sets `ispell-local-pdict' to \"default\" if language of
selected dictionary does not match with `ispell-dictionary',
preventing \"expected language x\" errors caused by a language
mismatch with `ispell-personal-dictionary'.
Ref: `ispell-change-dictionary', `spell-fu-dictionary-add'"
  (interactive)
  (require 'consult)
  (require 'ispell)
  (require 'spell-fu)
  (if dict
      (unless (member dict (ispell-valid-dictionary-list))
        (error "Specified dictionary '%s' is invalid!" dict))
    (setq dict (consult--read (mapcar #'list (ispell-valid-dictionary-list))
                              :prompt "Change buffer-local dictionary: "
                              :default (or ispell-local-dictionary ispell-dictionary)
                              :require-match t)))
  (setq ispell-local-dictionary dict
        ispell-local-dictionary-overridden t
        ispell-buffer-session-localwords nil)
  (if (string= (spell-fu--aspell-lang-from-dict ispell-local-dictionary)
               (spell-fu--aspell-lang-from-dict ispell-dictionary))
      (kill-local-variable 'ispell-local-pdict)
    (setq ispell-local-pdict "default"))
  (ispell-internal-change-dictionary)
  (run-hooks 'ispell-change-dictionary-hook)
  (setq spell-fu-dictionaries (spell-fu--default-dictionaries))
  (when spell-fu-mode
    (mapc #'spell-fu--dictionary-ensure-update spell-fu-dictionaries)
    (spell-fu--refresh-cache-table-list)
    (spell-fu--refresh)))

(defun dtm-spell-fu-dict-word-files ()
  "Update and return the word files corresponding to `spell-fu-dictionaries'."
  (require 'spell-fu)
  (unless spell-fu-mode
    (mapc #'spell-fu--dictionary-ensure-update spell-fu-dictionaries))
  (mapcar #'spell-fu--words-file spell-fu-dictionaries))

(defun dtm-spell-fu-bounds-word-at-point ()
  "Return the bounds of word at the current point or nil.
Based on `spell-fu--word-at-point'."
  (let ((point-init (point))
        (pos-beg (pos-bol))
        (pos-end (pos-eol)))
    (save-excursion
      (goto-char pos-beg)
      (catch 'result
        (with-syntax-table spell-fu-syntax-table
          (save-match-data
            (while (re-search-forward spell-fu-word-regexp pos-end t)
              (when (and (<= (match-beginning 0) point-init) (<= point-init (match-end 0)))
                (throw 'result (cons (match-beginning 0) (match-end 0)))))))
        (throw 'result nil)))))

(defun dtm/spell-correct ()
  "Wrap `+spell/correct' to use `dtm-spell-fu-bounds-word-at-point'."
  (interactive)
  (letf! ((defun bounds-of-thing-at-point (&rest _)
            (dtm-spell-fu-bounds-word-at-point))
          (defun thing-at-point (&rest _)
            (when-let ((bounds (dtm-spell-fu-bounds-word-at-point)))
              (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    ;; Set `this-command' for `vertico-multiform-commands'
    (let ((this-command '+spell/correct))
      (+spell/correct))))
