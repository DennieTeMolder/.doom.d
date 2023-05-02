;;; graveyard.el -*- lexical-binding: t; -*-
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
