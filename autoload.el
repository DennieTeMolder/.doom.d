;;; private/my-config/autoloads.el -*- lexical-binding: t; -*-

;;; Window management
;;;###autoload
(defun my/window-double-height ()
  "Double height of active window"
  (interactive)
  (enlarge-window (window-height)))

;;;###autoload
(defun my/window-half-height ()
  "Halves height of active window"
  (interactive)
  (enlarge-window (/ (window-height) -2)))

;;; File handeling
;;;###autoload
(defun my-project-ignored-p (project-root)
  "Return non-nil if remote or temporary file or a straight package."
  (or (file-remote-p project-root)
      (file-in-directory-p project-root temporary-file-directory)
      (file-in-directory-p project-root doom-local-dir)))

;;;###autoload
(defun my-file-local-readable-p (file)
  "Return non-nil if FILE is local and readable."
  (unless (file-remote-p file)
    (file-readable-p file)))

;;; Workspaces/perspectives
;;;###autoload
(defun my/buffer-move-to-workspace-prompt ()
  "Move current buffer from the current to the selected workspace"
  (interactive)
  (let ((buffer (current-buffer))
        (name (completing-read
               "Move current buffer to workspace:"
               (+workspace-list-names))))
    (my-buffer-move-to-workspace buffer name)))

;;;###autoload
(defun my-workspace-switch-maybe (name)
  "Switch to workspace NAME if not already current"
  (unless (equal name (+workspace-current-name))
    (+workspace-switch name t)
    (+workspace/display)))

;;;###autoload
(defun my/doom-private-goto-workspace ()
  "Open/create the dedicated private config workspace"
  (my-workspace-switch-maybe "*config*"))

;;;###autoload
(defun my-org-roam-goto-workspace (&rest _)
  "Open/create the dedicated org-roam workspace"
  (my-workspace-switch-maybe "*roam*"))

;;; Org
;;;###autoload
(defun my-insert-exit-fill-paragraph ()
  "Perform `org-fill-paragraph' unless el at point is a src block"
  ;; Check if `auto-fill-mode' is active
  (when auto-fill-function
    (unless (memq (org-element-type (org-element-at-point))
                  '(src-block comment-block))
      (org-fill-paragraph))))

;;;###autoload
(defun my-org-get-title-value ()
  "Returns the value of #+TITLE for the current document"
  (car (cdar (org-collect-keywords '("TITLE")))))

;;;###autoload
(defun drestivo-org-download-method (link)
  "This is an helper function for org-download.
It creates an \"./Image\" folder within the same directory of the org file.
File is named as: download name + timestamp + target org file
Based on drestivo's answer to this post: https://github.com/abo-abo/org-download/issues/40.
Which was based off this commit message:
https://github.com/abo-abo/org-download/commit/137c3d2aa083283a3fc853f9ecbbc03039bf397b"
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dir "Images/"))
    (progn
      (setq filename-with-timestamp
            (format "%s%s-<%s>.%s"
                    (file-name-sans-extension filename)
                    (format-time-string org-download-timestamp)
                    (file-name-base (buffer-file-name))
                    (file-name-extension filename)))
      ;; Check if directory exists otherwise create it
      (unless (file-exists-p dir)
        (make-directory dir t))
      (message "Image: %s saved!" (expand-file-name filename-with-timestamp dir))
      (concat dir filename-with-timestamp))))

;;; Org-roam
;;;###autoload
(defun my/org-roam-open-index ()
  "Opens the file specified in my-org-roam-index-file"
  (interactive)
  (my-org-roam-goto-workspace)
  (find-file (expand-file-name my-org-roam-index-file org-roam-directory)))

;;;###autoload
(defun my-org-element-at-point-get-content ()
  "Return the current element's content without properties.
Based on `org-mark-element' and `org-roam-preview-default-function'."
  ;; Move to beginning of item to include children
  (when (org-in-item-p)
    (org-beginning-of-item))
  (let* ((element (org-element-at-point))
         (beg (org-element-property :begin element))
         (end (org-element-property :end element)))
    (string-trim (buffer-substring-no-properties beg end))))

;;;###autoload
;;; Org-roam-dailies
(defun my-org-roam-dailes-calendar--file-to-absolute (file)
  "Convert file name (with gregorian date format) to absolute time"
  (calendar-absolute-from-gregorian (org-roam-dailies-calendar--file-to-date file)))

(defun my-org-roam-dailes-active-files ()
  "Return list of daily files corresponding to TODAY or later"
  (let ((files (org-roam-dailies--list-files))
        (today (calendar-absolute-from-gregorian (calendar-current-date))))
    (while (and files
                (< (my-org-roam-dailes-calendar--file-to-absolute (car files))
                   today))
      (pop files))
    files))

;;;###autoload
(defun my-org-roam-dailies-sync-agenda (&rest _)
  "Scan the dailies-directory and add current and future dates to agenda."
  (mapc (lambda (x) (cl-pushnew x org-agenda-files :test #'string=))
        (my-org-roam-dailes-active-files)))

;;;###autoload
(defun my/org-roam-dailies-schedule-time ()
  "Wrapper around `org-schedule' that only prompts for time.
The DATE is derived from the #+title which must match the Org date format."
  (interactive)
  (unless (org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (let ((date (my-org-get-title-value))
        (time (read-string "Schedule headline at (HH:MM): ")))
    (org-schedule nil (concat date " " time (when (length< time 3) ":00")))))

;;;###autoload
(defun my/org-roam-dailies-insert-timeblock ()
  "Inserts an org roam headline for each hour in START to END with a timestamp.
The DATE is derived from the #+title which must match the Org date format."
  (interactive)
  (let ((date (my-org-get-title-value))
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

;;; Dired
;;;###autoload
(defun my/dired-ediff ()
  "Compare file under cursor to file selected in prompt using Ediff"
  (interactive)
  (let* ((file (dired-get-filename t))
         (dir (dired-current-directory))
         (default nil)
         (target (read-file-name (format-prompt "Diff %s with" default file)
                                 default nil t)))
    (ediff (expand-file-name file dir) target)))

;;; Pdf-tools
;;;###autoload
(defun my/pdf-view-fit-half-height ()
  "Fit PDF height to 2x window (minus 0.1 to fix scrolling)"
  (interactive)
  (pdf-view-fit-height-to-window)
  (let* ((size (pdf-view-image-size))
         (pagesize (pdf-cache-pagesize
                    (pdf-view-current-page)))
         (scale (/ (float (car size))
                   (float (car pagesize)))))
    (setq pdf-view-display-size
          (- (* 2 scale) 0.1))
    (pdf-view-redisplay t)))

;;; Sh-mode
;;;###autoload
(defun thegodzeye/vterm-execute-current-line ()
  "Insert text of current line in vterm and execute.
Based off:
https://www.reddit.com/r/emacs/comments/op4fcm/send_command_to_vterm_and_execute_it/"
  (interactive)
  (require 'vterm)
  (let ((command (buffer-substring
                  (save-excursion
                    (beginning-of-line)
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point)))))
    (let ((cbuf (current-buffer))
          (vbuf (get-buffer vterm-buffer-name)))
      (if vbuf
          (progn
            (unless (doom-visible-buffer-p vbuf)
              (display-buffer vterm-buffer-name t))
            (switch-to-buffer-other-window vterm-buffer-name))
        (progn
          (vterm-other-window)
          (evil-normal-state)))
      (vterm--goto-line -1)
      (vterm-send-string command)
      (vterm-send-return)
      (switch-to-buffer-other-window cbuf)
      (forward-line))))

;;; ESS/R
;;;###autoload
(defun my-ess-insert-string (mystr)
  "Insert string, undo if the same input event is issued twice"
  (let* ((event (event-basic-type last-input-event))
         (char (ignore-errors (format "%c" event))))
    (cond ((and char (ess-inside-string-or-comment-p))
           (insert char))
          ((re-search-backward mystr (- (point) (length mystr)) t)
           (if (and char (numberp event))
               (replace-match char t t)
             (replace-match "")))
          (t (insert mystr)))))

;;;###autoload
(defun my/ess-r-insert-assign (arg)
  "Rewrite of `ess-insert-assign' that respects white space, invoke twice to undo"
  (interactive "p")
  (my-ess-insert-string " <- "))

;;;###autoload
(defun my/ess-r-insert-pipe (arg)
  "Based on `ess-insert-assign', invoking the command twice reverts the insert"
  (interactive "p")
  (my-ess-insert-string " %>% "))

;;;###autoload
(defun my-ess-switch-maybe-a (orig-fn TOGGLE-EOB)
  "Only switch to the REPL if it was already visible"
  (let* ((starting-window (selected-window))
         (ess-process (when ess-current-process-name
                        (get-process ess-current-process-name)))
         (ess-buffer-visible (when ess-process
                               (doom-visible-buffer-p
                                (buffer-name (process-buffer ess-process))))))
    (funcall orig-fn TOGGLE-EOB)
    (evil-normal-state)
    (unless ess-buffer-visible
      (select-window starting-window))))

;;; Python
;;;###autoload
(defun my/python-shell-send-statment-and-step ()
  "Send statement to python shell and move to next"
  (interactive)
  (python-shell-send-region
   (save-excursion (python-nav-beginning-of-statement))
   (save-excursion (python-nav-end-of-statement)))
  (python-nav-forward-statement))

;;;###autoload
(defun my/python-shell-send-block-and-step ()
  "Send block to python shell and move to next statement"
  (interactive)
  (python-shell-send-region
   (save-excursion (python-nav-beginning-of-block))
   (save-excursion (python-nav-end-of-block)))
  (python-nav-end-of-block)
  (python-nav-forward-statement))

;;;###autoload
(defun my/python-send-current-and-step ()
  "Sends statement under point to python shell, if the statement starts a code
block, send the entire code block."
  (interactive)
  ;; Ensure the python process is running
  (unless (python-shell-get-process)
    (save-selected-window (call-interactively #'+python/open-ipython-repl))
    ;; Give python time to load the interaction module
    (sleep-for .5))
  ;; Check for region, start of block, or other and act accordingly
  (cond ((region-active-p)
         (call-interactively #'python-shell-send-region))
        ((python-info-statement-starts-block-p)
         (call-interactively #'my/python-shell-send-block-and-step))
        (t
         (call-interactively #'my/python-shell-send-statment-and-step))))

;;; Conda
;;;###autoload
(defun my-conda-env-promt-activate (env-name)
  "If conda environment with ENV-NAME is not activated, prompt the user to do so"
  (if (and (not (equal env-name conda-env-current-name))
           (y-or-n-p (format "Activate conda env: %s?" env-name)))
      (conda-env-activate (conda-env-name-to-dir env-name))))

;;;###autoload
(defun my/conda-env-guess-prompt ()
  "Guess the currently relevant conda env and prompt user to activate it"
  (interactive)
  (let ((candidate-env (conda--infer-env-from-buffer))
        (fallback-env "base"))
    (cond (candidate-env (my-conda-env-promt-activate candidate-env))
          ((not conda-env-current-name) (my-conda-env-promt-activate fallback-env)))))

;;;###autoload
(defun my-remote-buffer-p (&optional buf)
  ;; And don't save TRAMP buffers; they're super slow to restore
  (let* ((buf (or buf (current-buffer)))
         (dir (buffer-local-value 'default-directory buf)))
    (ignore-errors (file-remote-p dir))))

;;; atomic-chrome
;;;###autoload
(defun my/atomic-chrome-toggle-server ()
  (interactive)
  (if (bound-and-true-p global-atomic-chrome-edit-mode)
      (progn
        (atomic-chrome-stop-server)
        (message "Stopped GhostText Server"))
    (progn
      (atomic-chrome-start-server)
      (message "Started GhostText Server"))))

;;; Vterm
;;;###autoload
(defun tiku91/vterm-redraw-cursor (args)
  "Redraw evil cursor with vterm to keep it consistent with the current state.
Fix by tiku91:
https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-867525845"
  (evil-refresh-cursor evil-state))

;;; Theme recommendations
;;;###autoload
(defun my--theme-which-inactive (theme1 theme2)
  "Return THEME1 of not currently active, else return THEME2"
  (if (eq theme1 (car custom-enabled-themes)) theme2 theme1))

;;;###autoload
(defun my-recommend-theme ()
  "Recommend a NEW theme to use based on context and time of day."
 (if (bound-and-true-p org-tree-slide-mode)
     my-presentation-theme
   (let ((hour (string-to-number (substring (current-time-string) 11 13))))
     (if (member hour (number-sequence my-first-hour-of-day my-last-hour-of-day))
         (my--theme-which-inactive my-day-theme my-solarized-theme)
       (my--theme-which-inactive my-night-theme my-dark-theme)))))

;;;###autoload
(defun my--load-theme-confirm (theme)
  "Load THEME after user confirmation."
  (when (y-or-n-p (format "Activate \"%s\" theme?" theme))
    (mapc #'disable-theme custom-enabled-themes)
    (if (custom-theme-p theme)
        (enable-theme theme)
      (load-theme theme :no-confirm))))

;;;###autoload
(defun my/load-recommended-theme ()
  "Load the theme returned by `my-recommend-theme' after user confirmation."
  (interactive)
  (my--load-theme-confirm (my-recommend-theme)))

;;; Misc
;;;###autoload
(defun my/toggle-trash-delete ()
  "Toggle between trashing and deleting files"
  (interactive)
  (if delete-by-moving-to-trash
      (progn
        (setq delete-by-moving-to-trash nil)
        (message "Now deleting files PERMANTLY"))
    (progn
      (setq delete-by-moving-to-trash t)
      (message "Now moving deleted files to trash"))))
