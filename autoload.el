;;; private/my-config/autoloads.el -*- lexical-binding: t; -*-

;;; Theme recommendations
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

(defun my--load-theme-confirm (theme)
  "Load THEME after user confirmation."
  (when (y-or-n-p (format "Activate \"%s\" theme?" theme))
    (mapc #'disable-theme custom-enabled-themes)
    (if (custom-theme-p theme)
        (enable-theme theme)
      (load-theme theme :no-confirm))
    ;; Reload silently to remove artefacts
    (let ((inhibit-message t))
      (doom/reload-theme))))

;;;###autoload
(defun my/load-recommended-theme ()
  "Load the theme returned by `my-recommend-theme' after user confirmation."
  (interactive)
  (my--load-theme-confirm (my-recommend-theme)))

;;; UI
;;;###autoload
(defun my-doom-modeline-conditional-buffer-encoding ()
  "Only display encoding in modeline when it's not UTF-8"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

;;;###autoload
(defun my-doom-ascii-banner-fn ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'' ' '`---'`---'`---'"
            "                       DOOM"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

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
(defun my/load-session (file)
  "Stripped down `doom/load-session' with proper default value.
Also checks if FILE exists."
  (interactive
   (let ((session-file (doom-session-file)))
     (list (read-file-name "Session to restore: "
                           (file-name-directory session-file)
                           session-file
                           t))))
  (doom-load-session file)
  (message "Session restored. Welcome back."))

(defun my-buffer-move-to-workspace (buffer name)
  "Move BUFFER from the original workspace to NAME and switch"
  (let ((origin (+workspace-current-name)))
    (+workspace-switch name t)
    (persp-add-buffer buffer (+workspace-get name) t nil)
    (+workspace-switch origin)
    (persp-remove-buffer buffer (+workspace-get origin) nil t nil nil)
    (+workspace-switch name)
    (+workspace/display)))

;;;###autoload
(defun my/buffer-move-to-workspace-prompt ()
  "Move current buffer from the current to the selected workspace"
  (interactive)
  (let ((buffer (current-buffer))
        (name (completing-read
               "Move current buffer to workspace:"
               (+workspace-list-names))))
    (my-buffer-move-to-workspace buffer name)))

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

;;; Ediff
;;;###autoload
(defun my/ediff-this-file ()
  "Ediff file associated with current buffer to file selected in prompt."
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p "Save current buffer?"))
    (save-buffer))
  (let ((current (buffer-file-name (current-buffer))))
    (unless current
      (user-error "No file associated with current buffer!"))
    (ediff current (read-file-name "Diff current file with:" nil nil t))))

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

;;; Org-mode
;;;###autoload
(defun my-org-mode-setup-h ()
  "Personal org-mode customisation's after mode startup"
  (setq-local line-spacing my-org-line-spacing
              auto-hscroll-mode nil)
  (electric-quote-local-mode +1)
  (visual-line-mode -1)
  (auto-fill-mode +1)
  (add-hook! 'evil-insert-state-exit-hook
             :local #'my-insert-exit-fill-paragraph))

;;;###autoload
(defun my-insert-exit-fill-paragraph ()
  "Perform `org-fill-paragraph' unless el at point is a src block"
  ;; Check if `auto-fill-mode' is active
  (when auto-fill-function
    (unless (memq (org-element-type (org-element-at-point))
                  '(src-block comment-block))
      (org-fill-paragraph))))

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

;;; Org-modern
;;;###autoload
(defun my-org--modern-indent-heading ()
  "Correctly indents heading assuming leading stars are fully hidden (not invisible)."
  (dotimes (n org-indent--deepest-level)
    (unless (= n 0)
      (let* ((indentation (* org-indent-indentation-per-level (1- n)))
             (heading-prefix (make-string indentation ?\s)))
        (aset org-indent--heading-line-prefixes
              n
              (org-add-props heading-prefix nil 'face 'org-indent))))))

;;; Org-tree-slide
;;;###autoload
(defun my-org-tree-slide-setup-h ()
  "Additional settings to prettify presentations"
  (if org-tree-slide-mode
      (progn
        (setq-local buffer-read-only t
                    evil-normal-state-cursor 'hbar)
        (display-line-numbers-mode -1)
        (hl-line-mode -1)
        (mixed-pitch-mode +1)
        (org-appear-mode -1)
        (add-hook! 'pdf-view-mode-hook :append #'org-tree-slide-mode))
    (progn
      (setq-local buffer-read-only nil)
      (mixed-pitch-mode -1)
      (remove-hook! 'pdf-view-mode-hook #'org-tree-slide-mode)))
  (redraw-display))

;;;###autoload
(defun my-org-tree-slide-no-squiggles-a (orig-fn &optional ARG)
  "Toggle modes that litter the buffer with squiggly lines."
  (let ((ARG (if (memq ARG '(nil toggle))
                 (if org-tree-slide-mode -1 +1)
               ARG)))
    (if (< 0 ARG)
        (progn
          (outline-show-all)
          (flycheck-mode -1)
          (spell-fu-mode -1)
          (funcall orig-fn ARG))
      (funcall orig-fn ARG)
      (outline-show-all)
      (flycheck-mode +1)
      (spell-fu-mode +1))))

;;; Org-appear
(defun my-org-pretty-use-appear-a ()
  "Activate `org-appear-mode' based on `org-pretty-entities'."
  (org-appear-mode (if org-pretty-entities +1 -1)))

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
(defun hlissner-org-roam-add-preamble-a (string)
  "Add information about current node to top of org roam buffer."
  (let ((node org-roam-buffer-current-node))
    (insert
     (format "%-10s %s\n" (propertize "ID:" 'face 'bold)
             (org-roam-node-id node))
     (format "%-10s %s\n" (propertize "Type:" 'face 'bold)
             (if-let (type (org-roam-node-doom-type node))
                 (capitalize type)
               "-"))
     (format "%-10s %s\n" (propertize "Tags:" 'face 'bold)
             (if-let (tags (org-roam-node-tags node))
                 (mapconcat (lambda (tag)
                              (propertize (concat "#" tag) 'face 'org-tag))
                            tags " ")
               "-"))
     (format "%-10s %s\n" (propertize "Aliases:" 'face 'bold)
             (if-let (aliases (org-roam-node-aliases node))
                 (string-join aliases ", ")
               "-"))
     ?\n)))

(defvar org-roam-old-slug nil)

(defun hlissner-org-roam-update-slug-h ()
  "Rename the current file if #+title has changed.
Will ask for confirmation if the new filename already exists."
  (when (org-roam-buffer-p)
    (when-let* ((node (org-roam-node-at-point))
                (new-slug (org-roam-node-slug node))
                (old-slug org-roam-old-slug)
                (old-slug-re (concat "/[^/]*\\(" (regexp-quote old-slug) "\\)[^/]*\\.org$"))
                (file-name (org-roam-node-file node))
                ((not (equal old-slug new-slug)))
                ((string-match-p old-slug-re file-name)))
      (setq org-roam-old-slug new-slug)
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
         (setq org-roam-old-slug old-slug))))))

;;;###autoload
(defun hlissner-org-roam-update-slug-on-save-h ()
  "Set up auto-updating for the current node's filename.
Calls `hlissner-org-roam-update-slug-h' on `after-save-hook'."
  (setq-local org-roam-old-slug (ignore-errors (org-roam-node-slug (org-roam-node-at-point))))
  (add-hook 'after-save-hook #'hlissner-org-roam-update-slug-h
            'append 'local))

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

;;; Vterm
;;;###autoload
(defun tiku91/vterm-redraw-cursor (args)
  "Redraw evil cursor with vterm to keep it consistent with the current state.
Fix by tiku91:
https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-867525845"
  (evil-refresh-cursor evil-state))

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
(defun my--conda-env-promt-activate (env-name)
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
    (cond (candidate-env (my--conda-env-promt-activate candidate-env))
          ((not conda-env-current-name) (my--conda-env-promt-activate fallback-env)))))

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

;;; Interaction-log-mode
;;;###autoload
(defun my/interaction-log-mode-w-buffer ()
  "Toggles `interaction-log-mode' and shows/hides its buffer"
  (interactive)
  (call-interactively #'interaction-log-mode)
  (if interaction-log-mode
      (progn
        (sleep-for .1)
        (display-buffer ilog-buffer-name))
    (when-let ((window (get-buffer-window ilog-buffer-name)))
      (delete-window window))
    (kill-buffer ilog-buffer-name)))

;;; Good-scroll-mode
(defun my/good-scroll-down-half ()
  (interactive)
  (good-scroll-move (/ (good-scroll--window-usable-height) 2)))

(defun my/good-scroll-up-half ()
  (interactive)
  (good-scroll-move (/ (good-scroll--window-usable-height) -2)))

;;;###autoload
(defun my-good-scroll-evil-override-h ()
  (if good-scroll-mode
      (progn
        (advice-add 'evil-scroll-down :override #'my/good-scroll-down-half)
        (advice-add 'evil-scroll-up :override #'my/good-scroll-up-half))
    (progn
      (advice-remove 'evil-scroll-down #'my/good-scroll-down-half)
      (advice-remove 'evil-scroll-up #'my/good-scroll-up-half))))

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