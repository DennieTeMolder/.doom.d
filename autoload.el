;;; autoloads.el -*- lexical-binding: t; -*-

;;; Utility
;;;###autoload
(defun dtm-file-local-readable-p (file)
  "Return non-nil if FILE is local and readable."
  (unless (file-remote-p file)
    (file-readable-p file)))

;;;###autoload
(defun dtm-doctor-running-p ()
  "Returns t when the doom doctor CLI is running.
Required because doctor sets `noninteractive' to nil."
  (boundp 'doom-doctor--errors))

;;;###autoload
(defun dtm-evil-repeat-ignore (&rest symbol)
  "Instruct `evil-repeat' to ignore commands with SYMBOL."
  (unless symbol
    (error "SYMBOL should be provided!"))
  (dolist (current symbol)
    (evil-add-command-properties current :repeat nil)))

(defun dtm-region-as-string ()
  "Return the marked region as string."
  (buffer-substring-no-properties (mark) (point)))

(defun dtm-point-mark-same-line-p ()
  "Returns t if point and mark are on the same line"
  (<= (line-beginning-position) (mark) (line-end-position)))

;;; Buffer functions
(defun dtm-buffer-remote-p (&optional buf)
  "Returns t if BUF belongs to a remote directory."
  (let* ((buf (or buf (current-buffer)))
         (dir (buffer-local-value 'default-directory buf)))
    (ignore-errors (file-remote-p dir))))

(defun dtm-get-buffer (b-or-n)
  "Wrapper for `get-buffer', that handles `read-buffer' cons cells."
  (let ((buf (cond ((listp b-or-n) (cdr b-or-n))
                   ((stringp b-or-n) (get-buffer b-or-n))
                   (t b-or-n))))
    (unless (bufferp buf)
      (error "No buffer found for: %s" b-or-n))
    buf))

(defun dtm-get-buffer-name (b-or-n)
  "Wrapper for `buffer-name', that handles `read-buffer' cons cells."
  (let ((bname (cond ((listp b-or-n) (car b-or-n))
                     ((bufferp b-or-n) (buffer-name b-or-n))
                     (t b-or-n))))
    (unless (stringp bname)
      (error "Could not find buffer string for: %s" b-or-n))
    bname))

(defun dtm-read-display-buffer (prompt &optional predicate)
  "Display buffer by providing user with PROMPT on buffers matching PREDICATE."
  (when-let ((buf (read-buffer (format-prompt prompt nil)
                               nil t predicate)))
    (display-buffer buf)))

;;; Theme recommendations
(defun dtm--theme-which-inactive (theme1 theme2)
  "Return THEME1 of not currently active, else return THEME2"
  (if (eq theme1 (car custom-enabled-themes)) theme2 theme1))

;;;###autoload
(defun dtm-recommend-theme ()
  "Recommend a NEW theme to use based on context and time of day."
 (if (bound-and-true-p org-tree-slide-mode)
     dtm-presentation-theme
   (let ((hour (string-to-number (substring (current-time-string) 11 13))))
     (if (member hour (number-sequence dtm-first-hour-of-day dtm-last-hour-of-day))
         (dtm--theme-which-inactive dtm-day-theme dtm-solarized-theme)
       (dtm--theme-which-inactive dtm-night-theme dtm-dark-theme)))))

(defun dtm--load-theme-confirm (theme)
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
(defun dtm/load-recommended-theme ()
  "Load the theme returned by `dtm-recommend-theme' after user confirmation."
  (interactive)
  (dtm--load-theme-confirm (dtm-recommend-theme)))

;;; UI
;;;###autoload
(defun dtm-doom-modeline-conditional-buffer-encoding ()
  "Only display encoding in modeline when it's not UTF-8"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

;;;###autoload
(defun dtm-doom-ascii-banner-fn ()
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
(defun dtm/window-double-height ()
  "Double height of active window"
  (interactive)
  (enlarge-window (window-height)))

;;;###autoload
(defun dtm/window-half-height ()
  "Halves height of active window"
  (interactive)
  (enlarge-window (/ (window-height) -2)))

;;; Projectile
;;;###autoload
(defun dtm-project-ignored-p (project-root)
  "Return non-nil if remote or temporary file or a straight package."
  (or (file-remote-p project-root)
      (file-in-directory-p project-root temporary-file-directory)
      (file-in-directory-p project-root doom-local-dir)))

;;; Workspaces/perspectives
;;;###autoload
(defun dtm/load-session (file)
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

(defun dtm-buffer-orphan-p (&optional buf)
  "Return t if buffer BUF does not belong to any workspace/perspective."
  (let ((buf (or buf (current-buffer))))
    (not (persp--buffer-in-persps (dtm-get-buffer buf)))))

;;;###autoload
(defun dtm/switch-orphan-buffer ()
  "Prompt user to select buffer matching `dtm-buffer-orphan-p'."
  (interactive)
  (dtm-read-display-buffer "Select orphan buffer" #'dtm-buffer-orphan-p))

;;;###autoload
(defun dtm/ibuffer-orphans ()
  "Open an ibuffer window with all orphan buffers."
  (interactive)
  (let ((ibuffer-never-show-predicates '(persp--buffer-in-persps))
        (ibuffer-hook nil))
    (ibuffer)))

(defun dtm-workspace-switch-maybe (name)
  "Switch to workspace NAME if not already current"
  (unless (equal name (+workspace-current-name))
    ;; Recycle current workspace if empty
    (if (or (+workspace-exists-p name)
            (+workspace-buffer-list))
        (+workspace-switch name t)
      (+workspace-rename (+workspace-current-name) name))
    (+workspace/display)))

;;;###autoload
(defun dtm-doom-private-goto-workspace ()
  "Open/create the dedicated private config workspace"
  (dtm-workspace-switch-maybe "*config*"))

;;;###autoload
(defun dtm-org-roam-goto-workspace (&rest _)
  "Open/create the dedicated org-roam workspace"
  (dtm-workspace-switch-maybe "*roam*"))

;;;###autoload
(defun dtm/buffer-move-to-workspace (name &optional alist)
  "Move `current-buffer' to workspace with NAME and switch"
  (interactive (list
                (completing-read "Move current buffer to workspace:"
                                 (+workspace-list-names))))
  (let ((buffer (current-buffer))
        (persp (get-current-persp))
        (persp-autokill-buffer-on-remove nil))
    (unless (equal name (+workspace-current-name))
      (when (persp-contain-buffer-p buffer persp)
        (persp-remove-buffer buffer persp))
      (dtm-workspace-switch-maybe name))
    (display-buffer-same-window buffer alist)))

(defun dtm-display-buffer-in-workspace (buffer alist)
  "Display BUFFER in (workspace . name) defined in ALIST.
Intended for use in `display-buffer-alist'."
  (let ((name (cdr (assq 'workspace alist))))
    (let ((alist (remove (assq 'workspace alist) alist)))
      (dtm/buffer-move-to-workspace name alist))))

;;;###autoload
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

(defun dtm-ibuffer-workspace-filter-groups ()
  "Generate value for `ibuffer-filter-groups' based on perspectives."
  (mapcar #'(lambda (pn) (list pn (cons 'persp pn)))
          (nconc
           (cl-delete persp-nil-name (persp-names-current-frame-fast-ordered)
                      :test 'string=)
           (list persp-nil-name))))

;;;###autoload
(defun dtm-ibuffer-group-by-workspace-h ()
  "Set the current filter groups to filter by perspective.
Based on `ibuffer-projectile-set-filter-groups' from the ibuffer-projectile package:
https://github.com/purcell/ibuffer-projectile"
  (interactive)
  (setq ibuffer-filter-groups (dtm-ibuffer-workspace-filter-groups))
  (message "persp-ibuffer: grouping buffers by workspace")
  (let ((ibuf (get-buffer "*Ibuffer*")))
    (when ibuf
      (with-current-buffer ibuf
        (pop-to-buffer ibuf)
        (ibuffer-update nil t)))))

;;; Ediff
;;;###autoload
(defun dtm/ediff-this-file ()
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
(defun dtm/dired-ediff ()
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
(defun dtm-org-mode-setup-h ()
  "Personal org-mode customisation's after mode startup"
  (setq-local line-spacing dtm-org-line-spacing
              auto-hscroll-mode nil)
  (electric-quote-local-mode +1)
  (visual-line-mode -1)
  (auto-fill-mode +1)
  (add-hook! 'evil-insert-state-exit-hook
             :local #'dtm-insert-exit-fill-paragraph))

;;;###autoload
(defun dtm-insert-exit-fill-paragraph ()
  "Perform `org-fill-paragraph' unless el at point is a src block"
  ;; Check if `auto-fill-mode' is active
  (when auto-fill-function
    (unless (memq (org-element-type (org-element-at-point))
                  '(src-block comment-block))
      (org-fill-paragraph))))

(defun dtm-org-get-title-value ()
  "Returns the value of #+TITLE for the current document"
  (car (cdar (org-collect-keywords '("TITLE")))))

;;;###autoload
(defun dtm-org-download-method (link)
  "This is a helper function for org-download.
It creates the \"./Image\" folder within the same directory of the org file.
File is named as: org-file + timestamp + download-file-name

Based on drestivo's answer to this post:
https://github.com/abo-abo/org-download/issues/40.
Which was based off this commit message:
https://github.com/abo-abo/org-download/commit/137c3d2aa083283a3fc853f9ecbbc03039bf397b"
  (let* ((dir "Images/")
         (filename (file-name-nondirectory
                    (car (url-path-and-query
                          (url-generic-parse-url link)))))
         (filename-with-timestamp
          (format "%s%s--%s.%s"
                  (file-name-base (buffer-file-name))
                  (format-time-string org-download-timestamp)
                  (file-name-sans-extension filename)
                  (file-name-extension filename))))
    ;; Check if directory exists otherwise create it
    (unless (file-exists-p dir)
      (make-directory dir t))
    (message "Image: %s saved!" (expand-file-name filename-with-timestamp dir))
    (concat dir filename-with-timestamp)))

;;; Org-modern
;;;###autoload
(defun dtm-org--modern-indent-heading ()
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
(defun dtm-org-tree-slide-setup-h ()
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
(defun dtm-org-tree-slide-no-squiggles-a (orig-fn &optional ARG)
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
;;;###autoload
(defun dtm-org-pretty-use-appear-a ()
  "Activate `org-appear-mode' based on `org-pretty-entities'."
  (org-appear-mode (if org-pretty-entities +1 -1)))

;;; Org-roam
;;;###autoload
(defun dtm/org-roam-open-index ()
  "Opens the file specified in dtm-org-roam-index-file"
  (interactive)
  (dtm-org-roam-goto-workspace)
  (find-file (expand-file-name dtm-org-roam-index-file org-roam-directory)))

;;;###autoload
(defun dtm-org-element-at-point-get-content ()
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

;;;###autoload
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
(defun dtm-org-roam-dailes-calendar--file-to-absolute (file)
  "Convert file name (with gregorian date format) to absolute time"
  (calendar-absolute-from-gregorian (org-roam-dailies-calendar--file-to-date file)))

(defun dtm-org-roam-dailes-active-files ()
  "Return list of daily files corresponding to TODAY or later"
  (require 'org-roam-dailies)
  (let ((files (org-roam-dailies--list-files))
        (today (calendar-absolute-from-gregorian (calendar-current-date))))
    (while (and files
                (< (dtm-org-roam-dailes-calendar--file-to-absolute (car files))
                   today))
      (pop files))
    files))

;;;###autoload
(defun dtm-org-roam-dailies-sync-agenda (&rest _)
  "Scan the dailies-directory and add current and future dates to agenda."
  (mapc (lambda (x) (cl-pushnew x org-agenda-files :test #'string=))
        (dtm-org-roam-dailes-active-files)))

;;;###autoload
(defun dtm/org-roam-dailies-schedule-time ()
  "Wrapper around `org-schedule' that only prompts for time.
The DATE is derived from the #+title which must match the Org date format."
  (interactive)
  (unless (org-roam-dailies--daily-note-p)
    (user-error "Not in a daily-note"))
  (let ((date (dtm-org-get-title-value))
        (time (read-string "Schedule headline at (HH:MM): ")))
    (org-schedule nil (concat date " " time (when (length< time 3) ":00")))))

;;;###autoload
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

;;; Pdf-tools
;;;###autoload
(defun dtm/pdf-view-fit-half-height ()
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
(defun tiku91-vterm-redraw-cursor (args)
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
(defun dtm-ess-insert-string (mystr)
  "Insert string, undo if the same input event is issued twice"
  (let* ((event (event-basic-type last-input-event))
         (char (ignore-errors (format "%c" event))))
    (cond ((when char
             (or (ess-inside-string-or-comment-p)
                 (looking-back "[=|]" (1- (point)))))
           (insert char))
          ((re-search-backward mystr (- (point) (length mystr)) t)
           (if (and char (numberp event))
               (replace-match char t t)
             (replace-match "")))
          (t (insert mystr)))))

;;;###autoload
(defun dtm/ess-r-insert-assign (arg)
  "Rewrite of `ess-insert-assign' that respects white space, invoke twice to undo"
  (interactive "p")
  (dtm-ess-insert-string "<-"))

;;;###autoload
(defun dtm/ess-r-insert-pipe (arg)
  "Based on `ess-insert-assign', invoking the command twice reverts the insert"
  (interactive "p")
  (dtm-ess-insert-string "%>%"))

;;;###autoload
(defun dtm-ess-switch-maybe-a (orig-fn TOGGLE-EOB)
  "Only switch to the REPL if it was already visible"
  (let* ((starting-window (selected-window))
         (ess-process (and ess-current-process-name
                           (get-process ess-current-process-name)))
         (ess-buffer-visible (and ess-process
                                  (doom-visible-buffer-p
                                   (buffer-name (process-buffer ess-process))))))
    (funcall orig-fn TOGGLE-EOB)
    (evil-normal-state)
    (unless ess-buffer-visible
      (select-window starting-window))))

;;;###autoload
(defun dtm/ess-eval-symbol-at-point ()
  "Send the symbol under the cursor to the current ESS process"
  (interactive)
  (ess-send-string
   (get-process ess-current-process-name)
   (symbol-name (ess-symbol-at-point))
   t))

;;;###autoload
(defun dtm/ess-debug-command-step ()
  "Step into in debug mode.
Equivalent to 's' at the R prompt."
  (interactive)
  (ess-force-buffer-current)
  (unless (ess--dbg-is-active-p)
    (error "Debugger is not active"))
  (if (ess--dbg-is-recover-p)
      (ess-send-string (ess-get-process) "0")
    (ess-send-string (ess-get-process) "s")))

;;; Conda
(defun dtm--conda-env-promt-activate (env-name &optional silent)
  "If conda environment with ENV-NAME is not activated, prompt the user to do so.
Unless SILENT is t the user is notified when ENV-NAME is already active."
  (if (string= env-name conda-env-current-name)
      (unless silent (message "The %s environment is already active" env-name))
    (when (y-or-n-p (format "Activate conda env: %s?" env-name))
      (conda-env-activate (conda-env-name-to-dir env-name)))))

;;;###autoload
(defun dtm/conda-env-guess-prompt ()
  "Guess the currently relevant conda env and prompt user to activate it"
  (interactive)
  (dtm--conda-env-promt-activate (conda--infer-env-from-buffer)))

;;;###autoload
(defun dtm-conda-env-guess-prompt-h ()
  "Prompt the user to activate the relevant conda env if it is not \"base\"."
  (when (and (eq major-mode 'python-mode)
             (not (dtm-buffer-remote-p)))
    (let ((ienv (conda--infer-env-from-buffer)))
      (unless (string= ienv "base")
        (dtm--conda-env-promt-activate ienv)))))

;;; atomic-chrome
;;;###autoload
(defun dtm/atomic-chrome-toggle-server ()
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
(defun dtm/interaction-log-mode-w-buffer ()
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
;;;###autoload
(defun dtm/good-scroll-down-half ()
  (interactive)
  (good-scroll-move (/ (good-scroll--window-usable-height) 2)))

;;;###autoload
(defun dtm/good-scroll-up-half ()
  (interactive)
  (good-scroll-move (/ (good-scroll--window-usable-height) -2)))

;;;###autoload
(defun dtm-good-scroll-evil-override-h ()
  (if good-scroll-mode
      (progn
        (advice-add 'evil-scroll-down :override #'dtm/good-scroll-down-half)
        (advice-add 'evil-scroll-up :override #'dtm/good-scroll-up-half))
    (progn
      (advice-remove 'evil-scroll-down #'dtm/good-scroll-down-half)
      (advice-remove 'evil-scroll-up #'dtm/good-scroll-up-half))))

;;; Toggles
;;;###autoload
(defun dtm/toggle-trash-delete ()
  "Toggle between trashing and deleting files"
  (interactive)
  (if delete-by-moving-to-trash
      (progn
        (setq delete-by-moving-to-trash nil)
        (message "Now deleting files PERMANTLY"))
    (progn
      (setq delete-by-moving-to-trash t)
      (message "Now moving deleted files to trash"))))

(defvar dtm-left-margin 30
  "Size of left margin that can be added to selected-window on demand")

;;;###autoload
(defun dtm/window-toggle-left-margin ()
  "Toggle left margin on selected window."
  (interactive)
  (let ((window (selected-window)))
    (set-window-margins window (unless (car (window-margins window)) dtm-left-margin))))

;;; So-long-mode/csv-mode/tsv-mode
(defvar dtm-csv-mode-max-length 300
  "Maximum line length (bytes) for csv/tsv-mode to be enabled.")

;;;###autoload
(defun dtm-csv-mode-maybe-h ()
  "Activate csv/tsv-mode if max line is below `dtm-csv-mode-max-length'."
  (when-let* ((file (buffer-file-name))
              (ext (file-name-extension file)))
    (when (< (cadr (buffer-line-statistics)) dtm-csv-mode-max-length)
      (cond ((string= ext "csv")
             (csv-mode))
            ((string= ext "tsv")
             (tsv-mode))))))

;;; Lispy
;;;###autoload
(defun dtm/lispy-step-into (arg)
  "Step into the list at point, moving the point to after ARG atoms.
If REGION is active, call `lispy-delete' instead."
  (interactive "p")
  (cond ((region-active-p)
         (call-interactively 'lispy-delete))
        ((lispy-left-p)
         (lispy-dotimes arg
           (if (lispy-left-p)
               (progn
                 ;; Step into list
                 (forward-char 1)
                 ;; If not at the next list move to end of atom
                 (unless (or (lispy-left-p)
                             (lispy--in-empty-list-p
                              lispy-parens-preceding-syntax-alist))
                   (lispyville-forward-atom-end)))
             (lispyville-forward-atom-end))))
        ((lispy-right-p)
         (lispy-dotimes arg
           (if (lispy-right-p)
               (forward-char -1)
             (lispyville-backward-atom-end))))
        (t
         (error "Unexpected"))))

;;; Imenu
(defvar dtm-imenu-orginal-index-function nil
  "Original indexing function before calling `dtm-imenu-merge-index-h'")

;;;###autoload
(defun dtm-imenu-merge-index-h ()
  "Append results from `imenu-generic-expression' to the current imenu (add to major-mode hook).
This is useful when the index function does not utilise the generic expression such as in python-mode."
  (setq-local dtm-imenu-orginal-index-function imenu-create-index-function
              imenu-create-index-function 'dtm--imenu-merge-index))

(defun dtm--imenu-merge-index ()
  "See `dtm-imenu-merge-index-h'."
  (let ((original-index (funcall dtm-imenu-orginal-index-function))
        (generic-index (imenu--generic-function imenu-generic-expression)))
    (append generic-index original-index)))

;;; With lagging point functions
(defvar dtm-lagging-point-actual nil
  "Position of cursor when `dtm-with-lagging-point-a' would not have been active.")

;;;###autoload
(defun dtm-with-lagging-point-a (orig-fn)
  "Keep/lag the cursor position one command execution behind.
Indented to advise functions that move the point."
  (dtm/lagging-point-goto-actual)
  (save-excursion
    (funcall orig-fn)
    (sleep-for .05)
    (setq-local dtm-lagging-point-actual (point))))

;;;###autoload
(defun dtm/lagging-point-goto-actual ()
  "Restore cursor to the unlagged position."
  (interactive)
  (when dtm-lagging-point-actual
    (goto-char dtm-lagging-point-actual)
    (recenter nil)))

;;;###autoload
(defun dtm-lagging-point-reset ()
  "Reset `dtm-lagging-point-actual'."
  (setq-local dtm-lagging-point-actual nil))

;;; Flycheck
;;;###autoload
(defun dtm-flycheck-disable-proselint-rmd-h ()
  "Disable the 'proselint' flycheck checker when in R markdown.
Intended for `markdown-mode-hook'."
  (when (string-match-p "\\.Rmd$" buffer-file-name)
    (flycheck-disable-checker 'proselint)))

;;; Python
;;;###autoload
(defun dtm/elpy-send-current-and-step ()
  "If region is active call `elpy-shell-send-region-or-buffer' else call `elpy-shell-send-statement-and-step'."
  (interactive)
  (if (region-active-p)
      (progn
        (when (dtm-point-mark-same-line-p)
          (exchange-point-and-mark))
        (call-interactively #'elpy-shell-send-region-or-buffer)
        (deactivate-mark))
    (call-interactively #'elpy-shell-send-statement-and-step)))

;;;###autoload
(defun dtm/elpy-print-symbol-or-region ()
  "Prints the symbol at point or active region in the python REPL."
  (interactive)
  (let* ((symbol (if (region-active-p)
                     (dtm-region-as-string)
                   (python-info-current-symbol)))
         (command (concat "print(" symbol ")")))
    (message "Sent: %s" command)
    (elpy-shell--with-maybe-echo
     (python-shell-send-string command))))

;;; Doom popup module
;;;###autoload
(defun dtm/popup-raise ()
  "Wrapper for `+popup/raise' that will ensure a popup is selected."
  (interactive)
  (unless (+popup-window-p)
    (+popup/other))
  (call-interactively #'+popup/raise))

;;;###autoload
(defun dtm/popup-kill ()
  "Kill the currently open popup."
  (interactive)
  (unless (+popup-window-p)
    (+popup/other))
  (+popup--remember (list (selected-window)))
  (kill-buffer-and-window))

(defun dtm-popup-get-rule (buf)
  "Returns (predicate . action) for BUF in `display-buffer-alist'.
Based on `+popup/diagnose'."
  (let ((bname (dtm-get-buffer-name buf)))
    (cl-loop for (pred . action) in display-buffer-alist
             if (and (functionp pred) (funcall pred bname action))
             return (cons pred action)
             else if (and (stringp pred) (string-match-p pred bname))
             return (cons pred action))))

(defun dtm-popup-buffer-p (&optional buf)
  "Returns t if BUF has a non-nil `set-popup-rule!' in `display-buffer-alist'."
  (when-let* ((buf (or buf (current-buffer)))
              (rule (dtm-popup-get-rule buf)))
    (eq '+popup-buffer (caadr rule))))

;;;###autoload
(defun dtm/switch-popup-buffer ()
  "Prompt user to select buffer matching `dtm-popup-buffer-p'."
  (interactive)
  (dtm-read-display-buffer "Select popup" #'dtm-popup-buffer-p))

