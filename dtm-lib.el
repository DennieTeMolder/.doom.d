;;; dtm-lib.el --- Library of personal functions -*- lexical-binding: t; -*-

;;* Utility
(defun dtm-doctor-running-p ()
  "Returns t when the doom doctor CLI is running.
Required because doctor sets `noninteractive' to nil."
  (boundp 'doom-doctor--errors))

(defun dtm-doom-docs-p ()
  "Return non-nil if current buffer will trigger `doom-docs-org-mode'."
  (eq (alist-get 'mode (cdr (hack-dir-local--get-variables nil)))
      'doom-docs-org))

(defun dtm-member-previous (item lst &optional test key)
  "Return the sublist of LST where the KEY of `cdr' matches ITEM using TEST.
Defaults to comparing with `car' using `equal'."
  (or test (setq test #'equal))
  (or key (setq key #'car))
  (unless (listp lst)
    (signal 'wrong-type-argument (list 'listp lst)))
  (named-let loop-list ((curr lst)
                        (prev nil))
    (cond ((not curr) nil)
          ((funcall test item (funcall key curr)) prev)
          (t (loop-list (cdr curr) curr)))))

(defmacro dtm-to-front (item lst &optional test key)
  "Push ITEM to the front of LST, modifying it in place.
ITEM is matched by comparing to KEY using TEST, see `dtm-member-previous'."
  `(progn
     (when-let ((prev (dtm-member-previous ,item ,lst ,test ,key)))
       (push (cadr prev) ,lst)
       (setcdr prev (cddr prev)))
     ,lst))

(defun dtm-buffer-list-unsaved (&optional buffers)
  "Return all modified BUFFERS with an associated file. Defaults to `buffer-list'."
  (thread-last
    (or buffers (buffer-list))
    (cl-remove-if-not #'buffer-modified-p)
    (mapcar #'buffer-file-name)
    (remq nil)))

(defun dtm-file-local-readable-p (file)
  "Return non-nil if FILE is local and readable."
  (unless (file-remote-p file)
    (file-readable-p file)))

(defun dtm-ensure-dir (dir &optional default-dir parents)
  "Check if DIR exists in DEFAULT-DIR, else create it and optionally its PARENTS."
  (setq dir (expand-file-name (file-name-as-directory dir) default-dir))
  (unless (file-exists-p dir)
    (make-directory dir parents))
  dir)

(defun dtm-file-name-as-title (&optional fname)
  "Convert NAME to title by replacing _,... to space and capitalising.
If NAME is not provided `buffer-file-name' is used."
  (let ((name (file-name-base (or fname buffer-file-name))))
    (capitalize (replace-regexp-in-string "[^A-Za-z0-9]+" " " name))))

(defun dtm-point-mark-same-line-p ()
  "Returns t if point and mark are on the same line"
  (<= (line-beginning-position) (mark) (line-end-position)))

(defun dtm-deactivate-mark ()
  "Run `deactivate-mark' keeping point at the left side of region."
  (when (and (dtm-point-mark-same-line-p)
             (< (mark) (point)))
    (exchange-point-and-mark))
  (deactivate-mark))

(defun dtm-region-as-string (&optional deactivate)
  "Return the marked region as a string. If DEACTIVATE unmark the region."
  (when (use-region-p)
    (prog1 (buffer-substring-no-properties (mark) (point))
      (when deactivate (dtm-deactivate-mark)))))

(defun dtm-current-line-as-string ()
  "Return the current line as string."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun dtm-line-empty-p ()
  "Returns non-nil if the current line contains only whitespace."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun dtm-forward-line-non-empty ()
  "Move cursor to the start of the next non-empty line."
  (forward-line)
  (while (and (dtm-line-empty-p)
              (not (eobp)))
    (forward-line)))

(defun dtm-straight-prioritize (dir)
  "Move straight package DIR to the front of `load-path'."
  (let ((lib-dir (file-name-concat straight-base-dir "straight"
                                   straight-build-dir dir)))
    (when (file-exists-p lib-dir)
      (setq load-path (cons lib-dir (delete lib-dir load-path))))))

(defun dtm-ignore-user-error-a (orig-fun &rest args)
  "Calls ORIG-FUN with ARGS, return nil when an `user-error' is raised.
Intended as :around advice (e.g. for capf functions)."
  (condition-case err
      (apply orig-fun args)
    (user-error nil)))

;;* Buffer functions
(defun dtm-buffer-remote-p (&optional buf)
  "Returns t if BUF belongs to a remote directory."
  (or buf (setq buf (current-buffer)))
  (ignore-errors (file-remote-p (buffer-local-value 'default-directory buf))))

;;* Window functions
(defun dtm/split-window-optimally (&optional window min-width)
  "Split WINDOW based on width of the new window after `balance-windows'.
Split right if width stays above MIN-WIDTH, else split below.
Defaults to `selected-window' and half of `split-width-threshold' + 10.
See also: `split-window-sensibly'"
  (interactive)
  (or min-width (setq min-width (+ (/ split-width-threshold 2) 10)))
  (with-selected-window (or window (selected-window))
    (let ((parent-tree (car (window--subtree (or (window-parent) (selected-window)))))
          width n-window)
      (if (windowp parent-tree)
          (setq n-window 2
                width (window-total-width))
        (setq n-window (1+ (if (car parent-tree) 1 (length (cddr parent-tree))))
              width (- (nth 2 (cadr parent-tree)) (nth 0 (cadr parent-tree)))))
      (setq window (if (< (/ width n-window) min-width)
                       (split-window-below)
                     (split-window-right)))
      (balance-windows (window-parent))))
  (if (called-interactively-p 'any) (select-window window) window))

(defun dtm/buffer-move-to-window ()
  "Move the current buffer to the window of `aw-select'."
  (interactive)
  (require 'ace-window)
  (let ((this-command 'ace-select-window)
        (aw-dispatch-when-more-than 2)
        (aw-dispatch-always nil))
    (when (eq 1 (length (aw-window-list)))
      (save-selected-window (dtm/split-window-optimally)))
    (aw-select " Ace - Move Buffer" #'aw-move-window)))

(defun dtm-scroll-hide-eob (&optional window)
  "If lines past `point-max' are visible `recenter' to hide them."
  (with-selected-window (or window (selected-window))
    ;; If the first line is visible we cannot scroll any further
    (when (and (not (pos-visible-in-window-p 1))
               (pos-visible-in-window-p t))
      (save-excursion
        (goto-char (point-max))
        (recenter (- (1+ scroll-margin)))))))

(defun dtm-hide-eob-on-window-change ()
  "Call `dtm-scroll-hide-eob' if the windows size of `current-buffer' changes."
  (add-hook 'window-size-change-functions #'dtm-scroll-hide-eob 'append 'local))

(defun dtm/recenter-till-eob ()
  "Recenter point to middle of screen but do not scroll past EOB"
  (interactive)
  (recenter)
  (dtm-scroll-hide-eob))

;;* Theme recommendations
(defun dtm-theme-which-inactive (theme1 theme2)
  "Return THEME1 of not currently active, else return THEME2"
  (if (eq theme1 (car custom-enabled-themes)) theme2 theme1))

(defun dtm-recommend-theme ()
  "Recommend a NEW theme to use based on context and time of day."
 (if (bound-and-true-p org-tree-slide-mode)
     dtm-presentation-theme
   (let ((hour (string-to-number (substring (current-time-string) 11 13))))
     (if (member hour (number-sequence dtm-first-hour-of-day dtm-last-hour-of-day))
         (dtm-theme-which-inactive dtm-light-theme dtm-alternative-light-theme)
       (dtm-theme-which-inactive dtm-dark-theme dtm-alternative-dark-theme)))))

;;* UI
(defun dtm-doom-check-fonts ()
  "Check if doom fonts are installed, otherwise prevent a blank display."
  (when (display-graphic-p)
   (dolist (spec (list doom-font doom-serif-font doom-variable-pitch-font))
     (when (and spec (not (find-font spec)))
       (warn "Font \"%s\" not found!" (font-get spec :family))
       (font-put spec :family nil)))))

(defun dtm-doom-modeline-conditional-encoding-h ()
  "Only display encoding in modeline when it's not UTF-8.
Use for `after-change-major-mode-hook'."
  (setq-local doom-modeline-buffer-encoding
              (not (memq (coding-system-get buffer-file-coding-system :category)
                         '(coding-category-undecided coding-category-utf-8)))))

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

(defun dtm-y-or-n-p-trash-a (orig-fun &rest args)
  "Replace delete by trashed in `y-or-n-p' prompts within ORIG-FUN.
Respects `delete-by-moving-to-trash'. Intended as :around advice."
  (letf! ((defun y-or-n-p (prompt)
            (when delete-by-moving-to-trash
              (setq prompt (string-replace "delete" "trash" prompt)))
            (funcall y-or-n-p prompt)))
    (apply orig-fun args)))

;;* Workspaces/perspectives
(defun dtm-workspace-switch-maybe (name)
  "Switch to workspace NAME if not already current"
  (unless (equal name (+workspace-current-name))
    ;; Recycle current workspace if empty
    (if (or (+workspace-exists-p name)
            (+workspace-buffer-list))
        (+workspace-switch name t)
      (+workspace-rename (+workspace-current-name) name))
    (+workspace/display)))

(defun dtm-doom-private-goto-workspace ()
  "Open/create the dedicated private config workspace"
  (dtm-workspace-switch-maybe "*config*"))

(defun dtm-citar-goto-workspace (&rest _)
  "Open/create the dedicated citar bibliography workspace"
  (dtm-workspace-switch-maybe "*bib*"))

(defun dtm/buffer-move-to-workspace (name &optional alist)
  "Move `current-buffer' to workspace with NAME and switch"
  (interactive (list
                (completing-read "Move current buffer to workspace: "
                                 (+workspace-list-names))))
  (let ((buffer (current-buffer))
        (persp (get-current-persp))
        (persp-autokill-buffer-on-remove nil))
    (unless (equal name (+workspace-current-name))
      (when (persp-contain-buffer-p buffer persp)
        (persp-remove-buffer buffer persp))
      (dtm-workspace-switch-maybe name))
    (display-buffer-same-window buffer alist)))

(defun dtm-get-buffer (b-or-n)
  "Wrapper for `get-buffer', that handles `read-buffer' cons cells."
  (let ((buf (cond ((listp b-or-n) (cdr b-or-n))
                   ((stringp b-or-n) (get-buffer b-or-n))
                   (t b-or-n))))
    (unless (bufferp buf)
      (error "No buffer found for: %s" b-or-n))
    buf))

(defun dtm-buffer-orphan-p (&optional buf)
  "Return t if buffer BUF does not belong to any workspace/perspective."
  (let ((buf (or buf (current-buffer))))
    (not (persp--buffer-in-persps (dtm-get-buffer buf)))))

(defun dtm/switch-orphan-buffer (buf)
  "Prompt user to select buffer matching `dtm-buffer-orphan-p'."
  (interactive
   (list (read-buffer "Select orphan-buffer: " nil t #'dtm-buffer-orphan-p)))
  (pop-to-buffer-same-window buf))

;;* Projectile
(defun dtm-project-ignored-p (project-root)
  "Return non-nil if remote or temporary file or a straight package."
  (or (file-remote-p project-root)
      (file-in-directory-p project-root temporary-file-directory)
      (file-in-directory-p project-root doom-local-dir)))

;;* Doom Popup
(defun dtm-popup-ensure ()
  "Ensure a popup is selected."
  (or (+popup-window-p)
      (+popup/other)
      (user-error "No popups are open"))
  (current-buffer))

(defun dtm/popup-raise ()
  "Wrapper for `+popup/raise' that will ensure a popup is selected."
  (interactive)
  (dtm-popup-ensure)
  (call-interactively #'+popup/raise))

(defun dtm/popup-kill ()
  "Kill the currently open popup."
  (interactive)
  (dtm-popup-ensure)
  (+popup--remember (list (selected-window)))
  (kill-buffer-and-window))

(defun dtm-get-buffer-name (b-or-n)
  "Wrapper for `buffer-name', that handles `read-buffer' cons cells."
  (let ((bname (cond ((listp b-or-n) (car b-or-n))
                     ((bufferp b-or-n) (buffer-name b-or-n))
                     (t b-or-n))))
    (unless (stringp bname)
      (error "Could not find buffer string for: %s" b-or-n))
    bname))

(defun dtm/popup-only ()
  "Close all other popup except the currently selected one."
  (interactive)
  (let ((buf (dtm-popup-ensure)))
    (+popup/close-all)
    (display-buffer buf)))

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
  (or buf (setq buf (current-buffer)))
  (when-let ((rule (dtm-popup-get-rule buf)))
    (eq '+popup-buffer (caadr rule))))

(defun dtm/switch-popup-buffer (buf)
  "Prompt user to select buffer matching `dtm-popup-buffer-p'."
  (interactive
   (list (read-buffer "Select popup: " nil t #'dtm-popup-buffer-p)))
  (display-buffer buf))

;;* Imenu
(defun dtm-elisp-extend-imenu-h ()
  "Add `modulep!' and `+emacs-lisp-outline-regexp' support to `imenu'."
  (unless (cl-find "Module" imenu-generic-expression :test #'string= :key #'car)
    (push '("Module" "^\\s-*(when (modulep! +\\([^)]+\\))" 1)
          imenu-generic-expression))
  (setq imenu-generic-expression
        (append (cl-remove "Section" imenu-generic-expression :test #'string= :key #'car)
                `(("Section" ,(concat +emacs-lisp-outline-regexp "[ \t]*\\([^\n]+\\)") 1)))))

(defvar dtm-imenu-orginal-index-function nil
  "Original indexing function before calling `dtm-imenu-merge-index-h'")

(defun dtm-imenu-merge-index-h ()
  "Append results from `imenu-generic-expression' to the current imenu.
This is useful when the `imenu-create-index-function' does not
utilise the generic expression such as in `python-mode'.

Intended for appending to major-mode hooks."
  (setq-local dtm-imenu-orginal-index-function imenu-create-index-function
              imenu-create-index-function 'dtm-imenu-merge-index))

(defun dtm-imenu-merge-index ()
  "See `dtm-imenu-merge-index-h'."
  (let ((original-index (funcall dtm-imenu-orginal-index-function))
        (generic-index (imenu--generic-function imenu-generic-expression)))
    (append generic-index original-index)))

;;* Ibuffer
(defun dtm/ibuffer-orphans ()
  "Open an ibuffer window with all orphan buffers."
  (interactive)
  (let ((ibuffer-never-show-predicates '(persp--buffer-in-persps))
        (ibuffer-hook nil))
    (ibuffer)))

(defun dtm-ibuffer-popup-p (&optional buf)
  "Run `dtm-popup-buffer-p' but always return nil for `messages-buffer-name'."
  (or buf (setq buf (current-buffer)))
  (unless (string= (buffer-name buf) messages-buffer-name)
    (dtm-popup-buffer-p buf)))

(defun dtm-ibuffer-persp-filter-groups ()
  "Generate value for `ibuffer-filter-groups' based on perspectives.
Requires \"persp\" to be defined via `define-ibuffer-filter'.
Ref: `ibuffer-mode' section on 'Filter Groups'"
  (let ((persps (mapcar #'(lambda (pn) (list pn (cons 'persp pn)))
                        (cl-delete persp-nil-name
                                   (persp-names-current-frame-fast-ordered)
                                   :test 'string=))))
    `(("proc" (process))
      ,@persps
      ("popups" (predicate dtm-ibuffer-popup-p))
      (,persp-nil-name (persp . ,persp-nil-name)))))

(defun dtm-ibuffer-group-by-persp-h ()
  "Set the current filter groups to filter by perspective.
Based on `ibuffer-projectile-set-filter-groups' from ibuffer-projectile:
https://github.com/purcell/ibuffer-projectile"
  (interactive)
  (setq ibuffer-filter-groups (dtm-ibuffer-persp-filter-groups))
  (message "persp-ibuffer: grouping buffers by workspace")
  (let ((ibuf (get-buffer "*Ibuffer*")))
    (when ibuf
      (with-current-buffer ibuf
        (pop-to-buffer ibuf)
        (ibuffer-update nil t)))))

;;* Dired/Dirvish
(defun dtm/dired-delete-marked ()
  "Delete marked or current file(s), with C-u toggle `delete-by-moving-to-trash'."
  (interactive)
  (let ((delete-by-moving-to-trash (if current-prefix-arg
                                       (not delete-by-moving-to-trash)
                                     delete-by-moving-to-trash)))
    (dired-do-delete)))

(defun dtm/dired-ediff ()
  "Compare file under cursor to file selected in prompt using Ediff"
  (interactive)
  (let* ((file (dired-get-filename t))
         (dir (dired-current-directory))
         (default nil)
         (target (read-file-name (format-prompt "Diff %s with" default file)
                                 default nil t)))
    (ediff (expand-file-name file dir) target)))

(defun dtm-dirvish-preview-window-p (&optional window)
  "Returns t if WINDOW is a dirvish preview window, defaults to `selected-window'."
  (when (fboundp 'dirvish-curr)
    (or window (setq window (selected-window)))
    (cl-some (lambda (win)
               (when-let (dv (with-current-buffer (window-buffer win)
                               (dirvish-curr)))
                 (eq window (dv-preview-window dv))))
             (window-list))))

(defun dtm/dirvish-side ()
  "Wrapper for `dirvish-side' that always closes the window if visible."
  (interactive)
  (if-let (window (and (fboundp 'dirvish-side--session-visible-p)
                       (dirvish-side--session-visible-p)))
      (progn (select-window window) (dirvish-quit))
    (call-interactively #'dirvish-side)))

(defun dtm/dirvish-copy-file-name ()
  "Copy file name, or path with C-u. Also works for multiple marked files."
  (interactive)
  (call-interactively
   (if current-prefix-arg
       #'dirvish-copy-file-path
     #'dirvish-copy-file-name)))

(defun dtm/dirvish-find-file ()
  "Like `find-file' but for use in `dirvish' buffers."
  (interactive)
  (dirvish--find-entry
   'find-file
   (read-file-name "Open: " nil default-directory
                   (confirm-nonexistent-file-or-buffer))))

(defun dtm/dirvish-search-cwd ()
  "Grep files from current dirvish directory, kill dirvish on confirm."
  (interactive)
  (let ((dv (or (dirvish-curr) (user-error "Not a dirvish buffer")))
        (path default-directory))
    (let ((dirvish-reuse-session 'resume))
      (dirvish-quit)
      (let ((default-directory path)
            (inhibit-quit t))
        (or (with-local-quit
              (+default/search-cwd) t)
            (dirvish))))
    (unless (memq dirvish-reuse-session `(t resume open))
      (dirvish--clear-session dv))))

(defun dtm/dirvish-narrow ()
  "Run `dirvish-narrow' and provide revert instruction after finish."
  (interactive)
  (call-interactively #'dirvish-narrow)
  (message "Run `revert-buffer' (%s) to un-narrow"
           (substitute-command-keys "\\[revert-buffer]")))

(defun dtm-dirvish-sort-history (hist)
  "Preserve sorting of HIST removing duplicates and the `default-directory'.
Intended for use as `vertico-sort-function' via `vertico-multiform-commands'."
  (let ((res))
    (dolist (x hist)
      (unless (string= x default-directory)
          (cl-pushnew x res :test #'string=)))
    (reverse res)))

;;* Vundo
(defun dtm-vundo-pre-enter-h ()
  "Ensure cursor remains visible in the edited buffer."
  (setq-local cursor-in-non-selected-windows t))

(defun dtm-vundo-post-exit-h ()
  "Revert `dtm-vundo-pre-enter-h'."
  (kill-local-variable 'cursor-in-non-selected-windows))

(defun dtm-vundo-diff-window ()
  "Return the diff window belonging to the current Vundo buffer."
  (get-buffer-window
   (concat "*vundo-diff-" (buffer-name vundo--orig-buffer) "*")))

(defun dtm-vundo-diff-window-resize (&optional window)
  "Resize the diff window to only show the relevant information."
  (unless (or window (setq window (dtm-vundo-diff-window)))
    (error "No vundo-diff window visible"))
  (let ((max-size 0.4))
   (with-selected-window window
     (unless (eq major-mode 'vundo-diff-mode)
       (error "Not a vundo-diff window: %s" window))
     (switch-to-buffer (window-buffer window))
     (goto-char (point-min))
     (forward-line 4)
     (recenter 0)
     (let ((delta (- (count-lines (point) (point-max)) 2 (window-height)))
           (delta-max (- (floor (* max-size (frame-height))) (window-height))))
       (window-resize window (min delta delta-max))))))

(defun dtm-vundo-live-diff-post-command ()
  "Post command hook function for live diffing."
  (unless (memq this-command '(vundo-quit vundo-confirm))
    (vundo-diff)
    (dtm-vundo-diff-window-resize)))

(define-minor-mode dtm-vundo-live-diff-mode
  "Live diff when moving in vundo buffer.
Ref: https://github.com/casouri/vundo/issues/112"
  :lighter ""
  (require 'vundo-diff)
  (if dtm-vundo-live-diff-mode
      (add-hook 'post-command-hook #'dtm-vundo-live-diff-post-command 0 'local)
    (remove-hook 'post-command-hook #'dtm-vundo-live-diff-post-command 'local)
    (with-current-buffer vundo--orig-buffer
      (vundo-diff--quit))))

;;* CSV/TSV-mode
(defvar dtm-csv-mode-max-length 300
  "Maximum characters per line for csv/tsv-mode to be enabled.")

(defun dtm-csv-mode-maybe-h ()
  "Activate csv/tsv-mode if max line is below `dtm-csv-mode-max-length'."
  (when-let ((file (buffer-file-name)))
    (when (< (cadr (buffer-line-statistics)) dtm-csv-mode-max-length)
      (pcase (file-name-extension file)
        ("csv" (csv-mode))
        ("tsv" (tsv-mode))))))

;;* Image-mode
(defun dtm-image-overlay ()
  "Return current image overlay, create if not exists."
  (or (image-mode-window-get 'overlay)
      (alist-get 'overlay (image-mode-window-put
                           'overlay (make-overlay (point-min) (point-max))))))

(defun dtm/image-center (&optional window)
  "Centre the current image in the window.
Can also be used as :after advice for `image-fit-to-window'.
Ref: `pdf-view-display-image'"
  (interactive)
  (with-selected-window (or window (selected-window))
    (unless (derived-mode-p 'image-mode 'pdf-view-mode)
      (error "Not in 'image-mode'!"))
    (let ((img-width (floor (car (image-size (image-get-display-property))))))
      (overlay-put (dtm-image-overlay) 'before-string
                   (when (> (window-width) img-width)
                     (propertize " " 'display
                                 `(space :align-to ,(/ (- (window-width)
                                                          img-width)
                                                       2))))))))

;;* Elisp-refs
(defun dtm-elisp-refs--find-file-a (button)
  "Open the file referenced by BUTTON in the other window.
Intended as :override advice for `elisp-refs--find-file'."
  (find-file-other-window (button-get button 'path))
  (goto-char (point-min)))

;;* Lispy
(defun dtm/lispy-mark-car ()
  "Wrap `lispy-mark-car' to also work on sharp-quoted symbols."
  (interactive)
  (letf! ((defun looking-at (regexp)
            (funcall looking-at (string-replace "'" "#'" regexp))))
    (lispy-mark-car)))

(defun dtm/lispy-step-into (arg)
  "Step into the list at point, moving the point to after ARG atoms.
If REGION is active, call `dtm/lispy-mark-car' instead."
  (interactive "p")
  (cond ((region-active-p)
         (dtm/lispy-mark-car))
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
        (t (user-error "Unexpected"))))

(defun dtm/lispy-yank-list ()
  "Copy region or result of `lispy-mark-list' to kill-ring."
  (interactive)
  (unless (region-active-p)
    (lispy-mark-list 1)
    (exchange-point-and-mark)
    (evil-goggles--show-async-hint (region-beginning) (region-end)))
  (lispy-new-copy)
  (deactivate-mark))

(defun dtm/lispy-wrap-round (arg)
  "Surround the region of `lispy-mark-list' with parenthesis."
  (interactive "P")
  (let ((lispy-insert-space-after-wrap))
    (lispy-wrap-round arg)))

(defun dtm/lispy-delete-list ()
  "Call `lispy-delete-backward' on the current S-expression."
  (interactive)
  (if (region-active-p)
      (lispy-kill-at-point)
    (let ((right-p (cond ((lispy-right-p) t)
                        ((lispy-left-p) nil)
                        (t (user-error "Unexpected")))))
      (lispy-kill-at-point)
      (when (lispy-bolp) (lispy-delete-backward 1))
      (unless (and (lispy-right-p)
                   (or right-p
                       (save-excursion
                         (forward-char)
                         (lispy-right-p))))
        (lispy-forward 1))
      (unless right-p (backward-list)))))

(defun dtm/lispy-change-symbol (arg)
  "Call or `lispy-ace-symbol-replace' or delete region."
  (interactive "p")
  (if (region-active-p)
      (lispy-kill-at-point)
    (lispy-ace-symbol-replace arg)))

(defun dtm/lispy-eval-and-insert (arg)
  "ARG = 1 `lispy-eval-and-comment', ARG = 0/4 `lispy-eval-and-replace'.
Else call `lispy-eval-and-insert'."
  (interactive "p")
  (cond ((eq arg 1) (lispy-eval-and-comment))
        ((memq arg '(0 4)) (lispy-eval-and-replace))
        (t (lispy-eval-and-insert))))

(defun dtm/lispy-paste-before ()
  "Version of `lispy-paste' that pastes before the cursor."
  (interactive)
  (left-char 1)
  (insert " ")
  (call-interactively #'lispy-paste)
  (right-char 1)
  (lispy--normalize-1))

;;* Lispyville
(defmacro dtm-lispyville-smart-remap (evil-fn lispy-fn)
  "Remap EVIL-FN to LISPY-FN unless `lispy--in-string-or-comment-p' is non-nil.
Ref: https://github.com/noctuid/lispyville/issues/284"
  `(define-key lispyville-mode-map
     [remap ,evil-fn]
     (general-predicate-dispatch ,lispy-fn
       (lispy--in-string-or-comment-p) #',evil-fn)))

(defun dtm-lispyville-toggle-mark-type-a (oldfun &rest args)
  "Around advice for `lispyville-toggle-mark-type' that preserves cursor position."
  (let ((evil-move-cursor-back nil)
        (evil-move-beyond-eol t))
    (apply oldfun args)))

;;* Vterm
(defun dtm-vterm-redraw-cursor-a (orig-fn &rest args)
  "Prevent vterm from modifying `cursor-type'.
Intended as around advice for `vterm--redraw'
Ref: https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1191400836"
  (let ((cursor-type cursor-type)) (apply orig-fn args)))

(defun dtm-vterm-sync-cursor-a (&rest _)
  "Keep vterm cursor position consistent with evil.
Intended as before advice for `vterm-send-key'"
  (vterm-goto-char (point)))

(defun dtm/vterm-send-current-region-or-line (&optional no-step)
  "Execute the current line in the vterm buffer.
Moves the point to the next non-empty line unless NO-STEP is non-nil."
  (interactive "P")
  (let ((command (dtm-region-as-string 'deactivate)))
    (if command
        (setq command (string-trim command "[\\n\\r]+"))
      (when (dtm-line-empty-p) (dtm-forward-line-non-empty))
      (setq command (dtm-current-line-as-string))
      (+nav-flash-blink-cursor)
      (unless no-step (dtm-forward-line-non-empty)))
    (save-selected-window
      (vterm-other-window)
      (vterm--goto-line -1)
      (vterm-send-string command)
      (vterm-send-return))))

(defun dtm/vterm-cape-dabbrev ()
  "Vterm compatible `cape-dabbrev' completion using `completing-read'.
`completion-at-point' fails for read-only buffers because preview is blocked.
Ref: https://github.com/akermu/emacs-libvterm/pull/401"
  (interactive)
  (let ((completion (dtm-cape-collect (cape-dabbrev))))
    (unless completion
      (user-error "cape-dabbrev: No completions"))
    (setq completion (completing-read "Dabbrev:" completion))
    (when (thing-at-point 'symbol)
      (vterm-send-C-w))
    (vterm-send-string completion t)))

;;* Ispell-fu
(defun dtm-spell-fu-set-treesit-faces-h ()
  "Add tree-sitter font's to `spell-fu-faces-include'."
  (setq spell-fu-faces-include
        '(tree-sitter-hl-face:comment
          tree-sitter-hl-face:string
          tree-sitter-hl-face:doc)))

(defun dtm-spell-fu-set-conf-faces-h ()
  "Add conf-mode font's to `spell-fu-faces-include'."
  (setq spell-fu-faces-include
        '(font-lock-comment-face
          font-lock-string-face
          font-lock-doc-face)))

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
    (+spell/correct)))

(defun dtm/spell-correct-previous ()
  "Correct the first spelling error before word at point."
  (interactive)
  (save-excursion
    (+spell/previous-error)
    (let ((bounds (dtm-spell-fu-bounds-word-at-point))
          (evil-goggles--commands `((,this-command :face isearch-fail)))
          (evil-goggles-async-duration 10.0))
      (evil-goggles--show-async-hint (car bounds) (cdr bounds)))
    (dtm/spell-correct)
    (evil-goggles--vanish)))

;;* Corfu/Cape
(defun dtm/corfu-complete-always ()
  (interactive)
  (when (< corfu--index 0)
    (corfu-next))
  (corfu-complete))

(defun dtm-cape-keyword-dict ()
  "Return results from `cape-keyword' and `cape-dict' combined."
  (cape-wrap-super #'cape-keyword #'cape-dict))

(defun dtm/cape-keyword-dict ()
  "Interactive version of `dtm-cape-keyword-dict'."
  (interactive)
  (cape-interactive #'dtm-cape-keyword-dict))

(defun dtm-cape-collect (capf-table)
  "Collect all completions from the output of a capf as a list of strings."
  (all-completions "" (nth 2 capf-table)))

;;* Markdown
(defun dtm-flycheck-disable-proselint-rmd-h ()
  "Disable the \"proselint\" flycheck checker when in R markdown.
Intended for `markdown-mode-hook'."
  (when-let ((fname (buffer-file-name)))
    (when (string-match-p "\\.Rmd$" fname)
      (flycheck-disable-checker 'proselint))))

(defun dtm/markdown-backward-same-level ()
  "Move to previous list item or first heading above current line."
  (interactive)
  (if (or (markdown-on-heading-p) (markdown-cur-list-item-bounds))
      (markdown-outline-previous-same-level)
    (markdown-back-to-heading-over-code-block)))

(defun dtm/markdown-up ()
  "Move up in list or heading hierarchy. Ref: `markdown-outline-up'."
  (interactive)
  (unless (markdown-up-list)
    (if (markdown-on-heading-p)
        (markdown-up-heading 1)
      (markdown-back-to-heading-over-code-block))))

;;* Org-mode
(defun dtm-org-limit-styling-p ()
  "Return non-nil if limited styling should be applied."
  (or (doom-temp-buffer-p (current-buffer))
      (dtm-doom-docs-p)
      (dtm-dirvish-preview-window-p)))

(defun dtm-org-fill-paragraph ()
  "Perform `org-fill-paragraph' unless el at point is a comment or source block."
  ;; Check if `auto-fill-mode' is active
  (when auto-fill-function
    (unless (memq (org-element-type (org-element-at-point-no-context))
                  '(src-block comment))
      (org-fill-paragraph))))

(defun dtm-org-mode-setup-h ()
  "Personal org-mode customisation's after mode startup"
  (unless (dtm-org-limit-styling-p)
    (setq-local line-spacing dtm-org-line-spacing)
    ;; NOTE org-hide-emphasis-markers used by +org-pretty-mode is very expensive
    (setq-local org-pretty-entities t)
    (+word-wrap-mode +1)
    (+zen-light-toggle +1)
    (add-hook! 'evil-insert-state-exit-hook :local #'dtm-org-fill-paragraph)
    (dtm-org-link-as-png-check)))

(defun dtm-org-fold-font-lock-remove ()
  "Remove `org-activate-folds' `font-lock-keyword-face' on line ends.
Intended for `org-font-lock-hook'."
  (font-lock-remove-keywords nil '(org-activate-folds)))

(defun dtm-org-get-title-value ()
  "Returns the value of #+TITLE for the current document"
  (cadar (org-collect-keywords '("TITLE"))))

(defun dtm/org-pretty-mode-toggle ()
  "Toggle `+org-pretty-mode' keeping `org-pretty-entities' in-sync.
Also syncs `org-appear-mode' and `org-pretty-entities-include-sub-superscripts'."
  (interactive)
  (make-local-variable 'org-hide-emphasis-markers)
  (let ((enable (not +org-pretty-mode)))
    (when (and org-pretty-entities enable)
      (setq-local org-pretty-entities nil))
    (setq-local org-pretty-entities-include-sub-superscripts enable)
    (+org-pretty-mode 'toggle)
    (org-appear-mode enable)))

(defun dtm/org-clock-in-after (&optional select)
  "Similar to C-u C-u `org-clock-in-last', but with ability to SELECT last clock."
  (interactive "@P")
  (org-clock-in
   nil
   (save-excursion
     (if select
         (let ((m (org-clock-select-task "Clock in after:")))
           (pop-to-buffer-same-window (marker-buffer m))
           (goto-char m))
       (org-clock-goto))
     (org-clock-get-last-clock-out-time))))

;;** Org-export (ox)
(defvar dtm-org-export-source-file nil
  "Current source file. Set by `org-export-before-processing-functions'.
Not guaranteed to be correct during async export with multiple processes.")

(defun dtm-org-export-remember-source-file-h (backend)
  "Set `dtm-org-export-source-file' to `buffer-file-name'.
Intended for `org-export-before-processing-functions'."
  (ignore-errors backend)
  (setq dtm-org-export-source-file (buffer-file-name)))

(defun dtm-reftex-TeX-master-file-a (orig-fun &rest args)
  "Let bind `dtm-org-export-source-file' to `buffer-file-name' if nil.
RefTeX assumes `buffer-file-name' to be non-nil, which not always true.
Intended as :around advice for `reftex-TeX-master-file'."
  (let ((buffer-file-name (or buffer-file-name dtm-org-export-source-file)))
    (apply orig-fun args)))

;;** Org-link
(defvar dtm-org-link-as-png-dir "Media/"
  "Directory for storing converted png files.")

(defvar dtm-org-link-convert-executable
  ;; Avoid using the MS Windows command convert.exe .
  (unless (memq system-type '(ms-dos windows-nt))
    'executable-find)
  "Absolute path to the ImageMagick/convert program or `executable-find'.")

(defvar-local dtm-org-link-as-png-disable nil
  "Non-nil if org-link \"as_png:\" conversion should be disabled.
This variable takes precedence over `dtm-org-link-as-png-disabled-dispatchers'.")

(defvar dtm-org-link-as-png-disabled-dispatchers '(latex beamer)
  "List of `org-export-backends' for which \"as_png:\" conversion is disabled.")

(defun dtm-org-link-as-png-disabled-p ()
  "Returns non-nil if \"as_png:\" conversion is disabled in current context."
  (or dtm-org-link-as-png-disable
      (when (bound-and-true-p org-export-current-backend)
        (memq org-export-current-backend dtm-org-link-as-png-disabled-dispatchers))))

(defun dtm-org-link-as-png (tag)
  "Convert TAG to .png to the format generated by `dtm/org-link-as-png-convert'.
If TAG starts with an org link abbreviation (as defined by \"#+LINK:\") it is
expanded and the result is placed in ./Media/$LINKWORD/$TAG.

Intended for use in `org-link-abbrev-alist'."
  (if (dtm-org-link-as-png-disabled-p)
      (org-link-expand-abbrev tag)
    (file-name-concat
     (unless (string-match-p "^[~/.]" dtm-org-link-as-png-dir) ".")
     dtm-org-link-as-png-dir
     (thread-first
       (if-let ((split-at (string-search ":" tag)))
           (file-name-concat (substring tag nil split-at)
                             (substring tag (1+ split-at) nil))
         tag)
       (file-name-sans-extension)
       (concat ".png")))))

(defun dtm-org-link-string (link)
  "Return the raw string for org-element LINK without the description."
  (replace-regexp-in-string (rx (or "[[" (seq "]" (* nonl)))) ""
                            (buffer-substring-no-properties
                             (org-element-property :begin link)
                             (org-element-property :end link))))

(defun dtm-org-link-as-png-parse (link)
  "If LINK as an \"as_png:\" link, return file conversion alist (infile . outfile).
Else return nil"
  (let ((s (dtm-org-link-string link)))
    (when (string-prefix-p "as_png:" s)
      (thread-first
        (string-remove-prefix "as_png:" s)
        (org-link-expand-abbrev)
        (cons (org-element-property :path link))))))

(defun dtm-org-link-as-png-parse-all ()
  "Return a list of all \"as_png:\" file conversion in the current org buffer.
The elements of the list take the shape (infile . outfile)."
  (cl-remove-duplicates
   (remq nil (org-element-map (org-element-parse-buffer) 'link
               #'dtm-org-link-as-png-parse))
   :test #'string= :key #'car))

(defun dtm-org-link-as-png-conversions ()
  "Returns a categorised list of all \"as_png:\" conversion in the current org buf.
The zeroth element is total number of conversions
The first element is a list of conversions with missing output files
The second element is a list of conversions with outdated output files
The third element is a list of conversions with missing input files
These lists are similar to the return value of `dtm-org-link-as-png-parse-all'."
  (unless (dtm-org-link-as-png-disabled-p)
    (let ((conversions (dtm-org-link-as-png-parse-all))
          n-total missing outdated broken)
      (when conversions
        (setq n-total (length conversions))
        (dolist (conv conversions)
          (cond ((not (file-readable-p (cdr conv)))
                 (if (file-readable-p (car conv))
                     (push conv missing)
                   (push conv broken)))
                ((and (file-readable-p (car conv))
                      (file-newer-than-file-p (car conv) (cdr conv)))
                 (push conv outdated))))
        (list n-total missing outdated broken)))))

(defun dtm-org-link-as-png-check ()
  "Check the status of \"as_png:\" conversions in the current org buffer.
The result is printed to the echo-area."
  (when-let (conversions (dtm-org-link-as-png-conversions))
    (message "Org[as_png]: %s missing; %s outdated; %s broken (%s total)"
             (length (nth 1 conversions))
             (length (nth 2 conversions))
             (length (nth 3 conversions))
             (car conversions))))

(defun dtm/org-link-as-png-convert (&rest _)
  "Prompt the user to handle \"as_png:\" conversions for the current buffer."
  (interactive)
  (let ((conversions (dtm-org-link-as-png-conversions)))
    (if (not conversions)
        (message "Org[as_png]: No conversions detected")
      (let ((n-total (car conversions))
            (missing (nth 1 conversions))
            (outdated (nth 2 conversions))
            (n-broken (length (nth 3 conversions)))
            response)
        (if (not (or missing outdated))
            (message "Org[as_png]: All %s possible conversions are up to date (%s broken)"
                     (- n-total n-broken) n-broken)
          (setq response
                (thread-first
                  "Org[as_png]: %s missing, %d outdated (%d total); Update [a]ll/[m]issing/[n]one? "
                  (format-prompt nil (length missing) (length outdated) n-total)
                  (read-char-choice '(?a ?m ?n))))
          (if (not (memq response '(?a ?m)))
              (progn (message "Org[as_png]: Conversion cancelled by user") nil)
            (prog1 (dtm-org-link-as-png--convert
                    (append missing (when (eq ?a response) outdated)))
              ;; FIXME doesn't work when triggered by `org-export-before-parsing-functions'
              (font-lock-update))))))))

(defun dtm-org-link-as-png--convert (conversions)
  "Convert CONVERSIONS to PNG format.
CONVERSIONS should have the structure of `dtm-org-link-as-png-parse-all'."
  (when (eq 'executable-find dtm-org-link-convert-executable)
    (setq dtm-org-link-convert-executable (executable-find "convert")))
  (if (not (and dtm-org-link-convert-executable
                (file-executable-p dtm-org-link-convert-executable)))
      (progn (warn "Org[as_png]: `dtm-org-link-convert-executable' unset or non-executable!") nil)
    (let (warn-msg)
      (dolist (conv conversions)
        (let* ((infile (car conv))
               (outfile (cdr conv))
               (msg (format "Org[as_png]: Converting '%s' -> '%s'" infile outfile))
               (dir (file-name-directory outfile))
               status)
          (message "%s" msg)
          (with-temp-buffer
            (when dir (make-directory dir t))
            (insert "\n" msg "\n")
            (setq status
                  ;; The PNG32 prefix seems to prevent certain colorspace issues
                  (call-process dtm-org-link-convert-executable nil (current-buffer) nil
                                "-density" "250" "-quality" "90"
                                infile (concat "PNG32:" outfile)))
            (unless (and (numberp status) (= 0 status))
              (insert (format "Error, '%s' exited with status %s"
                              dtm-org-link-convert-executable status))
              (setq warn-msg (concat warn-msg (buffer-string)))))))
      (message "Org[as_png]: Done")
      (if warn-msg (progn (warn warn-msg) nil) t))))

;;* Org-modern
(defun dtm-org-modern-mode-maybe-h ()
  "Activate `org-modern-mode' unless in `doom-emacs-dir'.
The additional markup used in doom-style org documents causes rendering issues."
  (unless (dtm-org-limit-styling-p) (org-modern-mode +1)))

;;* Org-download
(defun dtm-org-download-file-format (filename)
  "Prefix FILENAME with `buffer-name' and `org-download-timestamp'."
  (format "%s%s--%s"
          (file-name-base (buffer-name))
          (format-time-string org-download-timestamp)
          filename))

;;* Org-tree-slide
(defun dtm-org-tree-slide-setup-h ()
  "Additional settings to prettify presentations"
  (if org-tree-slide-mode
      (progn
        (setq-local buffer-read-only t
                    evil-normal-state-cursor 'hbar)
        (display-line-numbers-mode -1)
        (hl-line-mode -1)
        (mixed-pitch-mode +1)
        (+org-pretty-mode +1)
        (org-appear-mode -1)
        (add-hook! 'minibuffer-exit-hook :append #'redraw-frame)
        (add-hook! 'pdf-view-mode-hook :append #'org-tree-slide-mode))
    (progn
      (setq-local buffer-read-only nil)
      (mixed-pitch-mode -1)
      (remove-hook! 'minibuffer-exit-hook #'redraw-frame)
      (remove-hook! 'pdf-view-mode-hook #'org-tree-slide-mode))))

(defun dtm-org-tree-slide-no-squiggles-a ()
  "Toggle modes that litter the buffer with squiggly lines.
Use as advice :before `org-tree-slide--setup'."
  (outline-show-all)
  (flycheck-mode -1)
  (spell-fu-mode -1))

;;* Org-roam
(defun dtm-org-roam-goto-workspace (&rest _)
  "Open/create the dedicated org-roam workspace"
  (dtm-workspace-switch-maybe "*roam*"))

(defun dtm/org-roam-open-index ()
  "Open `dtm-org-roam-index-file' in dedicated workspace, activate `org-overview'."
  (interactive)
  (dtm-org-roam-goto-workspace)
  (find-file (expand-file-name dtm-org-roam-index-file org-roam-directory))
  (while-let ((lvl (org-up-heading-safe))
              ((not (eq 1 lvl)))))
  (org-overview)
  (recenter))

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

(defun dtm-org-roam-add-preamble-a (string)
  "Add information about current node to top of org roam buffer.
Ref: https://github.com/hlissner/.doom.d"
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

;;* Pdf-tools
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

(defun dtm-pdf-annot-edit-contents-setup-h ()
  "Apply personal customisations.
Intended for `pdf-annot-edit-contents-minor-mode-hook'"
  ;; Inherit `ispell-local-dictionary' from pdf buffer
  (when-let ((dict (buffer-local-value 'ispell-local-dictionary
                                       (pdf-annot-get-buffer
                                        pdf-annot-edit-contents--annotation))))
    (dtm/ispell-fu-change-dictionary dict))
  (auto-fill-mode -1)
  (+word-wrap-mode +1)
  (goto-char (point-max))
  (evil-insert-state))

;;* ESS
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

(defun dtm/ess-r-insert-assign ()
  "Rewrite of `ess-insert-assign' that respects white space, invoke twice to undo"
  (interactive)
  (dtm-ess-insert-string "<-"))

(defun dtm/ess-r-insert-pipe ()
  "Based on `ess-insert-assign', invoking the command twice reverts the insert"
  (interactive)
  (dtm-ess-insert-string "%>%"))

(defun dtm-ess-startup-dir ()
  "Returns `default-directory' unless in `ess-r-package-mode'."
  (unless ess-r-package-mode default-directory))

(defun dtm-ess-mode-line-compact-process ()
  "Set `mode-line-process' to a compacted version of the `ess-mode' default."
  (setcar mode-line-process "["))

(defun dtm-ess-mode-line-show-busy ()
  "Display spinner if ESS process is busy.
Ref: `ess--tb-start', https://github.com/seagle0128/doom-modeline/issues/410"
  (setq-local ess-busy-strings (cons "%s" (cdr ess-busy-strings))
              mode-line-process '("["
                                  ess--mode-line-process-indicator
                                  ess--local-mode-line-process-indicator
                                  "]: "
                                  (:eval (nth ess--busy-count ess-busy-strings))
                                  " ")))

(defun dtm/ess-lookup-documentation ()
  "Wrapper for `ess-display-help-on-object' to improve `+lookup/documentation'.
Bypasses `ess-completing-read', defers further lookup if process is busy."
  (interactive)
  (ess-make-buffer-current)
  (condition-case err
      (ess-display-help-on-object (ess-helpobjs-at-point--read-obj))
    (user-error
     (message "%s" (error-message-string err))
     'deferred)))

(defun dtm/ess-quit-and-kill-no-save ()
  "Call `ess-quit' and prompt to kill the inferior process buffer and window."
  (interactive)
  (when (ess-process-live-p)
    (let ((buf (ess-get-process-buffer)))
      (ess-quit 'no-save)
      (when (y-or-n-p "Kill ESS process window?")
        (when-let ((win (get-buffer-window buf)))
          (delete-window win))
        (kill-buffer buf)
        (when (functionp 'ess-plot-hide)
          (ess-plot-hide))))))

(defun dtm/ess-eval-rfp-and-step-recenter ()
  "Call `ess-eval-region-or-function-or-paragraph-and-step' and recenter."
  (interactive)
  (prog1
      (call-interactively #'ess-eval-region-or-function-or-paragraph-and-step)
    (recenter)))

(defun dtm/ess-eval-object-at-point ()
  "Send the object under the cursor or region to the current ESS process."
  (interactive)
  (ess-send-string
   (ess-get-current-process)
   (or (dtm-region-as-string 'deactivate)
       (when-let ((bounds (ess-bounds-of-symbol)))
         (buffer-substring-no-properties (car bounds) (cdr bounds)))
       (user-error "No object at point!"))
   t))

(defvar dtm-ess-debug-previous-position nil
  "Previous value of `ess--dbg-current-debug-position'.")

(defun dtm-ess-debug-track-previous ()
  "Update `dtm-ess-debug-previous-position'.
Intended as :before `ess--dbg-activate-overlays' advice."
  (setq dtm-ess-debug-previous-position
        (copy-marker ess--dbg-current-debug-position)))

(defun dtm/ess-debug-goto-previous (&optional no-history)
  "Goto to `dtm-ess-debug-previous-position' returning the buffer if successful.
Unless NO-HISTORY is non-nil `better-jumper-set-jump' is called before jumping."
  (interactive)
  (when (and (markerp dtm-ess-debug-previous-position)
             (buffer-live-p (marker-buffer dtm-ess-debug-previous-position)))
    (and (not no-history)
         (bound-and-true-p better-jumper-mode)
         (better-jumper-set-jump))
    (pop-to-buffer-same-window (marker-buffer dtm-ess-debug-previous-position))
    (goto-char (marker-position dtm-ess-debug-previous-position))
    (back-to-indentation)
    dtm-ess-debug-previous-position))

(defun dtm/ess-print-last-value ()
  "Print .Last.value in `ess-local-process-name'.
If `ess--dbg-is-active-p' eval the object at `dtm-ess-debug-previous-position'."
  (interactive)
  (save-window-excursion
    (if (and (ess--dbg-is-active-p)
             (dtm/ess-debug-goto-previous 'no-history))
        (dtm/ess-eval-object-at-point)
      (ess-send-string (ess-get-current-process) ".Last.value" t))))

(defun dtm-ess-plot-file-p (file)
  "Return non-nil if FILE is an ESS plot."
  (when (fboundp 'ess-plot-file-p)
    (ess-plot-file-p file)))

;; REVIEW unused
(defun dtm-ess-recenter-proc-buffer (&optional name)
  "Recenter the buffer of console process NAME if visible.
Defaults to `ess-local-process-name'."
  (interactive)
  (or name (setq name ess-local-process-name))
  (when-let* ((buf (ess-get-current-process-buffer))
              (win (get-buffer-window buf)))
    (with-selected-window win
      (move-to-window-line (- (1+ scroll-margin)))
      (recenter -1))))

(defun dtm-cape-ess-keep-cache-p (old new)
  "Return non-nil if cache based on OLD remains valid for NEW."
  (or (< (length new) corfu-auto-prefix)
      (and (> (length new) (length old))
           (not (string-suffix-p "$" new)))))

(defun dtm-cape-ess-r-object-completion ()
  "Wrapper for `ess-r-object-completion' with correct caching."
  (cape-wrap-buster #'ess-r-object-completion #'dtm-cape-ess-keep-cache-p))

(defun dtm-corfu-ess-r-setup-capf ()
  "Setup `completion-at-point-functions' for `ess-r-mode'.
Removes `ess-filename-completion' and replaces `ess-r-object-completion' in
favour of `cape-file' and `dtm-cape-ess-r-object-completion'."
  (remove-hook 'completion-at-point-functions #'ess-filename-completion 'local)
  (setq-local completion-at-point-functions
              (cl-substitute #'dtm-cape-ess-r-object-completion
                             #'ess-r-object-completion
                             completion-at-point-functions)))

;;* Python/Elpy-shell
(defun dtm-elpy-shell-get-doom-process-a (&optional sit)
  "Obtain a Python process using `+python/open-repl'.
Intended as override advice for `elpy-shell-get-or-create-process'.
Also prompts to activate a Conda env if executable is found."
  (or (python-shell-get-process)
      (progn
        (when (and (fboundp #'conda-env-list)
                   (require 'conda)
                   (ignore-errors (conda--get-executable-path)))
          (dtm/conda-env-guess))
        (let ((buf (save-selected-window (+python/open-repl))))
          (and sit (sit-for sit))
          (get-buffer-process buf)))))

(defun dtm-elpy-shell-send-string (str)
  "Send STR to Python shell using `elpy-shell-send-buffer'.
STR is first stripped and indented according to mode."
  (with-temp-buffer
    (insert (python-util-strip-string str))
    (indent-according-to-mode)
    (call-interactively #'elpy-shell-send-buffer)))

(defun dtm/elpy-send-region-and-step ()
  "Send current region to Python shell, step if region is multi-line."
  (interactive)
  (unless (use-region-p)
    (user-error "No valid active region!"))
  (dtm-elpy-shell-send-string (dtm-region-as-string 'deactivate)))

(defun dtm/elpy-send-statement-or-line ()
  (interactive)
  (if (python-info-statement-starts-block-p)
      (call-interactively #'elpy-shell-send-statement)
    (dtm-elpy-shell-send-string (dtm-current-line-as-string))))

(defun dtm/elpy-send-statement-or-line-and-step ()
  (interactive)
  (if (python-info-statement-starts-block-p)
      (call-interactively #'elpy-shell-send-statement-and-step)
    (dtm-elpy-shell-send-string (dtm-current-line-as-string))
    (forward-line)))

(defun dtm/elpy-send-current-and-step ()
  "Send current region, statement, or line to Python shell and step."
  (interactive)
  (if (use-region-p)
      (dtm/elpy-send-region-and-step)
    (dtm/elpy-send-statement-or-line-and-step)))

(defun dtm/elpy-print-symbol-or-region ()
  "Print the symbol at point or active region in the Python shell."
  (interactive)
  (let* ((reg (or (dtm-region-as-string 'deactivate)
                  (python-info-current-symbol)))
         (cmd (concat "print(" reg ")")))
    (dtm-elpy-shell-send-string cmd)))

;;* Conda
(defun dtm-conda-env-infer-name ()
  "Alternative `conda--infer-env-from-buffer' that ignores auto_activate_base
from `conda--get-config'. Still respects `conda-activate-base-by-default'."
  (or (and (bound-and-true-p conda-project-env-path)
           (conda-env-dir-to-name conda-project-env-path))
      (let* ((filename (buffer-file-name))
             (dir (if filename (f-dirname filename) default-directory)))
        (when dir
          (conda--get-name-from-env-yml (conda--find-env-yml dir))))
      (and conda-activate-base-by-default "base")))

(defun dtm/conda-env-guess ()
  "Prompt the user to activate env from `dtm-conda-env-infer-name'.
Alternative `conda-env-activate-for-buffer' that prompts before activation"
  (interactive)
  (require 'conda)
  (let ((conda-activate-base-by-default (called-interactively-p 'interactive)))
    (when-let ((name (dtm-conda-env-infer-name)))
      (and (not (string= name conda-env-current-name))
           (y-or-n-p (format "Activate Conda env: %s?" name))
           (conda-env-activate name)))))

(defun dtm-conda-env-guess-maybe ()
  "Run `dtm/conda-env-guess' unless `non-essential' or `dtm-buffer-remote-p'."
  (unless (or non-essential (dtm-buffer-remote-p))
    (dtm/conda-env-guess)))

;;* CTRLF
(defun dtm-translate-fuzzy-multi-literal (input)
  "Build a fuzzy-matching regexp from literal INPUT.
See `ctrlf-split-fuzzy' for how INPUT is split into subinputs.
Each subinput is quoted and the results are joined with a non-greedy \".*\n?.*\".
This enables each word of the query to be on consecutive non-blank lines."
  (string-join (mapcar #'regexp-quote (ctrlf-split-fuzzy input))  ".*?\\(?:\n.*?\\)??"))

;;* Tempel
(defun dtm/tempel-open-template-file ()
  "Open the last file in `tempel-path' in the other window."
  (interactive)
  (require 'tempel)
  (let ((enable-local-variables :all))
    (find-file-other-window (last tempel-path))
    (goto-char (point-min))))

(defun dtm/tempel-add-template ()
  "Open the last file in `tempel-path' & insert a heading for current major-mode."
  (interactive)
  (let ((mode (symbol-name major-mode)))
    (dtm/tempel-open-template-file)
    (goto-char (point-max))
    (backward-char)
    (while (doom-point-in-comment-p)
      (forward-line -1))
    (end-of-line)
    (insert (concat "\n" mode "\n\n()\n"))
    (backward-char 2)
    (recenter)
    (evil-insert-state)))

(defvar +file-templates-inhibit nil
  "If non-nil, inhibit file template expansion.
Copied from the `file-templates' doom module.")

(defun dtm-tempel-autoinsert-template ()
  "Get the autoinsert/empty file template for current-buffer."
  (require 'tempel)
  (alist-get '__ (tempel--templates)))

(defun dtm-tempel-autoinsert-check-h ()
  "Expand the autoinsert/emtpy file template into the current buffer.
The buffer must be non-read-only, empty, and there must
be an entry for __ in `tempel-path' for the current mode."
  (and (not +file-templates-inhibit)
       buffer-file-name
       (not buffer-read-only)
       (bobp) (eobp)
       (not (member (substring (buffer-name) 0 1) '("*" " ")))
       (not (buffer-modified-p))
       (null (buffer-base-buffer))
       (when-let ((template (dtm-tempel-autoinsert-template)))
         (tempel-insert template))))

(defun dtm-tempel-double-quote (elt)
  "Insert a single double quote using the \"d\" as template ELT.
For use in `tempel-user-elements'."
  (when (eq elt 'd) "\""))

(defun dtm-tempel-whitespace (elt)
  "Insert a space using \"_\" or N spaces using \"(_ N)\" as template ELT.
For use in `tempel-user-elements'."
  (when-let ((n (cond ((eq elt '_) 1)
                      ((eq (car-safe elt) '_) (cadr elt)))))
    (make-string n 32)))

(defun dtm-tempel-include (elt)
  "Insert template with NAME using \"(i NAME)\" as template ELT.
Ref: https://github.com/minad/tempel"
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))

;; Unused
(defun dtm-tempel-setup-capf-h ()
  "Add the Tempel Capf to `completion-at-point-functions'.
Caution: make sure `tempel-trigger-prefix' is not nil.
Meant for hooking onto `prog-mode-hook' and `text-mode-hook'."
  (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

;;* Pixel-scroll-precision-mode
(defun dtm-pixel-scroll-precision-mode-h ()
  "Prevent `pixel-scroll-precision-mode' from changing
`make-cursor-line-fully-visible'. Use with `pixel-scroll-precision-mode-hook'."
  (setq-default make-cursor-line-fully-visible t))

(defun dtm-window-usable-height ()
  "Return the usable height of the selected window.
Return the pixel height of the area of the selected window
that the cursor is allowed to be inside.
This is from the bottom of the header line to the top of the mode line.
Ref: `good-scroll--window-usable-height'."
  (let ((hl-height (window-header-line-height))
        (tl-height (window-tab-line-height))
        (w-edges (window-inside-pixel-edges)))
    (let ((w-top (- (nth 1 w-edges) hl-height))
          (w-bottom (+ (nth 3 w-edges) tl-height)))
      (- w-bottom w-top (+ hl-height tl-height)))))

(defvar dtm-precision-scroll-time-factor 5
  "Factor to increase scrolling time with when scroll the full display height.
Higher values give slower scrolling.")

(defun dtm-precision-scroll-window-fraction (fraction)
  "Scroll window by FRACTION of total height."
  (let* ((delta (* fraction (dtm-window-usable-height)))
         (pixel-scroll-precision-interpolation-total-time
          (max pixel-scroll-precision-interpolation-total-time
               (* pixel-scroll-precision-interpolation-total-time
                  (/ (abs delta) (display-pixel-height))
                  dtm-precision-scroll-time-factor))))
    (pixel-scroll-precision-interpolate delta nil 1)))

(defun dtm-precision-scroll-up ()
  "Scroll up half a window, obeying `pixel-scroll-precision-mode'."
  (interactive)
  (if pixel-scroll-precision-mode
      (dtm-precision-scroll-window-fraction 0.49)
    (call-interactively #'evil-scroll-up)))

(defun dtm-precision-scroll-down ()
  "Scroll down half a window, obeying `pixel-scroll-precision-mode'."
  (interactive)
  (if pixel-scroll-precision-mode
      (dtm-precision-scroll-window-fraction -0.49)
    (call-interactively #'evil-scroll-down)))

(defun dtm-precision-scroll-page-up ()
  "Scroll up a full page, obeying `pixel-scroll-precision-mode'."
  (interactive)
  (if pixel-scroll-precision-mode
      (dtm-precision-scroll-window-fraction 0.99)
    (call-interactively #'evil-scroll-page-up)))

(defun dtm-precision-scroll-page-down ()
  "Scroll down a full page, obeying `pixel-scroll-precision-mode'."
  (interactive)
  (if pixel-scroll-precision-mode
      (dtm-precision-scroll-window-fraction -0.99)
    (call-interactively #'evil-scroll-page-down)))

;;* Evil-collection
  (defun dtm-evil-collection-inhibit-insert-state-a (map-sym)
    "Advice that additionally ignores `evil-enter-replace-state' (R) in MAP-SYM.
Intended as :after advice for `evil-collection-inhibit-insert-state'."
    (evil-collection-define-key 'normal map-sym
      [remap evil-enter-replace-state] #'ignore))

;;* Commands
(defun dtm/load-session (file)
  "Stripped down `doom/load-session' with a proper default value.
Also checks if FILE exists."
  (interactive
   (let ((session-file (doom-session-file)))
     (list (read-file-name "Session to restore: "
                           (file-name-directory session-file)
                           session-file
                           t))))
  (doom-load-session file)
  (message "Session restored. Welcome back."))

(defun dtm/toggle-trash-delete ()
  "Toggle between trashing and deleting files"
  (interactive)
  (if delete-by-moving-to-trash
      (progn
        (setq delete-by-moving-to-trash nil)
        (message "Now deleting files PERMANTLY"))
    (setq delete-by-moving-to-trash t)
    (message "Now moving deleted files to trash")))

(defun dtm-advice-list (symbol)
  "Return the list of functions advising SYMBOL."
  (let (result)
    (advice-mapc (lambda (ad props) (push ad result))
     symbol)
    (nreverse result)))

(defun dtm/advice-remove (symbol advice)
  "Remove ADVICE from SYMBOL, with interactive support.
Ref: https://emacs.stackexchange.com/a/33344"
  (interactive
   (let* ((sym (intern (completing-read "Function: " obarray #'dtm-advice-list t)))
          (advice (let ((advice-names
                         (mapcar (lambda (ad) (cons (prin1-to-string ad) ad))
                                 (dtm-advice-list sym))))
                    (cdr (assoc (completing-read "Remove advice: " advice-names nil t)
                                advice-names)))))
     (list sym advice)))
  (advice-remove symbol advice))

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

(defun dtm/toggle-line-numbers ()
  "Toggle `display-line-numbers' on or off. With C-u toggle visual lines.
Relative lines are more performant, but fail with folded/wrapped lines"
  (interactive)
  (let ((type (if current-prefix-arg
                  (if (eq 'visual display-line-numbers) t 'visual)
                (not display-line-numbers))))
    (when (xor type display-line-numbers-mode)
      (display-line-numbers-mode 'toggle))
    (setq display-line-numbers type)))

(defun dtm/word-wrap-mode-no-fill ()
  "Enable `+word-wrap-mode' without wrapping at `fill-column'."
  (interactive)
  (setq-local +word-wrap-fill-style nil)
  (+word-wrap-mode 1))

(defun dtm/list-loaded-files ()
  "List loaded elisp files (`load-history'). C-u retains load order."
  (interactive)
  (with-current-buffer (get-buffer-create "*loaded-files*")
    (erase-buffer)
    (pop-to-buffer (current-buffer))
    (let ((files (seq-filter #'stringp (mapcar #'car load-history))))
      (insert (format "%s elisp files loaded:\n" (length files)))
      (unless current-prefix-arg
        (setq files (sort files #'string-lessp)))
      (dolist (file files) (insert "\n" file)))
    (goto-char (point-min))))

(defun dtm/diagnostics-describe ()
  "Describe the syntax checker/linter that provides current diagnostics."
  (interactive)
  (cond ((bound-and-true-p lsp-mode)
         (lsp-describe-session))
        ((bound-and-true-p flycheck-mode)
         (flycheck-verify-setup))
        ((bound-and-true-p flymake-mode)
         ;; Flymake doesn't seem to have a dedicated equivalent function
         (message "Flymake running: %s; disabled: %s"
                  (flymake-running-backends) (flymake-disabled-backends)))
        (t
         (user-error "No diagnostics backend detected. See `+default/diagnostics'."))))

;;** Move-splitter
(defun dtm/move-splitter-left (arg)
  "Move window splitter left. Ref: hydra-examples"
  (interactive "p")
  (require 'windmove)
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun dtm/move-splitter-right (arg)
  "Move window splitter right. Ref: hydra-examples"
  (interactive "p")
  (require 'windmove)
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun dtm/move-splitter-up (arg)
  "Move window splitter up. Ref: hydra-examples"
  (interactive "p")
  (require 'windmove)
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun dtm/move-splitter-down (arg)
  "Move window splitter down. Ref: hydra-examples"
  (interactive "p")
  (require 'windmove)
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))
