;;; dtm-lib.el -*- lexical-binding: t; -*-
;; Library of personal functions

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

(defun dtm-region-as-string ()
  "Return the marked region as string."
  (when (use-region-p)
    (buffer-substring-no-properties (mark) (point))))

(defun dtm-current-line-as-string ()
  "Return the current line as string."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun dtm-line-empty-p ()
  "Returns t if line contains no text or whitespace."
  (and (bolp) (eolp)))

(defun dtm-forward-line-non-empty ()
  "Move cursor to the start of the next non-empty line."
  (forward-line)
  (while (and (dtm-line-empty-p)
              (not (eobp)))
    (forward-line)))

(defun dtm-point-mark-same-line-p ()
  "Returns t if point and mark are on the same line"
  (<= (line-beginning-position) (mark) (line-end-position)))

(defun dtm-deactivate-mark ()
  "Run `deactivate-mark' keeping point at the left side of region."
  (when (and (dtm-point-mark-same-line-p)
             (< (mark) (point)))
    (exchange-point-and-mark))
  (deactivate-mark))

(defun dtm-straight-prioritize (dir)
  "Move straight package DIR to the front of `load-path'."
  (let ((lib-dir (file-name-concat straight-base-dir "straight"
                                   straight-build-dir dir)))
    (when (file-exists-p lib-dir)
      (setq load-path (cons lib-dir (delete lib-dir load-path))))))

;;* Buffer functions
(defun dtm-buffer-remote-p (&optional buf)
  "Returns t if BUF belongs to a remote directory."
  (or buf (setq buf (current-buffer)))
  (ignore-errors (file-remote-p (buffer-local-value 'default-directory buf))))

(defun dtm-read-display-buffer (prompt &optional predicate)
  "Display buffer by providing user with PROMPT on buffers matching PREDICATE."
  (when-let ((buf (read-buffer (format-prompt prompt nil)
                               nil t predicate)))
    (display-buffer buf)))

;;* Window functions
(defun dtm/split-window-optimally (&optional w/h-factor)
  "Split window based on width/height of `window-inside-pixel-edges'.
A larger W/H-FACTOR favours splitting vertically (i.e. down)."
  (interactive)
  (let* ((w/h-factor (or w/h-factor 1.5))
         (w-edges (window-inside-pixel-edges))
         (width (- (nth 2 w-edges) (nth 0 w-edges)))
         (height (- (nth 3 w-edges) (nth 1 w-edges)))
         (w/h (/ width height 1.0)))
    (if (> w/h w/h-factor)
        (split-window-horizontally)
      (split-window-vertically))))

;;* Theme recommendations
(defun dtm--theme-which-inactive (theme1 theme2)
  "Return THEME1 of not currently active, else return THEME2"
  (if (eq theme1 (car custom-enabled-themes)) theme2 theme1))

(defun dtm-recommend-theme ()
  "Recommend a NEW theme to use based on context and time of day."
 (if (bound-and-true-p org-tree-slide-mode)
     dtm-presentation-theme
   (let ((hour (string-to-number (substring (current-time-string) 11 13))))
     (if (member hour (number-sequence dtm-first-hour-of-day dtm-last-hour-of-day))
         (dtm--theme-which-inactive dtm-day-theme dtm-solarized-theme)
       (dtm--theme-which-inactive dtm-night-theme dtm-dark-theme)))))

(defun dtm/consult-theme ()
  "Call `consult-theme' interactively with `dtm-recommend-theme' as default.
This is achieved by locally redefining `consult--read'.
Ref: https://nullprogram.com/blog/2017/10/27/"
  (interactive)
  (require 'consult)
  (letf! ((defun consult--read (candidates &rest options)
            (apply consult--read candidates
                   (plist-put options :default (symbol-name (dtm-recommend-theme))))))
    (call-interactively #'consult-theme)))

;;* UI
(defun dtm-doom-check-fonts ()
  "Check if doom fonts are installed, otherwise prevent a blank display."
  (dolist (spec (list doom-font doom-serif-font doom-variable-pitch-font))
    (when (and spec (not (find-font spec)))
      (warn "Font \"%s\" not found!" (font-get spec :family))
      (font-put spec :family nil))))

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

(defun dtm/switch-orphan-buffer ()
  "Prompt user to select buffer matching `dtm-buffer-orphan-p'."
  (interactive)
  (dtm-read-display-buffer "Select orphan buffer" #'dtm-buffer-orphan-p))

;;* Projectile
(defun dtm-project-ignored-p (project-root)
  "Return non-nil if remote or temporary file or a straight package."
  (or (file-remote-p project-root)
      (file-in-directory-p project-root temporary-file-directory)
      (file-in-directory-p project-root doom-local-dir)))

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
Use for `visual-line-mode-hook'. Also fixes `doom/toggle-line-numbers'."
  (let ((wrong-type (if visual-line-mode 'relative 'visual))
        (correct-type (if visual-line-mode 'visual 'relative)))
    (when (eq display-line-numbers wrong-type)
      (setq-local display-line-numbers correct-type))
    (when (eq display-line-numbers-type wrong-type)
      (setq-local display-line-numbers-type correct-type))))

;;* Doom Popup
(defun dtm/popup-raise ()
  "Wrapper for `+popup/raise' that will ensure a popup is selected."
  (interactive)
  (unless (+popup-window-p) (+popup/other))
  (call-interactively #'+popup/raise))

(defun dtm/popup-kill ()
  "Kill the currently open popup."
  (interactive)
  (unless (+popup-window-p) (+popup/other))
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

(defun dtm/switch-popup-buffer ()
  "Prompt user to select buffer matching `dtm-popup-buffer-p'."
  (interactive)
  (dtm-read-display-buffer "Select popup" #'dtm-popup-buffer-p))

;;* Imenu
(defun dtm-elisp-extend-imenu-h ()
  "Add `modulep!' support to `imenu' as the 2nd element."
  (push '("Module" "^\\s-*(when (modulep! +\\([^)]+\\))" 1)
        (cdr imenu-generic-expression)))

(defvar dtm-imenu-orginal-index-function nil
  "Original indexing function before calling `dtm-imenu-merge-index-h'")

(defun dtm-imenu-merge-index-h ()
  "Append results from `imenu-generic-expression' to the current imenu (add to major-mode hook).
This is useful when the index function does not utilise the generic expression such as in python-mode."
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

(defun dtm-ibuffer-workspace-filter-groups ()
  "Generate value for `ibuffer-filter-groups' based on perspectives."
  (mapcar #'(lambda (pn) (list pn (cons 'persp pn)))
          (nconc
           (cl-delete persp-nil-name (persp-names-current-frame-fast-ordered)
                      :test 'string=)
           (list persp-nil-name))))

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

(defun dtm/dirvish-find-entry ()
  "Like `find-file' but for use in `dirvish' buffers."
  (interactive)
  (dirvish-find-entry-a
   (read-file-name "Open: " nil default-directory
                   (confirm-nonexistent-file-or-buffer))))

(defun dtm/dirvish-search-cwd ()
  "Text search files from current working directory, kill dirvish on confirm."
  (interactive)
  (require 'consult)
  (let ((consult--buffer-display #'identity)
        (dv (dirvish-curr)))
    (+default/search-cwd)
    (let ((buf (current-buffer)))
      (dirvish-kill dv)
      (switch-to-buffer buf))))

(defun dtm/dirvish-narrow ()
  "Run `dirvish-narrow' and provide revert instruction after finish."
  (interactive)
  (call-interactively #'dirvish-narrow)
  (message "Run `revert-buffer' (%s) to un-narrow"
           (substitute-command-keys "\\[revert-buffer]")))

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
    (unless (derived-mode-p 'image-mode)
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
Intended as :around advice for `elisp-refs--find-file'."
  (find-file-other-window (button-get button 'path))
  (goto-char (point-min)))

;;* Lispy
(defun dtm/lispy-step-into (arg)
  "Step into the list at point, moving the point to after ARG atoms.
If REGION is active, call `lispy-delete' instead."
  (interactive "p")
  (cond ((region-active-p)
         (call-interactively #'lispy-delete))
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

(defun dtm/lispy-evil-yank-sexp ()
  "Call `evil-yank' on the region of `lispy-mark-list'."
  (interactive)
  (save-excursion
    (let ((evil-move-cursor-back nil))
      (evil-with-state normal
        (lispy-mark-list 1)
        (call-interactively #'evil-yank)))))

;;* Lispyville
(defmacro dtm-lispyville-smart-remap (evil-fn lispy-fn)
  "Remap EVIL-FN to LISPY-FN unless `lispy--in-string-or-comment-p' is non-nil.
Ref: https://github.com/noctuid/lispyville/issues/284"
  `(define-key lispyville-mode-map
     [remap ,evil-fn]
     (general-predicate-dispatch ,lispy-fn
       (lispy--in-string-or-comment-p) #',evil-fn)))

;;* Vterm
(defun dtm-vterm-redraw-cursor-a (orig-fn &rest args)
  "Prevent vterm from modifying `cursor-type'..
Intended as around advice for `vterm--redraw'
Ref: https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1191400836"
  (let ((cursor-type cursor-type)) (apply orig-fn args)))

(defun dtm-vterm-sync-cursor-a (&rest _)
  "Keep vterm cursor position consistent with evil.
Intended as before advice for `vterm-send-key'"
  (vterm-goto-char (point)))

(defun dtm/vterm-execute-current-line ()
  "Execute the current line in the vterm buffer."
  (interactive)
  (when (dtm-line-empty-p) (dtm-forward-line-non-empty))
  (+nav-flash-blink-cursor)
  (let ((command (dtm-current-line-as-string)))
    (save-selected-window
      (vterm-other-window)
      (vterm--goto-line -1)
      (vterm-send-string command)
      (vterm-send-return)))
  (dtm-forward-line-non-empty))

;;* Ispell-fu
(defun dtm/ispell-fu-change-dictionary ()
  "Interactively set `ispell-local-dictionary' & `ispell-local-pdict'.
These values are used to override `spell-fu-dictionaries'. Sets
`ispell-local-pdict' to \"default\" if language of selected dictionary does
not match with `ispell-dictionary', preventing \"expected language x\" errors
caused by a language mismatch with `ispell-personal-dictionary'.
Ref: `ispell-change-dictionary', `spell-fu-dictionary-add'"
  (interactive)
  (require 'consult)
  (setq ispell-local-dictionary
        (consult--read (mapcar #'list (ispell-valid-dictionary-list))
                       :prompt "Change buffer-local dictionary: "
                       :default (or ispell-local-dictionary ispell-dictionary)
                       :require-match t)
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

(defun dtm/spell-correct-previous ()
  "Correct the previous spelling error."
  (interactive)
  (save-excursion
    (+spell/previous-error)
    (+spell/correct)))

;;* Markdown
(defun dtm-flycheck-disable-proselint-rmd-h ()
  "Disable the 'proselint' flycheck checker when in R markdown.
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
      (dtm-doom-docs-p)))

(defun dtm-org-mode-setup-h ()
  "Personal org-mode customisation's after mode startup"
  (unless (dtm-org-limit-styling-p)
    (setq-local line-spacing dtm-org-line-spacing)
    (electric-quote-local-mode +1)
    (+org-pretty-mode +1)
    (visual-line-mode -1)
    (visual-fill-column-mode +1)
    (auto-fill-mode +1)
    (add-hook! 'evil-insert-state-exit-hook
               :local #'dtm-insert-exit-fill-paragraph)))

(defun dtm-org-get-title-value ()
  "Returns the value of #+TITLE for the current document"
  (cadar (org-collect-keywords '("TITLE"))))

(defun dtm-org-src-flycheck-h ()
  "Disable annoying flycheck messages. Use with `org-src-mode-hook'."
  (when (eq major-mode 'emacs-lisp-mode)
    (flycheck-mode -1)))

(defun dtm-insert-exit-fill-paragraph ()
  "Perform `org-fill-paragraph' unless el at point is a src block"
  ;; Check if `auto-fill-mode' is active
  (when auto-fill-function
    (unless (memq (org-element-type (org-element-at-point))
                  '(src-block comment-block))
      (org-fill-paragraph))))

;;* Org-modern
(defun dtm-org-modern-mode-maybe-h ()
  "Activate `org-modern-mode' unless in `doom-emacs-dir'.
The additional markup used in doom-style org documents causes rendering issues."
  (unless (dtm-org-limit-styling-p) (org-modern-mode +1)))

(defun dtm-org-modern-indent-heading ()
  "Correctly indents heading assuming leading stars are fully hidden (not invisible)."
  (dotimes (n org-indent--deepest-level)
    (unless (= n 0)
      (let* ((indentation (* org-indent-indentation-per-level (1- n)))
             (heading-prefix (make-string indentation ?\s)))
        (aset org-indent--heading-line-prefixes
              n
              (org-add-props heading-prefix nil 'face 'org-indent))))))

;;* Org-appear
(defun dtm-org-pretty-use-appear-a ()
  "Activate `org-appear-mode' based on `org-pretty-entities'.
Intended as after advice for `org-toggle-pretty-entities'."
  (org-appear-mode (if org-pretty-entities +1 -1)))

;;* Org-download
(defun dtm-org-download-method (link)
  "This is a helper function for `org-download-method'.
It creates the \"./Image\" folder within the same directory of the org file.
File is named as: org-file + timestamp + download-file-name

Based on drestivo's answer to this post:
https://github.com/abo-abo/org-download/issues/40.
Which was based off this commit message:
https://github.com/abo-abo/org-download/commit/137c3d2aa083283a3fc853f9ecbbc03039bf397b"
  (let* ((filename (file-name-nondirectory
                    (car (url-path-and-query
                          (url-generic-parse-url link)))))
         (filename-with-timestamp
          (format "%s%s--%s.%s"
                  (file-name-base (buffer-file-name))
                  (format-time-string org-download-timestamp)
                  (file-name-sans-extension filename)
                  (file-name-extension filename))))
    (file-name-concat (dtm-ensure-dir "Images")
                      filename-with-timestamp)))

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
  (unless (org-at-heading-p t)
    (org-backward-heading-same-level 1))
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
(defun dtm-org-roam-dailies-goto-date-a ()
  "Ensure function is executed from a roam buffer to activate keybindings.
Intended as :before advice for `org-roam-dailies-goto-date'"
  (dtm-org-roam-goto-workspace)
  (unless (org-roam-buffer-p)
    (dtm/org-roam-open-index)))

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
  (let ((date (dtm-org-get-title-value))
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

(defun dtm/org-noter-insert-maybe ()
  "Call `org-noter-insert-note' if `org-noter-doc-mode' is active."
  (interactive)
  (when org-noter-doc-mode (call-interactively #'org-noter-insert-note)))

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

(defun dtm-ess-modeline-show-busy ()
  "Display spinner if ESS process is busy.
Ref: `ess--tb-start', https://github.com/seagle0128/doom-modeline/issues/410"
  (setq-local ess-busy-strings (cons "%s" (cdr ess-busy-strings))
              mode-line-process '("["
                                  ess--mode-line-process-indicator
                                  ess--local-mode-line-process-indicator
                                  "]: "
                                  (:eval (nth ess--busy-count ess-busy-strings))
                                  " ")))

(defun dtm/ess-eval-symbol-at-point ()
  "Send the symbol under the cursor to the current ESS process"
  (interactive)
  (ess-send-string
   (get-process ess-current-process-name)
   (symbol-name (ess-symbol-at-point))
   t))

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

;;** dtm-ess-r-plot
(defvar dtm-ess-r-plot-dummy-name "*R plot*"
  "Name of the placeholder plot buffer.")

(defvar dtm-ess-r-plot-process-name nil
  "ESS process currently displaying plots inside emacs.")

(defvar dtm-ess-r-plot-descriptor nil
  "File notify descriptor watching the plot folder.")

(defun dtm-ess-r-plot-running-p ()
  "Return t if emacs is currently displaying R plots."
  (if dtm-ess-r-plot-descriptor t nil))

(defun dtm-ess-r-plot-dir ()
  "Create and return the R plot dir based on `ess-current-process-name'."
  (if (dtm-ess-r-plot-running-p)
      (thread-last
        (gethash dtm-ess-r-plot-descriptor file-notify-descriptors)
        (file-notify--watch-directory)
        (file-name-as-directory))
    (let ((tmp-dir (thread-last
                     (ess-get-words-from-vector "tempdir(check=TRUE)")
                     (car) (file-name-as-directory))))
      (unless (file-exists-p tmp-dir)
        (error "ESS: cannot get valid tempdir() from R process"))
      (dtm-ensure-dir "session_plots" tmp-dir))))

(defun dtm-ess-r-plot-file-p (file)
  "Return non-nil if FILE is an R plot."
  (and (dtm-ess-r-plot-running-p)
       (file-in-directory-p file (dtm-ess-r-plot-dir))))

(defun dtm-ess-r-plot-buffer-p (&optional buf)
  "Return BUF if it displays an R plot. Defaults to `current-buffer'."
  (when-let* ((buf (or buf (current-buffer)))
              (file (buffer-file-name buf)))
    (when (dtm-ess-r-plot-file-p file) buf)))

(defun dtm-ess-r-plot-buffers ()
  "Return a list of buffers associated with an R plot."
  (delq nil (cons (get-buffer dtm-ess-r-plot-dummy-name)
                  (mapcar #'dtm-ess-r-plot-buffer-p (buffer-list)))))

(defun dtm-ess-r-plot-window ()
  "Return the window currently displaying R plots."
  (cl-some #'get-buffer-window (dtm-ess-r-plot-buffers)))

(defun dtm-ess-r-plot-force-window ()
  "Return window for displaying R plot files, create if it not exists."
  (or (dtm-ess-r-plot-window)
      (save-selected-window
        (unless (eq (ess-get-process-buffer dtm-ess-r-plot-process-name)
                    (current-buffer))
          (ess-switch-to-ESS t))
        (select-window (dtm/split-window-optimally))
        (switch-to-buffer (generate-new-buffer dtm-ess-r-plot-dummy-name))
        (setq-local default-directory (dtm-ess-r-plot-dir))
        (selected-window))))

(defun dtm-ess-r-plot-cleanup-buffers (&optional kill-visible)
  "Kill all unmodified buffers dedicated to R plot files.
Only kill visible plot buffers if KILL-VISIBLE is t."
  (when-let ((bufs (dtm-ess-r-plot-buffers)))
    (mapc (lambda (buf)
            (unless (buffer-modified-p buf)
              (if-let ((win (get-buffer-window buf)))
                  (when kill-visible
                    (delete-window win)
                    (kill-buffer buf))
                (kill-buffer buf))))
          bufs)))

(defun dtm-ess-r-plot-file-notify-open (event)
  "Display file created by EVENT in `dtm-ess-r-plot-force-window'."
  (when (and (eq 'created (nth 1 event))
             (file-name-extension (nth 2 event)))
    (set-window-buffer (dtm-ess-r-plot-force-window)
                       (find-file-noselect (nth 2 event)))
    (dtm-ess-r-plot-cleanup-buffers)
    (message "ESS: updated plot")))

(defun dtm/ess-r-plot-toggle ()
  "Toggle displaying R plots in emacs.
Relies on using 'dtm::print_plot()' inside of R."
  (interactive)
  ;; Run `ess-command' first because it raises an `user-error' if process is busy
  (if (dtm-ess-r-plot-running-p)
      (let ((ess-local-process-name dtm-ess-r-plot-process-name))
        (when (ess-process-live-p)
          (ess-command "options(dtm.print_plot=NULL)"))
        (dtm-ess-r-plot-cleanup-buffers t)
        (file-notify-rm-watch dtm-ess-r-plot-descriptor)
        (setq dtm-ess-r-plot-process-name nil
              dtm-ess-r-plot-descriptor nil)
        (when (called-interactively-p 'interactive)
          (message "ESS: stopped displaying plots in emacs")))
    (ess-force-buffer-current)
    (ess-command "options(dtm.print_plot=\"png\")")
    (setq dtm-ess-r-plot-descriptor (file-notify-add-watch
                                     (dtm-ess-r-plot-dir)
                                     '(change)
                                     #'dtm-ess-r-plot-file-notify-open)
          dtm-ess-r-plot-process-name ess-current-process-name)
    (dtm-ess-r-plot-force-window)
    (when (called-interactively-p 'interactive)
      (message "ESS: displaying plots in emacs"))))

(defun dtm-ess-r-plot-reload-a (orig-fn &rest args)
  "Reload R plot display if active and attached to `ess-current-process-name'.
Intended as :around advice for `inferior-ess-reload'."
  (and (dtm-ess-r-plot-running-p)
       (ess-force-buffer-current)
       (string= ess-current-process-name dtm-ess-r-plot-process-name)
       (let ((running-p (dtm-ess-r-plot-running-p)))
         (when running-p (dtm/ess-r-plot-toggle))
         (apply orig-fn args)
         (when running-p (dtm/ess-r-plot-toggle)))))

;;** dtm-with-lagging-point
(defvar dtm-lagging-point-actual nil
  "Position of cursor when `dtm-with-lagging-point-a' would not have been active.")

(defun dtm-with-lagging-point-a (orig-fn)
  "Keep/lag the cursor position one command execution behind.
Indented to advise functions that move the point."
  (dtm/lagging-point-goto-actual)
  (save-excursion
    (funcall orig-fn)
    (sleep-for .05)
    (setq-local dtm-lagging-point-actual (point))))

(defun dtm/lagging-point-goto-actual ()
  "Restore cursor to the unlagged position."
  (interactive)
  (when dtm-lagging-point-actual
    (goto-char dtm-lagging-point-actual)
    (recenter nil)))

(defun dtm-lagging-point-reset ()
  "Reset `dtm-lagging-point-actual'."
  (setq-local dtm-lagging-point-actual nil))

;;* Python/Elpy-shell
(defun dtm-elpy-shell-get-doom-process-a (&optional sit)
  "Obtain a Python process using `+python/open-repl'.
Intended as override advice for `elpy-shell-get-or-create-process'.
Also prompts to activate a Conda env if executable is found."
  (if-let* ((bufname (format "*%s*" (python-shell-get-process-name nil)))
            (proc (get-buffer-process bufname)))
      proc
    (when (and (fboundp #'conda--get-executable-path)
               (ignore-errors (conda--get-executable-path)))
      (dtm/conda-env-guess))
    (let ((buf (save-selected-window (+python/open-repl))))
      (when sit (sit-for sit))
      (get-buffer-process buf))))

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
  (dtm-elpy-shell-send-string (dtm-region-as-string))
  (dtm-deactivate-mark))

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
  (let* ((reg (or (dtm-region-as-string)
                  (python-info-current-symbol)))
         (cmd (concat "print(" reg ")")))
    (dtm-elpy-shell-send-string cmd))
  (when (use-region-p)
    (dtm-deactivate-mark)))

;;* Conda
(defun dtm-conda-env-infer-name ()
  "Alternative `conda--infer-env-from-buffer' that ignores auto_activate_base
from `conda--get-config'. Still respects `conda-activate-base-by-default'."
  (or (and (bound-and-true-p conda-project-env-path)
           (conda-env-dir-to-name conda-project-env-path))
      (when-let* ((filename (buffer-file-name))
                  (dir (if filename (f-dirname filename) default-directory)))
        (conda--get-name-from-env-yml (conda--find-env-yml dir)))
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
Each subinput is quoted and the results are joined with \".*\n*.*\".
This enables the each word of the query to be on a consecutive non-blank line."
  (string-join (mapcar #'regexp-quote (ctrlf-split-fuzzy input)) ".*\n*.*"))

;;* Tempel
(defun dtm/tempel-complete-always ()
  "Trigger `tempel-complete' regardless if `tempel-trigger-prefix' is provided.
Auto-expand on exact match."
  (interactive)
  (require 'tempel)
  (let ((tempel-trigger-prefix (when (tempel--prefix-bounds) tempel-trigger-prefix)))
    (call-interactively #'tempel-complete)
    (when (and (not tempel--active)
               (tempel-expand))
      (call-interactively #'tempel-expand))))

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
Copied from the 'file-templates' doom module.")

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
  "Insert a single double quote using the 'd' as template ELT.
For use in `tempel-user-elements'."
  (when (eq elt 'd) "\""))

(defun dtm-tempel-whitespace (elt)
  "Insert a space using '_' or N spaces using '(_ N)' as template ELT.
For use in `tempel-user-elements'."
  (when-let ((n (cond ((eq elt '_) 1)
                      ((eq (car-safe elt) '_) (cadr elt)))))
    (make-string n 32)))

(defun dtm-tempel-include (elt)
  "Insert template with NAME using '(i NAME)' as template ELT.
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
              (cons #'tempel-complete completion-at-point-functions)))

;;* Good-scroll
(defun dtm/good-scroll-down-half ()
  (interactive)
  (good-scroll-move (/ (good-scroll--window-usable-height) 2)))

(defun dtm/good-scroll-up-half ()
  (interactive)
  (good-scroll-move (/ (good-scroll--window-usable-height) -2)))

(defun dtm-good-scroll-evil-override-h ()
  (if good-scroll-mode
      (progn
        (advice-add 'evil-scroll-down :override #'dtm/good-scroll-down-half)
        (advice-add 'evil-scroll-up :override #'dtm/good-scroll-up-half))
    (progn
      (advice-remove 'evil-scroll-down #'dtm/good-scroll-down-half)
      (advice-remove 'evil-scroll-up #'dtm/good-scroll-up-half))))

;;* GPTEL
(defvar dtm-gptel-dir nil
  "Directory for storing chats.")

(defun dtm-gptel-setup-h ()
  "Personal gptel-mode customisation's. Intended for `gptel-mode-hook'."
  (setq default-directory (or dtm-gptel-dir default-directory))
  (visual-line-mode +1)
  (flycheck-mode -1))

(defun dtm/gptel-send-buffer ()
  "Scroll to the end and call `gptel-send' to ensure the full buffer is send."
  (interactive)
  (goto-char (line-beginning-position))
  (recenter 0)
  (goto-char (point-max))
  (call-interactively #'gptel-send))

(defun dtm-gptel-goto-workspace (&rest _)
  "Open/create workspace for ChatGPT conversations."
  (dtm-workspace-switch-maybe "*gpt*"))

(defun dtm/gptel-new-chat ()
  "Open a new chat in the dedicated workspace."
  (interactive)
  (require 'gptel)
  (ignore-errors (gptel--api-key))
  (dtm-gptel-goto-workspace)
  (let ((gptel-default-session (generate-new-buffer-name "ChatGPT")))
    (call-interactively #'gptel)
    (persp-add-buffer gptel-default-session (get-current-persp) nil nil)
    (when (length= (+workspace-buffer-list) 1)
      (delete-other-windows))
    (with-current-buffer gptel-default-session
      (visual-fill-column-mode +1)
      (push '(continuation nil nil) fringe-indicator-alist))))

;;* Commands
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

(defun dtm/window-toggle-left-margin ()
  "Toggle left margin on selected window."
  (interactive)
  (let ((window (selected-window)))
    (set-window-margins window (unless (car (window-margins window)) dtm-left-margin))))

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

;;** Move-splitter
(defun dtm/move-splitter-left (arg)
  "Move window splitter left. Ref: hydra-examples"
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun dtm/move-splitter-right (arg)
  "Move window splitter right. Ref: hydra-examples"
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun dtm/move-splitter-up (arg)
  "Move window splitter up. Ref: hydra-examples"
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun dtm/move-splitter-down (arg)
  "Move window splitter down. Ref: hydra-examples"
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))