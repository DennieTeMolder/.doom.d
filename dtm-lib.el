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
  "Returns t if line contains only whitespace"
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

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

;;* Window functions
(defun dtm/split-window-optimally (&optional w/h-ratio)
  "Split window based on width to height ratio (including margins/fringes/bars).
When W/H is lower then W/H-RATIO split below, else split right."
  (interactive)
  (or w/h-ratio (setq w/h-ratio 1.5))
  (let ((type (if (< (window-pixel-width) (* w/h-ratio (window-pixel-height)))
                  'below 'right)))
    (select-window (split-window (selected-window) nil type))))

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
  (display-buffer buf))

;;* Projectile
(defun dtm-project-ignored-p (project-root)
  "Return non-nil if remote or temporary file or a straight package."
  (or (file-remote-p project-root)
      (file-in-directory-p project-root temporary-file-directory)
      (file-in-directory-p project-root doom-local-dir)))

;;* Doom Popup
(defun dtm-popup-ensure ()
  "Ensure a popup is selected."
  (unless (+popup-window-p)
    (unless (+popup/other)
      (user-error "No popups are open")))
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
  (when-let* ((buf (or buf (current-buffer)))
              (rule (dtm-popup-get-rule buf)))
    (eq '+popup-buffer (caadr rule))))

(defun dtm/switch-popup-buffer (buf)
  "Prompt user to select buffer matching `dtm-popup-buffer-p'."
  (interactive
   (list (read-buffer "Select popup: " nil t #'dtm-popup-buffer-p)))
  (display-buffer buf))

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
      ("popups" (predicate dtm-popup-buffer-p))
      (,persp-nil-name (persp . ,persp-nil-name)))))

(defun dtm-ibuffer-group-by-persp-h ()
  "Set the current filter groups to filter by perspective.
Based on `ibuffer-projectile-set-filter-groups' from the ibuffer-projectile package:
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
(defun dtm-spell-fu-tree-sitter-h ()
  "Add tree-sitter font's to `spell-fu-faces-include'."
  (when (and spell-fu-mode (derived-mode-p 'prog-mode))
    (setq spell-fu-faces-exclude
          (delq 'font-lock-constant-face spell-fu-faces-exclude))
    (pushnew! spell-fu-faces-include
              'tree-sitter-hl-face:comment
              'tree-sitter-hl-face:string
              'tree-sitter-hl-face:doc)))

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
    (+org-pretty-mode +1)
    (auto-fill-mode +1)
    (when (bound-and-true-p pdf-annot-edit-contents-minor-mode)
      (+word-wrap-mode +1))
    (+zen-light-toggle +1)
    (add-hook! 'evil-insert-state-exit-hook
               :local #'dtm-insert-exit-fill-paragraph)))

(defun dtm-org-get-title-value ()
  "Returns the value of #+TITLE for the current document"
  (cadar (org-collect-keywords '("TITLE"))))

(defun dtm-insert-exit-fill-paragraph ()
  "Perform `org-fill-paragraph' unless el at point is a src block"
  ;; Check if `auto-fill-mode' is active
  (when auto-fill-function
    (unless (memq (org-element-type (org-element-at-point-no-context))
                  '(src-block comment-block))
      (org-fill-paragraph))))

;;** Org-link
(defvar dtm-org-link-convert-p 'ask
  "Indicates if `dtm-org-link-as-png-convert' should attempt to convert files.")

(defvar dtm-org-link-convert-executable
  ;; Avoid using the MS Windows command convert.exe .
  (unless (memq system-type '(ms-dos windows-nt))
    'executable-find)
  "Absolute path to the ImageMagick/convert program.")

(defun dtm-org-link-as-png-maybe (infile outfile)
  "Convert INFILE to OUTFILE as .png using `dtm-org-link-convert-executable'.
Controlled by `dtm-org-link-convert-p'. Only runs if INFILE newer then OUTFILE.
Returns the created file or nil on failure."
  (setq outfile (concat (file-name-sans-extension outfile) ".png"))
  (catch 'result
    ;; Short-circuit if outfile does not need to or can not be generated
    (unless (and dtm-org-link-convert-p
                 (file-readable-p infile)
                 (or (not (file-readable-p outfile))
                     (file-newer-than-file-p infile outfile)))
      (throw 'result outfile))
    ;; Set user variables if they contain special placeholder values
    (when (eq 'ask dtm-org-link-convert-p)
      (or (setq-local dtm-org-link-convert-p
                      (y-or-n-p "Outdated/missing PNG conversions detected, Update?"))
          (throw 'result outfile)))
    (when (eq 'executable-find dtm-org-link-convert-executable)
      (setq dtm-org-link-convert-executable (executable-find "convert")))
    ;; Check if executable is valid
    (if (not (and dtm-org-link-convert-executable
                  (file-executable-p dtm-org-link-convert-executable)))
        (warn "Org[as_png]: `dtm-org-link-convert-executable' unset or non-executable!")
      ;; Start conversion
      (let ((msg (format "Org[as_png]: Converting '%s' -> '%s'" infile outfile))
            (dir (file-name-directory outfile))
            status)
        (message "%s" msg)
        (when dir (make-directory dir t))
        (with-temp-buffer
          (insert "\n" msg "\n")
          (setq status
                ;; The PNG32 prefix seems to prevent certain colorspace issues
                (call-process dtm-org-link-convert-executable nil (current-buffer) nil
                              "-density" "250" "-quality" "90"
                              infile (concat "PNG32:" outfile)))
          (when (and (numberp status) (= 0 status))
            (throw 'result outfile))
          (insert (format "Error, '%s' exited with status %s"
                          dtm-org-link-convert-executable status))
          (warn (buffer-string)))))
    ;; No result catched
    ;; TODO add to list of ignored input files
    (setq-local
     dtm-org-link-convert-p
     (not (y-or-n-p (concat "Conversion to PNG failed, disable all PNG "
                            "conversions for this buffer?"))))
    nil))

(defun dtm-org-link-as-png (tag)
  "Convert TAG to .png using `dtm-org-link-as-png-maybe', always returns a string.
The TAG can include an additional nested \"linkkey\", in which case the result
will be placed in ./img/$NESTED_LINKKEY/$NESTED_TAG (as defined by \"#+LINK:\").

Intended for use in `org-link-abbrev-alist'."
  (or (dtm-org-link-as-png-maybe
       (org-link-expand-abbrev tag)
       (if-let ((split-at (string-search ":" tag)))
           (file-name-concat "." "Media"
                             (substring tag nil split-at)
                             (substring tag (1+ split-at) nil))
         tag))
      "/file_could_not_be_converted_to_png.txt"))

;;* Org-modern
(defun dtm-org-modern-mode-maybe-h ()
  "Activate `org-modern-mode' unless in `doom-emacs-dir'.
The additional markup used in doom-style org documents causes rendering issues."
  (unless (dtm-org-limit-styling-p) (org-modern-mode +1)))

;;* Org-appear
(defun dtm-org-pretty-use-appear-a ()
  "Activate `org-appear-mode' based on `org-pretty-entities'.
Intended as after advice for `org-toggle-pretty-entities'."
  (org-appear-mode (if org-pretty-entities +1 -1)))

;;* Org-download
(defun dtm-org-download-file-format (filename)
  "Prefix FILENAME with `buffer-file-name' and `org-download-timestamp'."
  (unless (buffer-file-name)
    (user-error "No file on disk, save the current buffer first"))
  (format "%s%s--%s"
          (file-name-base (buffer-file-name))
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

(defun dtm/ess-lookup-documentation ()
  "Wrapper for `ess-display-help-on-object' to improve `+lookup/documentation'.
Bypasses `ess-completing-read', indicates if process is busy."
  (interactive)
  (ess-make-buffer-current)
  (let ((obj (ess-helpobjs-at-point--read-obj)))
    (condition-case err
        (ess-display-help-on-object obj)
      (user-error (progn
                    (message "%s" (error-message-string err))
                    'deferred)))))

(defun dtm-ess-plot-file-p (file)
  "Return non-nil if FILE is an ESS plot."
  (when (fboundp 'ess-plot-file-p)
    (ess-plot-file-p file)))

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
This enables the each word of the query to be on a consecutive non-blank line."
  (string-join (mapcar #'regexp-quote (ctrlf-split-fuzzy input))  ".*?\\(?:\n.*?\\)??"))

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

;;* Pixel-scroll-precision-mode
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

(defun dtm-precision-scroll-window-fraction (fraction)
  "Scroll window by FRACTION of total height."
  (let ((delta (* fraction (dtm-window-usable-height)))
        (pixel-scroll-precision-interpolation-total-time 0.25))
    (pixel-scroll-precision-interpolate delta nil 1)))

(defun dtm-precision-scroll-up-half ()
  "Scroll up half a window, obeying `pixel-scroll-precision-mode'."
  (interactive)
  (if pixel-scroll-precision-mode
      (dtm-precision-scroll-window-fraction 0.49)
    (call-interactively #'evil-scroll-up)))

(defun dtm-precision-scroll-down-half ()
  "Scroll down half a window, obeying `pixel-scroll-precision-mode'."
  (interactive)
  (if pixel-scroll-precision-mode
      (dtm-precision-scroll-window-fraction -0.49)
    (call-interactively #'evil-scroll-down)))

;;* GPTEL
(defvar dtm-gptel-dir nil
  "Directory for storing chats.")

(defun dtm-gptel-setup-h ()
  "Personal gptel-mode customisation's. Intended for `gptel-mode-hook'."
  (general-evil-define-key '(n i) 'local
    [C-return] #'dtm/gptel-send-buffer)
  (setq default-directory (or dtm-gptel-dir default-directory))
  (visual-line-mode 1)
  (flycheck-mode 0)
  (evil-insert-state))

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
      (+zen-light-toggle 1)
      (+word-wrap-mode 1))))

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
