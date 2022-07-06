;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;;; Flags
;; Determine if running on a laptop based on env variable (must be set by user)
(defvar IS-LAPTOP (string= "yes" (getenv "IS_LAPTOP")))
(defvar MAXIMIZE (string= "yes" (getenv "MAXIMIZE_EMACS")))

;;;; Doom preamble
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name nil
      user-mail-address nil)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(defvar my-base-font-size (if (and (<= (display-pixel-height) 1080)
                                   (not IS-LAPTOP))
                              13.0 14.0))

;; Use float for size as it indicates point size rather then pixels (better scaling)
(setq doom-font (font-spec :family "Iosevka" :width 'expanded :size my-base-font-size)
      doom-big-font (font-spec :family "Iosevka" :width 'expanded :size (+ my-base-font-size 5))
      doom-serif-font (font-spec :family "Iosevka Slab" :width 'expanded :size my-base-font-size)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size my-base-font-size))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Set theme based on time
(setq doom-theme
      (let ((light-theme 'doom-flatwhite)
            (dark-theme 'doom-vibrant)
            (start-time-light-theme 8)
            (end-time-light-theme 17)
            (hour (string-to-number (substring (current-time-string) 11 13))))
        (if (member hour (number-sequence start-time-light-theme end-time-light-theme))
            light-theme
          dark-theme))
      doom-flatwhite-no-highlight-variables t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (concat doom-etc-dir "org/"))

;; Make sure the org dir exists (important for id hash-tables)
(unless (file-exists-p org-directory)
  (make-directory org-directory t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;;; Basic Settings
;; Quit without confirmation
(setq confirm-kill-emacs nil)

;; Rudimentary settings
(setq-default delete-by-moving-to-trash t
              standard-indent 2
              tab-width 2
              text-scale-mode-step 1.1
              uniquify-buffer-name-style 'forward
              x-stretch-cursor t) ; stretch cursor to glyph size

;; Save clipboard to kill ring before deleting text
;; Cyle kill ring using <C-p> or <C-n> after pasting
(setq save-interprogram-paste-before-kill t)

;;;; UI Settings
;; Maximise emacs if specified in shell ENV
(when MAXIMIZE
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(add-hook! 'after-change-major-mode-hook
  (defun my-doom-modeline-conditional-buffer-encoding ()
    "Only display encoding in modeline when it's not UTF-8"
    (setq-local doom-modeline-buffer-encoding
                (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                            (eq buffer-file-coding-system 'utf-8))))))

;; Simplify window title and give a visual indication if file is edited
(setq frame-title-format
    '(""
      (:eval
       (if (s-contains-p (abbreviate-file-name (concat org-roam-directory "pages"))
                         (or buffer-file-truename ""))
           (replace-regexp-in-string ".*/[0-9]*-?" ">" buffer-file-name)
         "%b"))
      (:eval
       (if (buffer-modified-p) " +" ""))))

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

;; Replace the default doom splash screen with amore subtle one
(setq +doom-dashboard-ascii-banner-fn #'my-doom-ascii-banner-fn)

;; Customize dashboard menu options to include org roam
(setq +doom-dashboard-menu-sections
      '(("Reload last session" :icon
         (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
         :when
         (cond
          ((featurep! :ui workspaces)
           (file-exists-p
            (expand-file-name persp-auto-save-fname persp-save-dir)))
          ((require 'desktop nil t)
           (file-exists-p
            (desktop-full-file-name))))
         :face
         (:inherit
          (doom-dashboard-menu-title bold))
         :action doom/quickload-session)
        ("Recently opened files" :icon
         (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("Open roam index" :icon
         (all-the-icons-octicon "database" :face 'doom-dashboard-menu-title)
         :action my/org-roam-open-index)
        ("Open roam today" :icon
         (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
         :action org-roam-dailies-goto-today)
        ("Open project" :icon
         (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("Open private configuration" :icon
         (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
         :when
         (file-directory-p doom-private-dir)
         :action doom/open-private-config)
        ("Open documentation" :icon
         (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
         :action doom/help)))

;;;; General Doom Settings/Bindings
;; Default major mode for scratch buffer
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; Disable visual line mode as it can be expensive on long lines
(remove-hook! 'text-mode-hook #'visual-line-mode)

;; Disable global hl-line-mode
(remove-hook! 'doom-first-buffer-hook #'global-hl-line-mode)

;; Don't hide mode line when outside of popup
(remove-hook! '(completion-list-mode-hook Man-mode-hook)
              #'hide-mode-line-mode)

;; Spacemacs style M-x
;; Old SPC SPC binding (projectile find file) also available under "SPC p f"
;; This frees up the "SPC :" to be another evil-ex because i am condition to hit SPC
(map! :leader
      :desc "M-x" "SPC" #'execute-extended-command
      :desc "Evil ex command" ":" #'evil-ex)

;; Global keybindings
(map! "C-s" #'isearch-forward-word
      "C-l" #'+nav-flash/blink-cursor
      (:leader
       :desc "Repeat last command" "r" #'repeat
       "b D" #'kill-buffer-and-window))

;; Make "Z" bindings only kill buffers not the session
(map! :n "ZQ" #'kill-buffer-and-window
      :n "ZZ" #'doom/save-and-kill-buffer

      (:map with-editor-mode-map
       :n "ZQ" #'with-editor-cancel
       :n "ZZ" #'with-editor-finish))

;; Use mouse buttons to go forward/backward trough window configs
(map! :n [mouse-8] #'winner-undo
      :n [mouse-9] #'winner-redo

      (:map Info-mode-map
       :n [mouse-8] #'Info-history-back
       :n [mouse-9] #'Info-history-forward))

;; Window management functions/bindings
(defun my/window-double-height ()
  "Double height of active window"
  (interactive)
  (enlarge-window (window-height)))

(defun my/window-half-height ()
  "Halves height of active window"
  (interactive)
  (enlarge-window (/ (window-height) -2)))

(map! :leader :prefix "w"
      :desc "Adjust windows hydra" "a" #'+hydra/window-nav/body
      :desc "Enlarge double height" "e" #'my/window-double-height
      :desc "Halve height" "E" #'my/window-half-height)

;; Extra toggle functions/bindings
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

(map! :leader :prefix "t"
      :desc "Trash deleted files" "T" #'my/toggle-trash-delete
      :desc "Auto linebreaks" "a" #'auto-fill-mode)

;; Increase horizontal scroll (shift + mwheel) sensitivity
(setq mouse-wheel-scroll-amount-horizontal 12)

;; Enable vertico mouse extension (included with vertico)
(use-package! vertico-mouse
  :after vertico
  :config (vertico-mouse-mode +1))

;;;; Doom Core Package Settings
(after! evil
  ;; Indicate `evil-repeat' to ignore certain commands because they freeze emacs
  (evil-add-command-properties '+workspace/switch-left :repeat nil)
  (evil-add-command-properties '+workspace/switch-right :repeat nil)

  ;; Enable granular undo (remembers delete actions during insert state)
  (setq evil-want-fine-undo t
        evil-vsplit-window-right t
        evil-split-window-below t))

(after! evil-snipe
  ;; Make snipe commands (bound to f,F,t,T,s,S) go beyond the current line
  (setq evil-snipe-scope 'visible))

(after! company
  ;; Disable company auto pop-up as it can be expensive, use C-SPC to trigger
  (setq company-idle-delay nil
        company-selection-wrap-around t)

  ;; Enable in elisp mode at is not as expensive
  (add-hook! 'emacs-lisp-mode-hook (setq-local company-idle-delay 0.2)))

(after! projectile
  ;; Projectle sorting by recently opened
  (setq projectile-sort-order 'recently-active)

  (defun my-project-ignored-p (project-root)
    "Return non-nil if remote or temporary file or a straight package."
    (or (file-remote-p project-root)
        (file-in-directory-p project-root temporary-file-directory)
        (file-in-directory-p project-root doom-local-dir)))

  ;; Replace the doom-project-ignored-p function to ignore remote projects
  (setq projectile-ignored-project-function #'my-project-ignored-p)

  ;; Define a generic project as .projectile is not synced by Nextcloud
  (projectile-register-project-type 'generic '("PROJECT") :project-file "PROJECT")

  ;; Append the project name to the title frame format
  (add-to-list 'frame-title-format
               '(:eval
                 (let* ((project-name (projectile-project-name))
                        (project-name (if (string= "-" project-name)
                                          "Emacs"
                                        project-name)))
                   (concat " | " project-name)))
               t))

(after! recentf
  (defun my-recentf-keep-p (file)
    "Return non-nil if FILE should be kept in the recent list."
    (unless (file-remote-p file)
      (file-readable-p file)))

  ;;Exclude non-existent & remote files from recent files list after cleanup
  (setq recentf-keep '(my-recentf-keep-p))

  ;; Revert back to running cleanup on mode start instead of emacs shutdown
  (remove-hook! 'kill-emacs-hook #'recentf-cleanup)
  (setq recentf-auto-cleanup 'mode)

  ;; Exclude autosave file/folder and root from recent files
  (add-to-list 'recentf-exclude "/autosave/?\\'")
  (add-to-list 'recentf-exclude "\\`/\\'"))

(after! persp-mode
  (defun my-workspace-switch-maybe (name)
    "Switch to workspace NAME if not already current"
    (unless (equal name (+workspace-current-name))
      (+workspace-switch name t)
      (+workspace/display)))

  ;; Open private config files in a dedicated workspace
  (defun my/doom-private-goto-workspace ()
    "Open/create the dedicated private config workspace"
    (my-workspace-switch-maybe "*config*"))

  (dolist (symbol '(doom/open-private-config
                    doom/find-file-in-private-config
                    doom/goto-private-init-file
                    doom/goto-private-config-file
                    doom/goto-private-packages-file))
    (advice-add symbol :before #'my/doom-private-goto-workspace))

  ;; Functions and binding to move buffers between workspaces
  (defun my-buffer-move-to-workspace (buffer name)
    "Move BUFFER from the original workspace to NAME and switch"
    (let ((origin (+workspace-current-name)))
      (+workspace-switch name t)
      (persp-add-buffer buffer (+workspace-get name) t nil)
      (+workspace-switch origin)
      (persp-remove-buffer buffer (+workspace-get origin) nil t nil nil)
      (+workspace-switch name)
      (+workspace/display)))

  (defun my/buffer-move-to-workspace-prompt ()
    "Move current buffer from the current to the selected workspace"
    (interactive)
    (let ((buffer (current-buffer))
          (name (completing-read
                   "Move current buffer to workspace:"
                   (+workspace-list-names))))
      (my-buffer-move-to-workspace buffer name)))

  (map! :leader
        :desc "Move buffer to workspace" "b TAB" #'my/buffer-move-to-workspace-prompt))

(after! dired
  (defun my/dired-ediff ()
    "Compare file under cursor to file selected in prompt using Ediff"
    (interactive)
    (let* ((file (dired-get-filename t))
           (dir (dired-current-directory))
           (default nil)
           (target (read-file-name (format-prompt "Diff %s with" default file)
                                   default nil t)))
      (ediff (expand-file-name file dir) target)))

  (map! :map (dired-mode-map ranger-mode-map) [remap dired-diff] #'my/dired-ediff))

(after! undo-fu
  ;; Raise undo limit do 10 Mb (doom default: 40kb)
  (setq undo-limit 10000000))

(use-package! battery
  :if IS-LAPTOP
  :config
  (display-battery-mode +1))

(after! which-key
  (setq which-key-idle-delay 0.5
        which-key-ellipsis ".."))

(after! all-the-icons
  ;; A lower scaling factor works better with the Iosevka font
  ;; Ref: https://github.com/doomemacs/doomemacs/issues/2967
  (setq all-the-icons-scale-factor 1.1))

;;;; Writing/Organization Tools
;; Spell checking
(after! ispell
  ;; Global and personal ispell library
  (setq ispell-dictionary "en_GB"
        ispell-personal-dictionary "~/Nextcloud/Dictionary/personal_dict.pws"))

;; Org-mode settings
(after! org
  (setq org-ellipsis " ▾"
        org-indent-indentation-per-level 1
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
        org-agenda-start-day nil
        org-agenda-span 14
        org-agenda-time-grid '((daily today require-timed)
                               (759 1159 1300 1700)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "⭠ now ─────────")

  (defvar my-org-line-spacing 0.1)

  ;; Make headings bold and larger
  (custom-set-faces!
    '((org-document-title outline-1 outline-2 outline-3 outline-4 outline-5
       outline-6 outline-7 outline-8)
      :weight semi-bold)
    '(org-document-title :height 1.3)
    '(outline-1 :height 1.2)
    '(outline-2 :height 1.1)
    '(outline-3 :height 1.05))

  ;; Give ellipsis same color as text
  (custom-set-faces!
    '(org-ellipsis :foreground nil :background nil :weight regular)
    '(org-headline-done :strike-through t))

  (defun my-insert-exit-fill-paragraph ()
    "Perform `org-fill-paragraph' after some contextual checks"
      ;; Check if `auto-fill-mode' is active
      (when auto-fill-function
        (unless (eq (org-element-type (org-element-at-point))
                    'src-block)
          (org-fill-paragraph))))

  ;; Allow for double quoting using '' and `` (`` -> “)
  (add-hook! 'org-mode-hook
    (defun my-org-mode-hook ()
      "Personal org-mode customisation's after mode startup"
      (setq-local line-spacing my-org-line-spacing
                  auto-hscroll-mode nil)
      (electric-quote-local-mode +1)
      (visual-line-mode -1)
      (auto-fill-mode +1)
      (add-hook! 'evil-insert-state-exit-hook
                 :local #'my-insert-exit-fill-paragraph)))

  ;; Use old org-ref insert key, remove `org-agenda-file-to-front' binding
  (map! :map org-mode-map
        "C-c ]" #'org-cite-insert
        "C-c [" nil
        :nv "C-j" #'+org/return
        :desc "Toggle pretty visuals" :localleader "v" #'+org-pretty-mode))

;; Org-cite settings
(after! oc
  ;; according to the `oc-biblatex.el' you should use bibstyle/citestyle
  (setq org-cite-csl-styles-dir "~/Nextcloud/Zotero/Styles"
        org-cite-export-processors '((latex biblatex "ieee/numeric-comp")
                                     (t csl "ieee.csl"))))

(after! ox-odt
  ;; Ensure latest styles are used for ODT export
  (setq org-odt-styles-dir (expand-file-name "straight/repos/org/etc/styles" doom-local-dir)
        org-odt-preferred-output-format "doc"))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :init
  (defadvice! my-org-indent-modern-heading ()
    "Correctly indents heading assuming leading stars are fully hidden (not invisible)."
    :after #'org-indent--compute-prefixes
    (dotimes (n org-indent--deepest-level)
      (unless (= n 0)
        (let* ((indentation (* org-indent-indentation-per-level (1- n)))
               (heading-prefix (make-string indentation ?\s)))
          (aset org-indent--heading-line-prefixes
                n
                (org-add-props heading-prefix nil 'face 'org-indent))))))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  (setq org-modern-label-border my-org-line-spacing
        org-modern-statistics nil
        org-modern-table nil ;; Ref: https://github.com/minad/org-modern/issues/69
        org-modern-star ["●" "◉" "○" "◉" "○" "◉" "○" "◉"]
        org-modern-list '((?+ . 8226)
                          (?- . 8250)
                          (?* . 8208))))

(after! org-tree-slide
  (setq +org-present-text-scale 8
        org-tree-slide-fold-subtrees-skipped nil)

  (add-hook! 'org-tree-slide-mode-hook
    (defun my-org-tree-slide-present-toggles ()
      "Toggles for several settings to prettify presentations"
      (if org-tree-slide-mode
          (progn
            (setq-local display-line-numbers nil
                        buffer-read-only t
                        evil-normal-state-cursor 'hbar)
            (when hl-line-mode (hl-line-mode -1))
            (mixed-pitch-mode +1)
            (add-hook! 'pdf-view-mode-hook :append #'org-tree-slide-mode))
        (progn
          (setq-local buffer-read-only nil)
          (mixed-pitch-mode -1)
          (remove-hook! 'pdf-view-mode-hook #'org-tree-slide-mode)))))

  (map! :map org-tree-slide-mode-map
        :gn [left] #'org-tree-slide-move-previous-tree
        :gn [right] #'org-tree-slide-move-next-tree
        :gn [C-up] #'org-tree-slide-content))

;; Org-download settings
(after! org-download
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

  (setq org-download-method 'drestivo/org-download-method
        org-download-link-format "[[file:%s]]\n"))

;; Org-pomodoro settings
(after! org-pomodoro
  (setq org-pomodoro-manual-break t
        org-pomodoro-keep-killed-pomodoro-time t))

;; Org-roam init settings
(when (featurep! :lang org +roam2)
  (setq org-roam-directory "~/Nextcloud/PKM/"
        org-roam-dailies-directory "journals/"
        org-roam-file-exclude-regexp "Rubbish/")

  (defvar my-org-roam-index-file "pages/contents.org")

  (defun my-org-roam-goto-workspace (&rest _)
    "Open/create the dedicated org-roam workspace"
    (my-workspace-switch-maybe "*roam*"))

  (defun my/org-roam-open-index ()
    "Opens the file specified in my-org-roam-index-file"
    (interactive)
    (my-org-roam-goto-workspace)
    (find-file (expand-file-name my-org-roam-index-file org-roam-directory)))

  ;; Map to keybinding
  (map! :desc "Open index" :leader "n r o" #'my/org-roam-open-index))

(after! org-roam
  ;; Disable completion everywhere as it overrides company completion
  (setq org-roam-completion-everywhere nil)

  ;; Custom org-roam buffer preview function
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

  (setq org-roam-preview-function #'my-org-element-at-point-get-content)

  ;; Open all roam buffers in a dedicated workspace
  (dolist (symbol '(org-roam-node-find
                    org-roam-node-random
                    org-roam-ref-find
                    org-roam-dailies--capture
                    org-roam-buffer-display-dedicated
                    org-roam-buffer-toggle))
    (advice-add symbol :before #'my-org-roam-goto-workspace))

  ;; Roam templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t
           :empty-lines 1)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+date: %<%A %B %d, Week %W %Y>\n \n* Agenda\n")
           :empty-lines 1))))

(use-package! org-roam-dailies
  :after org-roam
  :config
  (defun my-org-roam-dailes-active-files ()
    "Return list of daily files corresponding to TODAY or later"
    (let ((files (org-roam-dailies--list-files))
          (today (calendar-absolute-from-gregorian (calendar-current-date))))
      (while
          (< (calendar-absolute-from-gregorian
              (org-roam-dailies-calendar--file-to-date (car files)))
             today)
        (pop files))
      files))

  (defadvice! my-org-roam-dailies-sync-agenda (&rest _)
    "Scan the dailies-directory and add current and future dates to agenda."
    :before #'org-agenda
    (mapc (lambda (x) (cl-pushnew x org-agenda-files :test #'string=))
          (my-org-roam-dailes-active-files)))

  (defun my-org-get-title-value ()
    "Returns the value of #+TITLE for the current document"
    (car (cdar (org-collect-keywords '("TITLE")))))

  ;; Nifty timeblock function
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

  (defun my/org-roam-dailies-schedule-time ()
    "Wrapper around `org-schedule' that only prompts for time.
The DATE is derived from the #+title which must match the Org date format."
    (interactive)
    (unless (org-roam-dailies--daily-note-p)
      (user-error "Not in a daily-note"))
    (let ((date (my-org-get-title-value))
          (time (read-string "Schedule headline at (HH:MM): ")))
      (org-schedule nil (concat date " " time (when (length< time 3) ":00")))))

  (map! :map org-roam-dailies-map :leader
        :desc "Schedule headline" "n r d s" #'my/org-roam-dailies-schedule-time))

(when (featurep! :tools biblio)
  ;; Citar bibliography settings
  (setq! citar-bibliography '("~/Nextcloud/Zotero/master.bib")
         citar-library-paths '("~/Nextcloud/Zotero/")
         citar-notes-paths '("~/Nextcloud/PKM/notes/")))

(after! citar
  ;; citar note template
  (push '(note . "${=key=}: ${title}\n\n* Notes") citar-templates)

  ;; Open notes in roam workspace
  (advice-add 'citar-open-notes :before #'my-org-roam-goto-workspace)

  ;; Update citar cache when bib-file changes during specified modes
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook))
  ;; Skip looking for local bibliographies if buffer has no file associated
  (advice-add 'citar-filenotify-local-watches :before-while #'buffer-file-name)

  ;; Disable citation delete binding
  (map! :map citar-org-citation-map "C-d" nil))

;; Org-noter settings
(after! org-noter
  (setq org-noter-hide-other nil ;; Don't fold headings when navigating
        org-noter-always-create-frame nil) ;; Only crete new frames for additional sessions

  ;; Kill session map in line with other C-c bound pdf controls for one hand use
  (map! :map (org-noter-doc-mode-map org-noter-notes-mode-map)
        "C-c q" #'org-noter-kill-session)

  ;; The pdf-view major mode overwrites the i binding with =ignore= for all minor modes
  ;; This works around that by incorporating the binding into the major mode
  (map! :map pdf-view-mode-map
        :desc "Insert org note" :n "i" (cmd! (if org-noter-doc-mode
                                                 (org-noter-insert-note)
                                               (ignore)))))

(after! pdf-tools
  ;; Distinguish current match
  (custom-set-faces! '(pdf-isearch-match :inherit highlight))

  (setq pdf-view-resize-factor 1.1)

  (add-to-list 'evil-snipe-disabled-modes 'pdf-view-mode)

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

  (map! (:map pdf-view-mode-map
         :gn "C-e" #'pdf-view-scroll-down-or-previous-page
         :gn "S" #'my/pdf-view-fit-half-height
         :gn "s r" #'image-rotate
         :desc "Slice original" :gn "s o" #'pdf-view-reset-slice
         :desc "Slice bounding box" :gn "s b" #'pdf-view-set-slice-from-bounding-box
         :desc "Slice using mouse" :gn "s m" #'pdf-view-set-slice-using-mouse
         :v "h" #'pdf-annot-add-highlight-markup-annotation
         :v "s" #'pdf-annot-add-strikeout-markup-annotation
         :v "u" #'pdf-annot-add-underline-markup-annotation
         (:prefix "C-c"
          :desc "Add Note" "a" #'pdf-annot-add-text-annotation
          :desc "Delete Annotation" "d" #'pdf-annot-delete))

        (:map pdf-history-minor-mode-map
         :gn "<tab>" #'pdf-history-backward
         :gn "<backtab>" #'pdf-history-forward
         :gn [mouse-8] #'pdf-history-backward
         :gn [mouse-9] #'pdf-history-forward)))

;; LaTeX settings
(after! tex-mode
  (setq TeX-PDF-mode t
        TeX-source-correlate-start-server t
        +latex-viewers '(pdf-tools))

  ;; Compatibility with multi-file documents
  (setq-default TeX-master nil))

;;;; Programming Languages
;; General interactive programming buffer settings
(after! comint
  (setq ansi-color-for-comint-mode 'filter
        comint-prompt-read-only t
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)

  ;; Shell style clear REPL binding
  (map! :map comint-mode-map
        "C-l" #'comint-clear-buffer))

(after! eshell
  (remove-hook! 'eshell-mode-hook #'hide-mode-line-mode))

(after! vterm
  ;; Actually clear buffer upon C-l
  (setq vterm-clear-scrollback-when-clearing t)

  (remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)

  (defadvice! tiku91/vterm-redraw-cursor (args)
    "Redraw evil cursor with vterm to keep it consistent with the current state.
Fix by tiku91:
https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-867525845"
    :after #'vterm--redraw
    (evil-refresh-cursor evil-state)))

(after! sh-script
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

  (map! :map sh-mode-map
        :nv [C-return] #'thegodzeye/vterm-execute-current-line))

;; Proper number highlighting for R mode
(after! highlight-numbers
  (let ((expr "\\_<[0-9]*\\(?:\\.[0-9]+\\)?\\(?:[eE]-?[0-9]+\\)?\\_>"))
    (puthash 'ess-r-mode expr highlight-numbers-modelist)
    (puthash 'python-mode expr highlight-numbers-modelist)))

(after! ess
  ;; Use current dir for session
  (setq ess-ask-for-ess-directory nil)

  ;; Add company-R-library backend, currently part of this pull request:
  ;; https://github.com/doomemacs/doomemacs/pull/6421/commits/16b4023f0d1e97ff32da581546360770dff800f6
  (set-company-backend! 'ess-r-mode
    '(company-R-args company-R-objects company-R-library company-dabbrev-code :separate))

  ;; Enable additional highlighting
  (add-to-list 'ess-R-font-lock-keywords '(ess-R-fl-keyword:F&T . t))
  (add-to-list 'ess-R-font-lock-keywords '(ess-fl-keyword:fun-calls . t))
  ;; Customize type faces (used for F&T color)
  (custom-set-faces! '(ess-constant-face :weight bold :inherit font-lock-warning-face))

  (defadvice! my/advice-ess-switch (orig-fn TOGGLE-EOB)
    "Only switch to the REPL if it was already visible"
    :around #'ess-switch-to-inferior-or-script-buffer
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

  ;; Make evil tab width same as ESS offset
  (add-hook! 'ess-mode-hook
             (setq-local evil-shift-width 'ess-indent-offset))

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

  (defun my/ess-r-insert-assign (arg)
    "Rewrite of `ess-insert-assign' that respects white space, invoke twice to undo"
    (interactive "p")
    (my-ess-insert-string " <- "))

  (defun my/ess-r-insert-pipe (arg)
    "Based on `ess-insert-assign', invoking the command twice reverts the insert"
    (interactive "p")
    (my-ess-insert-string " %>% "))

  ;; ESS R keybindings, make < add a <-, type twice to undo (same goes for >)
  (map! (:map ess-mode-map
         :nv [C-return] #'ess-eval-region-or-line-and-step
         :localleader
         :desc "Source current file" "s" #'ess-load-file
         "S" #'ess-switch-process)

        (:map inferior-ess-mode-map
         :localleader
         "TAB" #'ess-switch-to-inferior-or-script-buffer
         "x r" #'inferior-ess-reload)

        (:map ess-r-mode-map
         :localleader
         :desc "Eval reg|func|para" "e" #'ess-eval-region-or-function-or-paragraph
         :desc "Environment list R objects" "E" #'ess-rdired)

        (:map (ess-r-mode-map inferior-ess-r-mode-map)
         :i "<" #'my/ess-r-insert-assign
         :i ">" #'my/ess-r-insert-pipe)))

(after! ess-s-lang
  ;; Imenu search entries, best invoked with =consult-imenu= (SPC s i)
  (add-to-list 'ess-imenu-S-generic-expression
               '("Section" "^\\(#+ [^\n]+\\) ----+" 1)))

(use-package! ess-view-data
  :commands ess-view-data-print
  :init
  (map! :map ess-r-mode-map
        :desc "View R object" :localleader "o" #'ess-view-data-print))

(after! python
  ;; Python functions
  (defun my/python-shell-send-statment-and-step ()
    "Send statement to python shell and move to next"
    (interactive)
    (python-shell-send-region
     (save-excursion (python-nav-beginning-of-statement))
     (save-excursion (python-nav-end-of-statement)))
    (python-nav-forward-statement))

  (defun my/python-shell-send-block-and-step ()
    "Send block to python shell and move to next statement"
    (interactive)
    (python-shell-send-region
     (save-excursion (python-nav-beginning-of-block))
     (save-excursion (python-nav-end-of-block)))
    (python-nav-end-of-block)
    (python-nav-forward-statement))

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

  ;; Python keybindings
  (map! :map python-mode-map
        :nv [C-return] #'my/python-send-current-and-step
        :localleader
        :desc "Open python REPL" [tab] #'+python/open-ipython-repl
        :desc "Send buffer to REPL" "b" #'python-shell-send-buffer
        :desc "Send file to REPL" "f" #'python-shell-send-file))

;; Snakefiles in python mode
(add-to-list 'auto-mode-alist '("Snakefile" . python-mode))
(add-to-list 'auto-mode-alist '("\\.smk\\'" . python-mode))

(after! conda
  (custom-set-variables
   '(conda-anaconda-home "~/.local/miniconda3/"))

  (defadvice! my/anaconda-disable-on-remote ()
    "Only activate anaconda-mode if the buffer is local"
    :before-while #'+python-init-anaconda-mode-maybe-h
    (not (file-remote-p default-directory)))

  (defun my-conda-env-promt-activate (env-name)
    "If conda environment with ENV-NAME is not activated, prompt the user to do so"
    (if (and (not (equal env-name conda-env-current-name))
             (y-or-n-p (format "Activate conda env: %s?" env-name)))
      (conda-env-activate (conda-env-name-to-dir env-name))))

  (defun my/conda-env-guess-prompt ()
    "Guess the currently relevant conda env and prompt user to activate it"
    (interactive)
    (let ((candidate-env (conda--infer-env-from-buffer))
          (fallback-env "base"))
      (cond (candidate-env (my-conda-env-promt-activate candidate-env))
            ((not conda-env-current-name) (my-conda-env-promt-activate fallback-env)))))

  ;; Prompt user to change into a conda env
  (when (executable-find "conda")
      (add-hook! 'anaconda-mode-hook #'my/conda-env-guess-prompt))

  (map! :map (python-mode-map inferior-python-mode-map)
        :localleader :prefix ("c" . "Conda")
         :desc "Guess conda env" "g" #'my/conda-env-guess-prompt
         "a" #'conda-env-activate
         "d" #'conda-env-deactivate))

(after! csv-mode
  ;; Asume the first line of a csv is a header
  (setq csv-header-lines 1)

  ;; Ensure delimiters are not hidden when aligning
  (add-hook! 'csv-mode-hook
    (setq-local buffer-invisibility-spec nil)))

;;;; Misc Packages
;; M-x interaction-log-mode shows all executed command for debugging/showcasing
(use-package! interaction-log
  :commands interaction-log-mode
  :config
  ;; TODO prompt user to execute this function after interaction-log-mode
  (defun my/interaction-log-show ()
    "Creates an interaction log window if it doesn't exist"
    (interactive)
    (display-buffer ilog-buffer-name)))

;; Smooth scrolling
(use-package! good-scroll
  :unless IS-LAPTOP
  :config
  ;; Increase animation time and mouse scrolling sensitivity
  (setq good-scroll-duration .25
        good-scroll-algorithm 'good-scroll-linear
        good-scroll-step (round (/ (display-pixel-height) 5)))

  ;; binding to toggle good scroll mode
  (map! :desc "Smooth scrolling" :leader "t S" #'good-scroll-mode)

  ;; Evil scrolling
  (defun my/good-scroll-down-half ()
    (interactive)
    (good-scroll-move (/ (good-scroll--window-usable-height) 2)))

  (defun my/good-scroll-up-half ()
    (interactive)
    (good-scroll-move (/ (good-scroll--window-usable-height) -2)))


  ;; Override evil functions on mode activation, undo upon deactivation
  (add-hook! 'good-scroll-mode-hook
    (defun my-good-scroll-evil-override-hook ()
      (if good-scroll-mode
          (progn
            (advice-add 'evil-scroll-down :override #'my/good-scroll-down-half)
            (advice-add 'evil-scroll-up :override #'my/good-scroll-up-half))
        (progn
          (advice-remove 'evil-scroll-down #'my/good-scroll-down-half)
          (advice-remove 'evil-scroll-up #'my/good-scroll-up-half)))))

  ;; Enable good-scroll
  (good-scroll-mode +1))

;; Package for interacting with text fields, requires GhostText browser extension
(use-package! atomic-chrome
  :commands atomic-chrome-start-server
  :init
  ;; Function to make atomic chrome server toggleable
  (defun my/atomic-chrome-toggle-server ()
    (interactive)
    (if (bound-and-true-p global-atomic-chrome-edit-mode)
        (progn
          (atomic-chrome-stop-server)
          (message "Stopped GhostText Server"))
      (progn
        (atomic-chrome-start-server)
        (message "Started GhostText Server"))))

  ;; Bind toggle
  (map! :desc "GhostText server" :leader "t G" #'my/atomic-chrome-toggle-server)

  :config
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("overleaf\\.com" . latex-mode)
          ("azuredatabricks\\.net" . python-mode))))

(use-package! vundo
  :commands vundo
  :init
  (map! :desc "Show undo history" :leader "b h" #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (map! :map vundo-mode-map
        "h" #'vundo-backward
        "j" #'vundo-next
        "k" #'vundo-previous
        "l" #'vundo-forward
        "H" #'vundo-stem-root
        "L" #'vundo-stem-end))

(use-package! indent-tools
  :commands indent-tools-hydra/body
  :init
  (map! :desc "Indentation hydra" :leader ">" #'indent-tools-hydra/body)
  :config
  ;; Add potentially large movements to jump list
  (dolist (symbol '(indent-tools-goto-next-sibling
                    indent-tools-goto-previous-sibling
                    indent-tools-goto-parent
                    indent-tools-goto-parent
                    indent-tools-goto-child
                    indent-tools-goto-child
                    indent-tools-goto-end-of-tree))
    (advice-add symbol :around #'doom-set-jump-maybe-a)))

(use-package! trashed
  :commands trashed)

;; Reverse `rx' operation, for turning regex into lisp
(use-package! xr
  :commands xr)
