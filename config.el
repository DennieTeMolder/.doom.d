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

;; Settings use by `my/recommend-theme' to determine theme
(defvar my-first-hour-of-day 8)
(defvar my-last-hour-of-day 17)
(defvar my-day-theme 'doom-one-light)
(defvar my-night-theme 'doom-vibrant)
(defvar my-presentation-theme 'doom-ayu-light)
(defvar my-solarized-theme 'doom-flatwhite)
(defvar my-dark-theme 'doom-monokai-ristretto)

;; Load theme based on custom function
(setq doom-flatwhite-no-highlight-variables t
      doom-theme (my-recommend-theme))

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

;; Increase horizontal scroll (shift + mwheel) sensitivity
(setq mouse-wheel-scroll-amount-horizontal 12)

;;;; UI Settings
;; Maximise emacs if specified in shell ENV
(when MAXIMIZE
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Display battery status on laptops
(use-package! battery
  :if IS-LAPTOP
  :config
  (display-battery-mode +1))

;; Only display encoding in modeline when it's not UTF-8
(add-hook! 'after-change-major-mode-hook #'my-doom-modeline-conditional-buffer-encoding)

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

;; Replace the default doom splash screen with amore subtle one
(setq +doom-dashboard-ascii-banner-fn #'my-doom-ascii-banner-fn)

;; Customize dashboard menu options to include org roam
(setq +doom-dashboard-menu-sections
      '(("Restore previous session" :icon
         (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
         :when (file-exists-p (doom-session-file))
         :face (:inherit (doom-dashboard-menu-title bold))
         :action my/load-session)
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

;;;; General Doom Settings
;; Default major mode for scratch buffer
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; Disable visual line mode as it can be expensive on long lines
(remove-hook! 'text-mode-hook #'visual-line-mode)

;; Automatically load changes (should mostly be in log files)
(add-hook! 'text-mode-hook (auto-revert-mode +1))

;; Disable global hl-line-mode
(remove-hook! 'doom-first-buffer-hook #'global-hl-line-mode)

;; Don't hide mode line when outside of popup
(remove-hook! '(completion-list-mode-hook Man-mode-hook)
              #'hide-mode-line-mode)

;; Search options for "SPC s o" (`+lookup/online')
(setq +lookup-provider-url-alist
      '(("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
        ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
        ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
        ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
        ("Arch wiki"         "https://wiki.archlinux.org/index.php?search=%s")
        ("Doom Emacs issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
        ("Ubuntu packages"   "https://packages.ubuntu.com/search?suite=focal&arch=arm64&keywords=%s")
        ("Manjaro packages"  "https://packages.manjaro.org/?query=%s")
        ("AUR"               "https://aur.archlinux.org/packages?O=0&K=%s")
        ("Anaconda packages" "https://anaconda.org/search?q=%s")))

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
  (setq projectile-sort-order 'recently-active
        ;; Replace the doom-project-ignored-p function to ignore remote projects
        projectile-ignored-project-function #'my-project-ignored-p)

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
  ;;Exclude non-existent & remote files from recent files list after cleanup
  (setq recentf-keep '(my-file-local-readable-p))

  ;; Revert back to running cleanup on mode start instead of emacs shutdown
  (remove-hook! 'kill-emacs-hook #'recentf-cleanup)
  (setq recentf-auto-cleanup 'mode)

  ;; Exclude autosave file/folder and root from recent files
  (add-to-list 'recentf-exclude "/autosave/?\\'")
  (add-to-list 'recentf-exclude "\\`/\\'"))

(after! persp-mode
  ;; Open private config files in a dedicated workspace
  (dolist (symbol '(doom/open-private-config
                    doom/find-file-in-private-config
                    doom/goto-private-init-file
                    doom/goto-private-config-file
                    doom/goto-private-packages-file))
    (advice-add symbol :before #'my/doom-private-goto-workspace))

  ;; Fix default input value for `doom/load-session'
  (global-set-key [remap doom/load-session] #'my/load-session))

;; Use ediff in dired instead of diff
(after! dired
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  (define-key dired-mode-map [remap dired-diff] #'my/dired-ediff))

(after! ranger
  (define-key ranger-mode-map [remap dired-diff] #'my/dired-ediff))

(after! undo-fu
  ;; Raise undo limit do 10 Mb (doom default: 40kb)
  (setq undo-limit 10000000))

(after! which-key
  (setq which-key-ellipsis ".."))

(after! all-the-icons
  ;; A lower scaling factor works better with the Iosevka font
  ;; Ref: https://github.com/doomemacs/doomemacs/issues/2967
  (setq all-the-icons-scale-factor 1.1))

(after! avy
  ;; Make "g s s" search al windows (C-u to limit to current)
  (setq avy-all-windows t
        avy-all-windows-alt nil))

;;;; Doom core package extentions
;; Add colours to info pages to make them more readable
(use-package! info-colors
    :hook (Info-selection . info-colors-fontify-node))

;; On-demand listing of directory sizes in dired
(use-package! dired-du
  :after ranger
  :bind (:map ranger-mode-map ("d u" . dired-du-mode))
  :config
  (setq dired-du-size-format t))

;; Enable vertico mouse extension (included with vertico)
(use-package! vertico-mouse
  :after vertico
  :config (vertico-mouse-mode +1))

;; Manage trash folder inside emacs
(use-package! trashed
  :commands trashed)

;; Reverse `rx' operation, for turning regex into lisp
(use-package! xr
  :commands xr)

;;;; Writing/Organization Tools
;; Spell checking
(after! ispell
  ;; Global and personal ispell library
  (setq ispell-dictionary "en_GB"
        ispell-personal-dictionary "~/Nextcloud/Dictionary/personal_dict.pws"))

;; Org-mode settings
(defvar my-org-line-spacing 0.1
  "`line-spacing' used in `org-mode'.
Also used by `org-modern-mode' to calculate heights.")

(after! org
  (setq org-ellipsis " ▾"
        org-indent-indentation-per-level 1
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
        org-use-property-inheritance t ; can cause slowdown when searching
        org-image-actual-width '(800) ; default if not ATTR is provided
        org-agenda-start-day nil
        org-agenda-span 14
        org-agenda-time-grid '((daily today require-timed)
                               (759 1159 1300 1700)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "⭠ now ─────────")

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

  ;; Enable hard wrapping and automate paragraph filling
  ;; Allow for double quoting using '' and `` (`` -> “)
  (add-hook! 'org-mode-hook #'my-org-mode-setup-h)

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

(use-package! org-appear
  :commands org-appear-mode
  :init
  (advice-add 'org-toggle-pretty-entities :after #'my-org-pretty-use-appear-a)
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autoemphasis t
        org-appear-autoentities t))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :config
  (setq org-modern-label-border my-org-line-spacing
        org-modern-statistics nil
        org-modern-table nil ; Ref: https://github.com/minad/org-modern/issues/69
        org-modern-star ["●" "◉" "○" "◉" "○" "◉" "○" "◉"]
        org-modern-list '((?+ . "›")
                          (?- . "‒")
                          (?* . "•")))

  ;; Ensure symbols do not change when switching fonts
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  ;; Correct indentation of headings
  (advice-add 'org-indent--compute-prefixes :after #'my-org--modern-indent-heading)

  ;; Refresh `org-indent-mode' to apply advice
  (when org-indent-mode
    (org-indent-mode +1)))

(after! org-tree-slide
  (setq +org-present-text-scale 6)

  ;; Make presentations even prettier
  (add-hook! 'org-tree-slide-mode-hook :append #'my-org-tree-slide-setup-h)

  ;; Disable `flycheck-mode' and `spell-fu-mode' when presenting
  (advice-add 'org-tree-slide-mode :around #'my-org-tree-slide-no-squiggles-a)

  (map! :map org-tree-slide-mode-map
        :gn "q" (cmd! (org-tree-slide-mode -1))
        :gn [left] #'org-tree-slide-move-previous-tree
        :gn [right] #'org-tree-slide-move-next-tree
        :gn [C-up] #'org-tree-slide-content))

;; Org-download settings
(after! org-download
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

  (defvar my-org-roam-index-file "pages/contents.org"))

(after! org-roam
  ;; Disable completion everywhere as it overrides company completion
  (setq org-roam-completion-everywhere nil)

  ;; Custom org-roam buffer preview function
  (setq org-roam-preview-function #'my-org-element-at-point-get-content)

  ;; Automatically update the slug in the filename when #+title: has changed.
  (add-hook 'org-roam-find-file-hook #'hlissner-org-roam-update-slug-on-save-h)

  ;; Make the backlinks buffer easier to peruse by folding leaves by default.
  (add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)

  ;; Add ID, Type, Tags, and Aliases to top of backlinks buffer.
  (advice-add #'org-roam-buffer-set-header-line-format :after #'hlissner-org-roam-add-preamble-a)

  ;; Open all roam buffers in a dedicated workspace
  (dolist (symbol '(org-roam-node-find
                    org-roam-node-random
                    org-roam-ref-find
                    org-roam-dailies--capture
                    org-roam-buffer-display-dedicated
                    org-roam-buffer-toggle))
    (advice-add symbol :before #'my-org-roam-goto-workspace))

  ;; Sync org-agenda with org-roam dailies
  (advice-add 'org-agenda :before #'my-org-roam-dailies-sync-agenda)

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

  (map! (:map pdf-view-mode-map
         :gn "C-e" #'pdf-view-scroll-down-or-previous-page
         :gn "S"   #'my/pdf-view-fit-half-height
         :gn "s r" #'image-rotate
         :v  "h"   #'pdf-annot-add-highlight-markup-annotation
         :v  "s"   #'pdf-annot-add-strikeout-markup-annotation
         :v  "u"   #'pdf-annot-add-underline-markup-annotation
         (:prefix "s"
          :desc "Slice original"     :gn "o" #'pdf-view-reset-slice
          :desc "Slice bounding box" :gn "b" #'pdf-view-set-slice-from-bounding-box
          :desc "Slice using mouse"  :gn "m" #'pdf-view-set-slice-using-mouse)
         (:prefix "C-c"
          :desc "Add Note"          "a" #'pdf-annot-add-text-annotation
          :desc "Delete Annotation" "d" #'pdf-annot-delete))

        (:map pdf-history-minor-mode-map
         :gn "<tab>"     #'pdf-history-backward
         :gn "<backtab>" #'pdf-history-forward
         :gn [mouse-8]   #'pdf-history-backward
         :gn [mouse-9]   #'pdf-history-forward)))

;; LaTeX settings
(after! tex-mode
  (setq TeX-PDF-mode t
        TeX-source-correlate-start-server t
        +latex-viewers '(pdf-tools))

  ;; Compatibility with multi-file documents
  (setq-default TeX-master nil))

;;;; Programming Languages
;;; Treat sub-words in camelCased words as individual text objects
(after! prog-mode
  (add-hook! 'prog-mode-hook #'subword-mode))

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

  ;; Fix evil cursor getting out of sync
  (advice-add 'vterm--redraw :after #'tiku91/vterm-redraw-cursor))

(after! sh-script
  (map! :map sh-mode-map
        :nv [C-return] #'thegodzeye/vterm-execute-current-line))

;; Proper number highlighting for R mode
(after! highlight-numbers
  (let ((expr "\\_<[0-9]*\\(?:\\.[0-9]+\\)?\\(?:[eE]-?[0-9]+\\)?\\_>"))
    (puthash 'ess-r-mode expr highlight-numbers-modelist)
    (puthash 'python-mode expr highlight-numbers-modelist)))

(after! ess
  ;; Use current dir for session
  (setq ess-ask-for-ess-directory nil
        ess-style 'RStudio)

  ;; Add company-R-library backend, currently part of this pull request:
  ;; https://github.com/doomemacs/doomemacs/pull/6421/commits/16b4023f0d1e97ff32da581546360770dff800f6
  (set-company-backend! 'ess-r-mode
    '(company-R-args company-R-objects company-R-library company-dabbrev-code :separate))

  ;; Enable additional highlighting
  (add-to-list 'ess-R-font-lock-keywords '(ess-R-fl-keyword:F&T . t))
  (add-to-list 'ess-R-font-lock-keywords '(ess-fl-keyword:fun-calls . t))
  ;; Customize type faces (used for F&T color)
  (custom-set-faces! '(ess-constant-face :weight bold :inherit font-lock-warning-face))

  ;; Make inferior buffer not take focus on startup
  (advice-add 'ess-switch-to-inferior-or-script-buffer :around #'my-ess-switch-maybe-a)

  ;; Make evil tab width same as ESS offset
  (add-hook! 'ess-mode-hook (setq-local evil-shift-width 'ess-indent-offset))

  (add-hook! 'inferior-ess-r-mode-hook (visual-line-mode +1))

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

  (advice-add '+python-init-anaconda-mode-maybe-h :before-until #'my-remote-buffer-p)

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
  :commands interaction-log-mode)

;; Smooth scrolling
(use-package! good-scroll
  :commands good-scroll-mode
  :config
  ;; Increase animation time and mouse scrolling sensitivity
  (setq good-scroll-duration .25
        good-scroll-algorithm 'good-scroll-linear
        good-scroll-step (round (/ (display-pixel-height) 5)))

  ;; Override evil functions on mode activation, undo upon deactivation
  (add-hook! 'good-scroll-mode-hook #'my-good-scroll-evil-override-h))

;; Init good-scroll
(unless IS-LAPTOP
  (good-scroll-mode +1))

;; Package for interacting with text fields, requires GhostText browser extension
(use-package! atomic-chrome
  :commands atomic-chrome-start-server
  :config
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("overleaf\\.com" . latex-mode)
          ("azuredatabricks\\.net" . python-mode))))

(use-package! vundo
  :commands vundo
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

(load! "+keybindings")
