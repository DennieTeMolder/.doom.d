;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Note: custom functions/variables/macros are prefixed with 'dtm'

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;* Flags
;; Determine if running on a laptop based on env variable (must be set by user)
(defvar IS-LAPTOP (string= "yes" (getenv "IS_LAPTOP")))
(defvar FRAME-MAXIMIZE (string= "yes" (getenv "MAXIMIZE_EMACS")))

;;* Doom preamble
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Dennie te Molder"
      user-mail-address "john@doe.com")

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

(defvar dtm-base-font-size (if IS-LAPTOP 13.0 11.0))

;; Use float for size as it indicates point size rather then pixels (better scaling)
(setq doom-font (font-spec :family "Iosevka" :width 'expanded :size dtm-base-font-size)
      doom-big-font (font-spec :family "Iosevka" :width 'expanded :size (+ dtm-base-font-size 5))
      doom-serif-font (font-spec :family "Iosevka Slab" :width 'expanded :size dtm-base-font-size)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size dtm-base-font-size))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; Settings used by `dtm/recommend-theme' to determine theme
(defvar dtm-first-hour-of-day 8)
(defvar dtm-last-hour-of-day 17)
(defvar dtm-day-theme 'doom-one-light)
(defvar dtm-night-theme 'doom-vibrant)
(defvar dtm-presentation-theme 'doom-tomorrow-day)
(defvar dtm-solarized-theme 'doom-flatwhite)
(defvar dtm-dark-theme 'doom-monokai-ristretto)

;; Load theme based on custom function
(setq doom-flatwhite-no-highlight-variables t
      doom-theme (dtm-recommend-theme))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (concat doom-data-dir "org/"))

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


;;* Basic Settings
;; Quit without confirmation
(setq confirm-kill-emacs nil)

;; Rudimentary settings
(setq-default delete-by-moving-to-trash t
              tab-width 4
              text-scale-mode-step 1.1
              uniquify-buffer-name-style 'forward
              x-stretch-cursor t)

;; Save clipboard to kill ring before deleting text
;; Cycle kill ring using <C-p> or <C-n> after pasting
(setq save-interprogram-paste-before-kill t)

;; Increase horizontal scroll (shift + mwheel) sensitivity
(setq mouse-wheel-scroll-amount-horizontal 12)

;;* UI Settings
;; Maximise emacs if specified in shell ENV
(when FRAME-MAXIMIZE
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

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

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)

  ;; Only display encoding in modeline when it's not UTF-8
  (add-hook! 'after-change-major-mode-hook #'dtm-doom-modeline-conditional-buffer-encoding))

;; Display battery status on laptops
(use-package! battery
  :if IS-LAPTOP
  :config
  (display-battery-mode +1))

;; Replace the default doom splash screen with a more subtle one
(setq +doom-dashboard-ascii-banner-fn #'dtm-doom-ascii-banner-fn)

;; Customise dashboard menu options to include org roam
(setq +doom-dashboard-menu-sections
      '(("Restore previous session" :icon
         (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
         :when (file-exists-p (doom-session-file))
         :face (:inherit (doom-dashboard-menu-title bold))
         :action dtm/load-session)
        ("Recently opened files" :icon
         (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("Open roam index" :icon
         (all-the-icons-octicon "database" :face 'doom-dashboard-menu-title)
         :action dtm/org-roam-open-index)
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

;;* General Doom Settings
;; Default major mode for scratch buffer
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; Disable global hl-line-mode
(remove-hook! 'doom-first-buffer-hook #'global-hl-line-mode)

;; This prevents stepping into the left side window from the bottom side window
(advice-remove 'windmove-up #'+popup--ignore-window-parameters-a)

;; Do not scroll horizontally if auto-fill-mode is active
(add-hook! 'auto-fill-mode-hook (setq-local auto-hscroll-mode (not auto-fill-function)))

;; Search options for "SPC s o" (`+lookup/online')
(setq +lookup-provider-url-alist
      '(("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
        ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
        ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
        ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
        ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
        ("Arch wiki"         "https://wiki.archlinux.org/index.php?search=%s")
        ("Doom Emacs issues" "https://github.com/doomemacs/doomemacs/issues?q=is%%3Aissue+%s")
        ("Ubuntu packages"   "https://packages.ubuntu.com/search?suite=focal&arch=arm64&keywords=%s")
        ("Manjaro packages"  "https://packages.manjaro.org/?query=%s")
        ("AUR"               "https://aur.archlinux.org/packages?O=0&K=%s")
        ("Anaconda packages" "https://anaconda.org/search?q=%s")))

(after! text-mode
  ;; Disable visual line mode as it can be expensive on long lines
  (remove-hook! 'text-mode-hook #'visual-line-mode)

  ;; Automatically load changes (should mostly be in log files)
  (add-hook! 'text-mode-hook (auto-revert-mode +1)))

;;* Doom Core Package Settings
(after! evil
  ;; Indicate `evil-repeat' to ignore certain commands because they freeze emacs
  (dtm-evil-repeat-ignore '+workspace/switch-left '+workspace/switch-right)

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
        company-selection-wrap-around t
        company-dabbrev-ignore-case 'keep-prefix)

  ;; Enable auto pop-up in elisp mode as it is less expensive
  (add-hook! 'emacs-lisp-mode-hook (setq-local company-idle-delay 0.2))

  ;; Make dabbrev (C-x C-n) case sensitive in programming modes
  (add-hook! 'prog-mode-hook (setq-local company-dabbrev-ignore-case nil)))

(after! projectile
  ;; Projectle sorting by recently opened
  (setq projectile-sort-order 'recently-active
        ;; Replace the doom-project-ignored-p function to ignore remote projects
        projectile-ignored-project-function #'dtm-project-ignored-p)

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
  (setq recentf-keep '(dtm-file-local-readable-p))

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
    (advice-add symbol :before #'dtm-doom-private-goto-workspace))

  ;; Fix default input value for `doom/load-session'
  (global-set-key [remap doom/load-session] #'dtm/load-session))

(after! ibuffer
  ;; Ref: https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-mode-ibuffer-groups-el
  (unless (dtm-doctor-running-p)
    (define-ibuffer-filter persp
        "Toggle current view to buffers of current perspective."
      (:description "persp-mode"
       :reader (persp-read-persp nil nil (safe-persp-name (get-frame-persp)) t))
      (cl-find buf (safe-persp-buffers (persp-get-by-name qualifier)))))

  ;; Group buffers based on perspective/workspace
  (add-hook 'ibuffer-hook #'dtm-ibuffer-group-by-workspace-h))

(when (modulep! :ui popup)
  (set-popup-rules!
    ;; Redefined rules
    '(("^\\*\\(?:Wo\\)?Man "
       :vslot -6 :size 0.45 :select t :quit t :ttl nil)
      ("^\\*\\([Hh]elp\\|Apropos\\)"
       :slot 2 :vslot -8 :size 0.42 :select t :ttl nil)
      ("^\\*info\\*$"
       :slot 2 :vslot 2 :size 0.45 :select t :ttl nil)
      ;; New rules
      ("\\*refs:"
       :size 0.42 :select t :ttl nil)))
  (unless (dtm-doctor-running-p)
    (+popup-cleanup-rules-h))

  ;; Allow popups to be balanced
  (advice-remove 'balance-windows #'+popup-save-a))

(after! dired
  (setq dired-listing-switches "-l --human-readable --group-directories-first"
        dired-hide-details-hide-symlink-targets nil
        dired-dwim-target nil)

  ;; REVIEW manually disable diff-hl hook until dirvish module is merged upstream
  (remove-hook 'dired-mode-hook #'diff-hl-dired-mode)

  ;; Use ediff in dired instead of diff
  (define-key dired-mode-map [remap dired-diff] #'dtm/dired-ediff))

(after! dired-x
  (remove-hook 'dired-mode-hook #'dired-omit-mode))

(after! dirvish
  (setq dirvish-reuse-session nil
        dirvish-attributes
        '(vc-state subtree-state all-the-icons file-time file-size)
        dirvish-mode-line-format
        '(:left (sort file-time) :right (omit yank index))
        dirvish-preview-dispatchers
        '(image gif archive)
        dirvish-quick-access-entries
        `(("D" "~/Downloads/" "Downloads")
          ("dc" ,doom-core-dir "Doom Core")
          ("dl" ,doom-local-dir "Doom Local")
          ("dm" ,doom-modules-dir "Doom Modules")
          ("dr" ,(concat straight-base-dir "straight/repos/") "Doom Repos")
          ("du" ,doom-user-dir "Doom User")
          ("h" "~/" "Home")
          ("m" "/mnt/" "Mount")
          ("n" "~/Nextcloud/" "Nextcloud")
          ("p" "~/Nextcloud/PhD/Projects/" "Projects")
          ("r" "/" "Root")))

  ;; Make dirvish recognise custom project types
  (advice-add 'dirvish--get-project-root :override #'projectile-project-root)

  ;; Bind `revert-buffer' for reloading directory contents
  (map! :map dirvish-mode-map
        :n "C-o"   #'dirvish-history-jump
        :n "C-r"   #'revert-buffer
        :n "C-s"   #'dirvish-fd
        :n "c"     #'dired-create-empty-file
        :n "h"     #'dired-up-directory
        :n "H"     #'dirvish-history-go-backward
        :n "l"     #'dired-find-file
        :n "L"     #'dirvish-history-go-forward
        :n "o"     #'dirvish-quick-access
        :n "Y"     #'dirvish-copy-file-path
        :n "/"     #'find-file)

  ;; Descriptions only work when bound to `major-mode' map
  (map! :map dired-mode-map
        :localleader
        :desc "chmod"           "c" #'drivish-chxx-menu
        :desc "fd"              "f" #'dirvish-fd
        :desc "fd menu"         "F" #'dirvish-fd-switches-menu
        :desc "Group files"     "g" #'dirvish-emerge-menu
        :desc "Git menu"        "G" #'dirvish-vc-menu
        :desc "Hide/omit files" "h" #'dired-omit-mode
        :desc "ls menu"         "l" #'dirvish-ls-switches-menu
        :desc "Mark menu"       "m" #'dirvish-mark-menu
        :desc "Renaming menu"   "r" #'dirvish-renaming-menu
        :desc "Setup dirvish"   "s" #'dirvish-setup-menu
        :desc "Subtree menu"    "S" #'dirvish-subtree-menu))

(after! dirvish-side
  (dirvish-side-follow-mode +1))

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

(after! hydra
  (setq hydra-hint-display-type 'posframe
        hydra-posframe-show-params
        '(:poshandler posframe-poshandler-frame-bottom-center
          :internal-border-width 1
          :internal-border-color "#51afef")))

;;* Doom Core Package Extensions
;; Add colours to info pages to make them more readable
(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-node))

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

;; Undo history tree
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

(use-package! tempel
  :commands tempel-complete tempel-expand tempel-insert
  :config
  (setq tempel-path (expand-file-name "snippets.eld" doom-user-dir)
        tempel-user-elements '(dtm-tempel-whitespace
                               dtm-tempel-double-quote
                               dtm-tempel-include)
        tempel-trigger-prefix ">")

  (map! :map tempel-map
        "<tab>"     #'tempel-next
        "<backtab>" #'tempel-previous
        "C-c C-c"   #'tempel-done
        "C-c C-k"   #'tempel-abort))

;; Templates for new/empty files
(add-hook 'doom-switch-buffer-hook #'dtm-tempel-autoinsert-check-h)

;;* Writing/Organisation Tools
;; Spell checking
(after! ispell
  ;; Global and personal ispell library
  (setq ispell-dictionary "en_GB"
        ispell-personal-dictionary "~/Nextcloud/Dictionary/personal_dict.pws"))

(after! flycheck
  ;; Select flycheck window when opened
  (set-popup-rule! "^\\*Flycheck errors\\*" :size 0.25 :select t :ttl 0)
  (+popup-cleanup-rules-h))

;; Org-mode settings
(defvar dtm-org-line-spacing 0.1
  "`line-spacing' used in `org-mode'.
Also used by `org-modern-mode' to calculate heights.")

(after! org
  (setq org-ellipsis " ▾"
        org-indent-indentation-per-level 1
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
        org-use-property-inheritance t  ; can cause slowdown when searching
        org-image-actual-width '(800)   ; default if not ATTR is provided
        org-agenda-start-day nil
        org-agenda-span 14
        org-agenda-time-grid '((daily today require-timed)
                               (759 1159 1259 1659)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "<- NOW ────────")

  ;; Make headings bold and larger
  (custom-set-faces!
    '((org-document-title outline-1 outline-2 outline-3 outline-4 outline-5
       outline-6 outline-7 outline-8)
      :weight semi-bold)
    '(org-document-title :height 1.3)
    '(outline-1 :height 1.2)
    '(outline-2 :height 1.1)
    '(outline-3 :height 1.05))

  ;; Give ellipsis same colour as text
  (custom-set-faces!
    '(org-ellipsis :foreground nil :background nil :weight regular)
    '(org-headline-done :strike-through t))

  ;; Enable hard wrapping and automate paragraph filling
  ;; Allow for double quoting using '' and `` (`` -> “)
  (add-hook! 'org-mode-hook #'dtm-org-mode-setup-h)

  ;; The `dtm-org-mode-setup-h' function always disables indent guides
  (remove-hook 'org-mode-local-vars-hook #'+indent-guides-disable-maybe-h))

;; Keys bound in after! org seem to get overwritten, this works
(after! org-keys
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
  (advice-add 'org-toggle-pretty-entities :after #'dtm-org-pretty-use-appear-a)
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autoemphasis t
        org-appear-autoentities t))

(use-package! org-modern
  :after org
  :init
  ;; Correct indentation of headings
  (advice-add 'org-indent--compute-prefixes :after #'dtm-org--modern-indent-heading)

  :config
  (setq org-modern-label-border dtm-org-line-spacing
        org-modern-statistics nil
        org-modern-table nil
        org-modern-star ["●" "◉" "○" "◉" "○" "◉" "○" "◉"]
        org-modern-list '((?+ . "›")
                          (?- . "‒")
                          (?* . "•")))

  ;; Ensure symbols do not change when switching fonts
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  ;; Enable
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (add-hook 'org-mode-hook #'dtm-org-modern-mode-maybe-h))

(after! org-tree-slide
  (setq +org-present-text-scale (- dtm-base-font-size 7))

  ;; Make presentations even prettier
  (add-hook! 'org-tree-slide-mode-hook :append #'dtm-org-tree-slide-setup-h)

  ;; Disable `flycheck-mode' and `spell-fu-mode' when presenting
  (advice-add 'org-tree-slide-mode :around #'dtm-org-tree-slide-no-squiggles-a)

  (map! :map org-tree-slide-mode-map
        :n "q" (cmd! (org-tree-slide-mode -1))
        :n [left] #'org-tree-slide-move-previous-tree
        :n [right] #'org-tree-slide-move-next-tree
        :n [C-up] #'org-tree-slide-content))

;; Org-download settings
(after! org-download
  (setq org-download-method 'dtm-org-download-method
        org-download-link-format "[[file:%s]]\n"))

;; Org-pomodoro settings
(after! org-pomodoro
  (setq org-pomodoro-format "%s"
        org-pomodoro-manual-break t
        org-pomodoro-keep-killed-pomodoro-time t))

;; Org-roam init settings
(when (modulep! :lang org +roam2)
  (setq org-roam-directory "~/Nextcloud/PKM/"
        org-roam-dailies-directory "journals/"
        org-roam-file-exclude-regexp "Rubbish/")

  (defvar dtm-org-roam-index-file "pages/contents.org"))

(after! org-roam
  ;; Disable completion everywhere as it overrides company completion
  (setq org-roam-completion-everywhere nil)

  ;; Custom org-roam buffer preview function
  (setq org-roam-preview-function #'dtm-org-element-at-point-get-content)

  ;; Automatically update the slug in the filename when #+title: has changed.
  (add-hook 'org-roam-find-file-hook #'dtm-org-roam-update-slug-on-save-h)

  ;; Make the backlinks buffer easier to peruse by folding leaves by default.
  (add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)

  ;; Add ID, Type, Tags, and Aliases to top of backlinks buffer.
  (advice-add #'org-roam-buffer-set-header-line-format :after #'dtm-org-roam-add-preamble-a)

  ;; Open all roam buffers in a dedicated workspace
  (dolist (symbol '(org-roam-node-find
                    org-roam-node-random
                    org-roam-ref-find
                    org-roam-dailies--capture
                    org-roam-buffer-display-dedicated
                    org-roam-buffer-toggle))
    (advice-add symbol :before #'dtm-org-roam-goto-workspace))

  ;; Sync org-agenda with org-roam dailies
  (advice-add 'org-agenda :before #'dtm-org-roam-dailies-sync-agenda)

  ;; Ensure keybindings are loaded for dailies-calendar
  (advice-add 'org-roam-dailies-goto-date :before #'dtm-org-roam-dailies-goto-date-a)

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
                              "#+title: %<%b %d %Y>\n#+date: %<%A %B %d, Week %W %Y>\n \n* Agenda\n")
           :empty-lines 1))))

(when (modulep! :tools biblio)
  ;; Citar bibliography settings
  (setq! citar-bibliography '("~/Nextcloud/Zotero/master.bib")
         citar-library-paths '("~/Nextcloud/Zotero/")))

(after! citar
  ;; Ensure notes can be accessed by `citar-open-notes'
  (require 'citar-org-roam)

  (setq citar-org-roam-note-title-template "${=key=}: ${title}\n\n* Notes"
        citar-org-roam-subdir "notes")

  ;; Dedicated workspaces
  (advice-add 'citar-open-files :before #'dtm-citar-goto-workspace)
  (advice-add 'citar--open-entry :before #'dtm-citar-goto-workspace)
  (advice-add 'citar-open-notes :before #'dtm-org-roam-goto-workspace)

  ;; Disable citation delete binding
  (map! :map citar-org-citation-map "C-d" nil))

;; Org-noter settings
(after! org-noter
  (setq org-noter-hide-other nil           ; Don't fold headings when navigating
        org-noter-always-create-frame nil) ; Only crete new frames for additional sessions

  ;; Kill session map in line with other C-c bound pdf controls for one hand use
  (map! :map (org-noter-doc-mode-map org-noter-notes-mode-map)
        "C-c q" #'org-noter-kill-session)

  ;; The pdf-view major mode overwrites the i binding with =ignore= for all minor modes
  ;; This works around that by incorporating the binding into the major mode
  (map! :map pdf-view-mode-map :n "i" #'dtm/org-noter-insert-maybe))

(after! pdf-tools
  ;; Distinguish current match
  (custom-set-faces! '(pdf-isearch-match :inherit highlight))

  (setq pdf-view-resize-factor 1.1)

  (add-to-list 'evil-snipe-disabled-modes 'pdf-view-mode)

  (map! (:map pdf-view-mode-map
         :n "C-e" #'pdf-view-scroll-down-or-previous-page
         :n "C-s" #'isearch-forward-word
         :n "S"   #'dtm/pdf-view-fit-half-height
         :v "h"   #'pdf-annot-add-highlight-markup-annotation
         :v "s"   #'pdf-annot-add-strikeout-markup-annotation
         :v "u"   #'pdf-annot-add-underline-markup-annotation
         (:prefix "s"
          :desc "Rotate page"        :n "r" #'image-rotate
          :desc "Slice original"     :n "o" #'pdf-view-reset-slice
          :desc "Slice bounding box" :n "b" #'pdf-view-set-slice-from-bounding-box
          :desc "Slice using mouse"  :n "m" #'pdf-view-set-slice-using-mouse)
         (:prefix "C-c"
          :desc "Add Note"          "a" #'pdf-annot-add-text-annotation
          :desc "Delete Annotation" "d" #'pdf-annot-delete))

        (:map pdf-history-minor-mode-map
         :n "<tab>"     #'pdf-history-backward
         :n "<backtab>" #'pdf-history-forward
         :n [mouse-8]   #'pdf-history-backward
         :n [mouse-9]   #'pdf-history-forward)))

;; LaTeX settings
(after! tex-mode
  (setq TeX-PDF-mode t
        TeX-source-correlate-start-server t
        +latex-viewers '(pdf-tools))

  ;; Compatibility with multi-file documents
  (setq-default TeX-master nil))

(after! markdown-mode
  ;; Disable proselint in Rmarkdown files
  (add-hook! 'markdown-mode-hook #'dtm-flycheck-disable-proselint-rmd-h))

;;* Programming Languages
;; General interactive programming buffer settings
(after! comint
  (setq ansi-color-for-comint-mode 'filter
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)

  ;; Shell style clear REPL binding
  (map! :map comint-mode-map
        "C-l" #'comint-clear-buffer))

(after! compile
  (add-hook! 'compilation-mode-hook (visual-line-mode +1)))

(after! tree-sitter
  (custom-set-faces!
    '(tree-sitter-hl-face:number :inherit highlight-numbers-number)
    '(tree-sitter-hl-face:type.builtin :inherit font-lock-warning-face :weight bold)))

(when (modulep! :lang emacs-lisp)
  ;; REVIEW fix the doom custom "Section" imenu entry
  (advice-add '+emacs-lisp-extend-imenu-h :after #'dtm-fix-elisp-extend-imenu-a)

  (add-hook! 'emacs-lisp-mode-hook :append #'dtm-elisp-extend-imenu-h))

(after! elisp-refs
  (add-hook 'elisp-refs-mode-hook #'hide-mode-line-mode)

  ;; Open files in other window to preserve the popup window
  (advice-add 'elisp-refs--find-file :override #'dtm-elisp-refs--find-file-a)
  (map! :map elisp-refs-mode-map :n "RET" #'elisp-refs-visit-match-other-window))

(after! lispy
  ;; Prettier function evaluation
  (setq lispy-eval-display-style 'overlay)

  ;; Do not enable lispy in the minibuffer
  (remove-hook 'eval-expression-minibuffer-setup-hook
               #'doom-init-lispy-in-eval-expression-h)

  ;; Unbind `lispy-occur' because we drop the swiper package
  (unbind-key "y" lispy-mode-map)

  ;; Define custom special key for stepping into lists/deleting marked regions
  (lispy-define-key lispy-mode-map "i" #'dtm/lispy-step-into)

  ;; Rebind the key previously on "i"
  (map! :map lispy-mode-map "TAB" #'special-lispy-tab))

(after! eros
  ;; Large results can freeze emacs, this limits the inconvenience
  (setq eros-eval-result-duration 2))

(after! eshell
  ;; Fuzzy match parent directories (a.k.a. "bd")
  ;; The "z" command does the same but for dir history
  (add-to-list '+eshell-aliases '("up" "eshell-up $1")))

(after! vterm
  ;; Actually clear buffer upon C-l
  (setq vterm-clear-scrollback-when-clearing t)

  ;; Don't consider vterm buffer as popup (only doom:vterm)
  (set-popup-rule! "^\\*vterm" :ignore t)
  (+popup-cleanup-rules-h)

  (remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)

  ;; Fix evil cursor getting out of sync
  (advice-add 'vterm-send-key :before #'dtm-vterm-sync-cursor-a)
  (advice-add 'vterm--redraw :around #'dtm-vterm-redraw-cursor-a))

(after! sh-script
  (map! :map sh-mode-map
        :nv [C-return] #'dtm/vterm-execute-current-line))

(after! ess
  ;; Use current dir for session
  (setq ess-ask-for-ess-directory nil
        ess-style 'RStudio)

  ;; Add company-R-library backend
  ;; REVIEW https://github.com/doomemacs/doomemacs/pull/6455
  (set-company-backend! 'ess-r-mode
    '(company-R-args company-R-objects company-R-library company-dabbrev-code :separate))

  (add-hook 'ess-r-mode-local-vars-hook #'tree-sitter! 'append)

  ;; Recognise F/T as boolean values
  (tree-sitter-hl-add-patterns 'r
    [((identifier) @boolean
      (.eq? @boolean "T"))
     ((identifier) @boolean
      (.eq? @boolean "F"))
     ((call function: (identifier) @keyword)
      (.eq? @keyword "stop"))
     ((call function: (identifier) @keyword)
      (.eq? @keyword "warning"))
     ((call function: (identifier) @keyword)
      (.eq? @keyword "return"))])

  ;; Make inferior buffer not take focus on startup
  (advice-add 'ess-switch-to-inferior-or-script-buffer :around #'dtm-ess-switch-maybe-a)

  ;; Make evil tab width same as ESS offset
  (add-hook! 'ess-mode-hook (setq-local evil-shift-width 'ess-indent-offset))

  (add-hook! 'inferior-ess-r-mode-hook (visual-line-mode +1))

  ;; Lag the cursor in debug mode, this leaves the point at a variable after its assigned
  (advice-add 'ess-debug-command-next :around #'dtm-with-lagging-point-a)
  (dolist (symbol '(dtm/ess-debug-command-step
                    ess-debug-command-up
                    ess-debug-command-quit))
    (advice-add symbol :after #'dtm-lagging-point-reset))

  ;; ESS R keybindings, make < add a <-, type twice to undo (same goes for >)
  (map! (:map ess-r-mode-map
         :nv [C-return] #'ess-eval-region-or-line-and-step
         :localleader
         :desc "Eval reg|func|para"         "e" #'ess-eval-region-or-function-or-paragraph
         :desc "Environment list R objects" "E" #'ess-rdired
         :desc "Source current file"        "s" #'ess-load-file
                                            "S" #'ess-switch-process
         :desc "Eval symbol at point"       "." #'dtm/ess-eval-symbol-at-point)

        (:map (ess-r-mode-map inferior-ess-r-mode-map)
         :i "<" #'dtm/ess-r-insert-assign
         :i ">" #'dtm/ess-r-insert-pipe)

        (:map inferior-ess-mode-map
         :localleader
         "TAB" #'ess-switch-to-inferior-or-script-buffer
         "x r" #'inferior-ess-reload
         "h h" #'ess-display-help-on-object)

        (:map ess-debug-minor-mode-map
         "M-S" #'dtm/ess-debug-command-step
         "M-E" #'dtm/ess-eval-symbol-at-point
         "M-A" #'dtm/lagging-point-goto-actual)

        ;; REVIEW https://github.com/doomemacs/doomemacs/pull/6455
        (:map ess-roxy-mode-map
         :i "RET" #'ess-indent-new-comment-line)))

(after! ess-s-lang
  ;; Imenu search entries, best invoked with `consult-imenu' (SPC s i)
  (add-to-list 'ess-imenu-S-generic-expression
               '("Section" "^\\(#+ [^\n]+\\) ----+" 1)))

(use-package! ess-view-data
  :commands ess-view-data-print
  :init
  (map! :map ess-r-mode-map
        :desc "View R object" :localleader "o" #'ess-view-data-print))

(use-package! ess-r-insert-obj
  :commands ess-r-insert-obj-col-name
  :init (map! :map ess-r-mode-map :i "M-o" #'ess-r-insert-obj-col-name))

(after! python
  ;; Add generic imenu expression and ensure python doesn't ignore them
  (add-hook! 'python-mode-hook
             #'dtm-imenu-merge-index-h
             (setq-local imenu-generic-expression
                         '(("Rule" "^rule \\(\\_<[^ \t():\n]+\\_>\\):" 1))))

  ;; Python keybindings
  (map! :map python-mode-map
        :desc "Send file to REPL" :localleader "f" #'python-shell-send-file))

;; Snakefiles in python mode
(pushnew! auto-mode-alist
          '("Snakefile" . python-mode)
          '("\\.smk\\'" . python-mode))

(use-package! elpy
  :commands elpy-enable
  :init
  (advice-add 'python-mode :before #'elpy-enable)
  :config
  (setq elpy-shell-starting-directory 'current-directory
        elpy-modules '(elpy-module-sane-defaults elpy-module-eldoc))

  (set-company-backend! 'python-mode
    'elpy-company-backend 'company-yasnippet)
  (set-lookup-handlers! 'python-mode
    :definition #'elpy-goto-definition
    :references #'elpy-rgrep-symbol
    :documentation #'elpy-doc
    :async t)

  (advice-add 'elpy-shell-switch-to-shell :after #'evil-normal-state)

  (map! (:map python-mode-map
         :nv [C-return] #'dtm/elpy-send-current-and-step
         (:localleader
          :desc "Eval buffer"            "b"   #'elpy-shell-send-buffer
          :desc "Eval defun"             "d"   #'elpy-shell-send-defun
          :desc "Eval line/statement"    "l"   #'elpy-shell-send-statement
          :desc "Eval top statement"     "s"   #'elpy-shell-send-top-statement
          :desc "Print symbol or region" "."   #'dtm/elpy-print-symbol-or-region
          :desc "Switch to REPL"         "TAB" #'elpy-shell-switch-to-shell))

        (:map inferior-python-mode-map
         :localleader
         :desc "Switch to script" "TAB" #'elpy-shell-switch-to-buffer)))

(after! conda
  ;; Prompt user to change into a conda env
  ;; HACK use the find-file-hook because elpy keeps triggering python-mode
  (when (getenv "CONDA_EXE")
    (add-hook! 'find-file-hook #'dtm-conda-env-guess-prompt-h))

  (map! :map (python-mode-map inferior-python-mode-map)
        :localleader :prefix ("c" . "Conda")
        :desc "Guess conda env" "g" #'dtm/conda-env-guess-prompt
                                "a" #'conda-env-activate
                                "d" #'conda-env-deactivate))

(after! csv-mode
  ;; Assume the first line of a csv is a header
  (setq csv-header-lines 1)

  ;; Ensure delimiters are not hidden when aligning
  (add-hook! 'csv-mode-hook
    (setq-local buffer-invisibility-spec nil)))

;; Start csv/tsv files in so-long-mode to prevent freezing
(pushnew! auto-mode-alist
          '(".csv" . so-long-mode)
          '(".tsv" . so-long-mode))

;; Enable csv/tsv mode on files with short lines
(add-hook! 'so-long-mode-hook #'dtm-csv-mode-maybe-h)

;;* Misc Packages
;; Smooth scrolling
(use-package! good-scroll
  :commands good-scroll-mode
  :init
  ;; Override evil functions on mode activation, undo upon deactivation
  (add-hook! 'good-scroll-mode-hook #'dtm-good-scroll-evil-override-h)

  ;; Auto enable only when not on laptop (can still be manually activated)
  (unless IS-LAPTOP (good-scroll-mode +1))

  :config
  ;; Increase animation time and mouse scrolling sensitivity
  (setq good-scroll-duration 0.25
        good-scroll-algorithm 'good-scroll-linear
        good-scroll-step (round (/ (display-pixel-height) 5))))

;; Package for interacting with text fields, requires GhostText browser extension
(use-package! atomic-chrome
  :commands atomic-chrome-start-server
  :config
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("overleaf\\.com" . latex-mode)
          ("azuredatabricks\\.net" . python-mode))))

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

;; Improved isearch
(use-package! ctrlf
  :commands ctrlf-forward-default
  :config
  ;; Use 'M-s s' while searching to change styles
  (setq ctrlf-default-search-style 'fuzzy-multi
        ctrlf-show-match-count-at-eol t
        ctrlf-auto-recenter t)

  ;; Fuzzy matching across multiple lines
  (add-to-list
   'ctrlf-style-alist
   '(fuzzy-multi . (:prompt "fuzzy multi-line"
                    :translator dtm-translate-fuzzy-multi-literal
                    :case-fold ctrlf-no-uppercase-literal-p)))

  (map! :map ctrlf-minibuffer-mode-map
        "C-s" #'ctrlf-next-match
        "C-r" #'ctrlf-previous-match
        "C-u" #'ctrlf-previous-page
        "C-d" #'ctrlf-next-page))

(load! "+keybindings")
(load! "+faces")
