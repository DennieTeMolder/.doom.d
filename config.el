;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name nil
      user-mail-address nil)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 18)
      doom-big-font (font-spec :family "Fira Code" :size 30)
      ;; doom-serif-font (font-spec :family "Nimbus Serif")
      doom-variable-pitch-font (font-spec :family "Lora" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Org/")

;; Make sure the org dir exists (important for id hash-tables)
(unless (file-exists-p org-directory)
  (make-directory org-directory t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Check if running on WSL
(setq IS-WSL (if (string-match "-[Mm]icrosoft" (shell-command-to-string "uname -a")) t nil))

;; Quit without confirmation
(setq confirm-kill-emacs nil)

;; Default major mode for scratch buffer
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; Rudimentary settings
(setq-default delete-by-moving-to-trash t
              tab-with 2
              uniquify-buffer-name-style 'forward
              x-stretch-cursor t) ; stretch cursor to glyph size

;; Save and undo settings
(setq undo-limit 80000000 ; Raise undo-limit to 80Mb
      auto-save-default t ; Enable auto save
      evil-want-fine-undo t ; Granular undo in insert mode
      inhibit-compacting-font-caches t) ; Keep all glyphs in memory

;; Start emacs maximized on WSL
(when IS-WSL (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; On laptops it's nice to know how much power you have
(use-package! battery
  :defer 1
  :config
  (unless (equal "unknown" (cdr (assoc 66 (funcall battery-status-function))))
    (display-battery-mode 1)))

;; Replace the default doom splash screen with amore subtle one
(defun doom-dashboard-draw-ascii-banner-fn ()
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

;; Increase horizontal scroll (shift + mwheel) sensitivity
(setq mouse-wheel-scroll-amount-horizontal 12)

;; Iterate through CamelCase words in programming buffers
(add-hook! 'prog-mode-hook (subword-mode 1))

;; Change window split direction
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Only display encoding in modeline when it's not UTF-8
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook! 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; Simplify window title and give a visual indication if file is edited
(setq frame-title-format
    '(""
      (:eval
       (if (s-contains-p (concat org-roam-directory "pages") (or buffer-file-truename ""))
           (replace-regexp-in-string ".*/[0-9]*-?" ">" buffer-file-name)
         "%b"))
      (:eval
       (let ((project-name (if (string= "-" (projectile-project-name)) "Doom Emacs" (projectile-project-name))))
         (format (if (buffer-modified-p)  " + | %s" " | %s") project-name)))))

;; Enable visual lines with word wrapping
(global-visual-line-mode t)

;; Make j/k move visual lines (gj/gk)
(map!
 :nvm "j" 'evil-next-visual-line
 :nvm "k" 'evil-previous-visual-line)

;; Spacemacs style M-x
;; Old SPC SPC binding (projectile find file) also available under "SPC p f"
;; This frees up the "SPC :" to be another evil-ex because i am condition to hit SPC
(map! :leader
      :desc "M-x" "SPC" 'execute-extended-command
      :desc "Evil ex command" ":" 'evil-ex)

;; Global keybindings
(map! :leader "b D" 'kill-buffer-and-window)

;; Use mouse buttons to go forward/backward in history
(map! :n [mouse-8] #'winner-undo
      :n [mouse-9] #'winner-redo)

;; Repeat last command using SPC r
(map! :leader "r" 'repeat)

;; Increase auto-completion suggestion delay
(setq company-idle-delay 0.4)

;; Projectle sorting by recently opened
(setq projectile-sort-order 'recently-active)

;; Exclude autosave from recent files
(after! recentf
  (add-to-list 'recentf-exclude "\\autosave\\'"))

;; Define zenmode text scale
(setq +zen-text-scale 1.25
      writeroom-width 70
      writeroom-mode-line t
      +zen-mixed-pitch-modes '(org-mode latex-mode markdown-mode))

;; Don't use the variable pitch font for treemacs
(setq doom-themes-treemacs-enable-variable-pitch nil)

;; Bind window resize hydra from doom hydra module
(map! (:leader :desc "Adjust windows hydra" :leader "w a" '+hydra/window-nav/body))

;; Save clipboard to kill ring before deleting text 
;; Cyle kill ring using <C-p> or <C-n> after pasting
(setq save-interprogram-paste-before-kill t)

;; Personal ispell library
(setq ispell-personal-dictionary "~/MEGA/Dictionary/personal_dict.pws")

;; PDFView bindings
(map! :map pdf-view-mode-map
      :v "h" 'pdf-annot-add-highlight-markup-annotation
      :v "s" 'pdf-annot-add-strikeout-markup-annotation
      :v "u" 'pdf-annot-add-underline-markup-annotation
      (:prefix "C-c"
       :desc "Add Note" "a" 'pdf-annot-add-text-annotation
       :desc "Delete Annotation" "d" 'pdf-annot-delete))

(map! :map pdf-history-minor-mode-map
      :nv "C-o" 'pdf-history-backward
      :nv [mouse-8] 'pdf-history-backward
      :nv [mouse-9] 'pdf-history-forward)

;; AucTex settings, inverse searching also requires config of the pdf reader
(setq TeX-command-force "LatexMk"
      TeX-PDF-mode t
      TeX-source-correlate-start-server t
      +latex-viewers '(okular pdf-tools sumatrapdf zathura skim evince))

;; Compatibility with multi-file documents
(setq-default TeX-master nil)

;; ESS R settings
;; Use a default session name and auto scroll down in REPL windows
(setq ess-ask-for-ess-directory nil
      comint-prompt-read-only t
      comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-move-point-for-output t)

;; Proper number highlighting for R mode
(after! highlight-numbers
  (puthash 'ess-r-mode
           "\\_<\\(?:[0-9]+\\)?\\(?:\\.[0-9]*\\)?\\(?:e-?[0-9]+\\)?\\_>"
           highlight-numbers-modelist))

(after! ess
  ;; Enable additional highlighting
  (add-to-list 'ess-R-font-lock-keywords '(ess-R-fl-keyword:F&T . t))
  ;; Customize type faces
  (set-face-attribute 'ess-constant-face nil :weight 'bold :inherit font-lock-warning-face))

(defadvice! my/advice-ess-describe (orig-fn)
  "Switch to the REPL buffer after closing the *ess-describe* buffer"
  :around #'ess-describe-object-at-point
  (let ((starting-window (selected-window)))
    (funcall orig-fn)
    (ess-switch-to-ESS t)
    (select-window starting-window)))

;; Define function to insert a pipe symbol for R mode
(defun ess-insert-pipe (arg)
  "Based on `ess-insert-assign', invoking the command twice reverts the insert"
  (interactive "p")
  (if (string= ess-language "S")
      (let* ((pipe " %>% ")
             (event (event-basic-type last-input-event))
             (char (ignore-errors (format "%c" event))))
        (cond ((and char (ess-inside-string-or-comment-p))
               (insert char))
              ((re-search-backward pipe (- (point) (length pipe)) t)
               (if (and char (numberp event))
                   (replace-match char t t)
                 (replace-match "")))
              (t (insert pipe))))
    (funcall #'self-insert-command arg)))

;; ESS R keybindings, make < add a <-, type twice to undo (same goes for >)
(map! (:after ess
       (:map ess-mode-map
        :n [C-return] 'ess-eval-region-or-line-and-step)
       (:map ess-r-mode-map
        "<" 'ess-insert-assign
        ">" 'ess-insert-pipe
        :localleader
        :desc "Environment list R objects" "e" 'ess-rdired)
       (:map inferior-ess-mode-map
        "C-l" 'comint-clear-buffer)))

(use-package! ess-view-data
  :commands ess-view-data-print
  :init
  (map! :map ess-r-mode-map
        :desc "View R object" :localleader "o" 'ess-view-data-print))

;; Python Settings
(custom-set-variables
 '(conda-anaconda-home "~/.local/miniconda3/"))

(after! conda
  ;; Default to base env if non is provided
  (add-hook! 'conda-env-autoactivate-mode-hook
    (if (and conda-env-autoactivate-mode (not conda-env-current-name))
        (conda-env-activate "base")))
  (conda-env-autoactivate-mode t))

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
  (if (not (python-shell-get-process))
      (progn
        (save-selected-window
          (call-interactively #'+python/open-ipython-repl))
        ;; Give python time to load the interaction module
        (sleep-for .5)))
  ;; Check for region, start of block, or other and act accordingly
  (cond ((region-active-p)
         (call-interactively #'python-shell-send-region))
        ((python-info-statement-starts-block-p)
         (call-interactively #'my/python-shell-send-block-and-step))
        (t
         (call-interactively #'my/python-shell-send-statment-and-step))))

;; Python keybindings
(map! :mode python-mode
      :nv [C-return] 'my/python-send-current-and-step
      :localleader
      :desc "Open python REPL" [tab] '+python/open-ipython-repl
      :desc "Send buffer to REPL" "b" 'python-shell-send-buffer
      :desc "Send file to REPL" "f" 'python-shell-send-file
      :prefix ("c". "Conda")
       "a" 'conda-env-activate
       "d" 'conda-env-deactivate)

;; Snakefiles in python mode
(add-to-list 'auto-mode-alist '("Snakefile" . python-mode))
(add-to-list 'auto-mode-alist '("\\.smk\\'" . python-mode))

;; Org-mode settings
(setq org-indent-indentation-per-level 1
      org-ellipsis " ▾")

;; Disable company auto pop-up in org, use C-SPC to trigger
(add-hook! 'org-mode-hook (setq-local company-idle-delay nil))

;; Enable auto-fill mode in org buffers
(add-hook! 'org-mode-hook
  (auto-fill-mode 1)
  (electric-quote-local-mode 1))

;; Unbind the insert mode cdlatex-math-symbol binding
;; This frees up the backtick for electric-quote-mode (`` -> “)
(after! org (map! :map org-cdlatex-mode-map "`" nil))

(after! org
  ;; Make headings bold and larger
  (dolist (face '((org-document-title . 1.2)
                  (org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'semi-bold :height (cdr face)))

  ;; Give ellipsis same color as text
  (set-face-attribute 'org-ellipsis nil :foreground nil :background nil :weight 'regular))

;; Fancy org mode bullets
(use-package! org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")
        org-superstar-todo-bullet-alist
        '(("TODO" . 9744)
          ("[ ]"  . 9744)
          ("DONE" . 9745)
          ("[X]"  . 9745))))

;; Default bibliography
(setq! citar-bibliography '("~/MEGA/Zotero/master.bib")
       citar-library-paths '("~/MEGA/Zotero/")
       citar-notes-paths '("~/MEGA/PKM/notes/"))
(setq! org-cite-csl-styles-dir "~/MEGA/Zotero/Styles")

;; Binding to view bibliography
(map! :leader "n b" 'citar-open-entry)

;; Update citar cache when bib-file changes in during specified modes
(after! bibtex-completion
  (citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook)))

;; Add roam id and ref to new literature notes
(setq citar-file-note-org-include '(org-id org-roam-ref))

;; Org-cite settings
(after! (oc org bibtex-completion citar)
  (setq org-cite-export-processors
        '((latex biblatex "ieee")
          (t csl "ieee.csl"))))

;; Use old org-ref insert key
(after! org (map! :map org-mode-map "C-c ]" 'org-cite-insert))

;; Org-roam workflow settings
(setq org-roam-directory "~/MEGA/PKM/"
      org-roam-dailies-directory "journals/"
      org-roam-index-file "pages/contents.org"
      org-roam-file-exclude-regexp "Rubbish/")

;; Disable completion everywhere as it overrides company completion
(after! org-roam (setq org-roam-completion-everywhere nil))

;; Define function to open index file
(defun my/org-roam-open-index ()
  "Opens the file specified in org-roam-index-file"
  (interactive)
  (find-file (expand-file-name org-roam-index-file org-roam-directory)))

;; Map to keybinding
(map! :desc "Open index" :leader "n r o" 'my/org-roam-open-index)

;; Add to doom dashboard
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
        ("Open roam index" :icon
         (all-the-icons-octicon "database" :face 'doom-dashboard-menu-title)
         :action my/org-roam-open-index)
        ("Open roam today" :icon
         (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
         :action org-roam-dailies-goto-today)
        ("Recently opened files" :icon
         (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
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

;; Roam templates
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?"
         :target (file+head "%<%Y_%m_%d>.org"
                            "#+title: %<%Y-%m-%d>\n#+DATE: %<%A %B %d, Week %W %Y>\n\n* Agenda\n"))))

;; citar note template
(after! citar
  (push '(note . "#+TITLE: ${=key=}: ${title}\n\n* Notes") citar-templates))

;; Deft settings
(setq deft-directory org-roam-directory
      deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
      deft-recursive t
      deft-use-filename-as-title t)

;; Org-noter settings
(setq org-noter-hide-other nil ;; Don't fold headings when navigating
      org-noter-always-create-frame nil) ;; Only crete new frames for additional sessions

;; Kill session map in line with other C-c bound pdf controls for one hand use
(map! :map (org-noter-doc-mode-map org-noter-notes-mode-map)
      "C-c q" 'org-noter-kill-session)

;; The pdf-view major mode overwrites the i binding with =ignore= for all minor modes
;; This works around that by incorporating the binding into the major mode
(map! :map pdf-view-mode-map
      :desc "Insert org note" :n "i" (cmd! (if org-noter-doc-mode
                                               (org-noter-insert-note)
                                             (ignore)))
      :nv "C-e" 'pdf-view-scroll-down-or-previous-page)

;; Org-download settings
(defun drestivo/org-download-method (link)
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

(after! org-download
  (setq org-download-method 'drestivo/org-download-method
        org-download-link-format "[[file:%s]]\n"))

;; M-x interaction-log-mode shows all executed command for debugging/showcasing
(use-package! interaction-log
  :commands interaction-log-mode
  :config
  ;; TODO prompt user to execute this function after interaction-log-mode
  (defun interaction-log-show ()
    "Creates an interaction log window if it doesn't exist"
    (interactive)
    (display-buffer ilog-buffer-name)))

;; Smooth scrolling
(use-package! good-scroll
  :if (not IS-WSL)
  :config
  ;; Increase animation time and mouse scrolling sensitivity
  (setq good-scroll-duration .25
        good-scroll-algorithm 'good-scroll-linear
        good-scroll-step (round (/ (display-pixel-height) 5)))

  ;; binding to toggle good scroll mode
  (map! :desc "Toggle smooth scrolling" :leader "t S" 'good-scroll-mode)

  ;; Evil scrolling
  (defun my/good-scroll-down-half ()
    (interactive)
    (good-scroll-move (/ (good-scroll--window-usable-height) 2)))

  (defun my/good-scroll-up-half ()
    (interactive)
    (good-scroll-move (/ (good-scroll--window-usable-height) -2)))

  (defun my/toggle-bind-evil-smooth-scroll ()
    (if good-scroll-mode
        (progn
          (map! :nv "C-d" 'my/good-scroll-down-half
                :nv "C-u" 'my/good-scroll-up-half))
      (progn
        (map! :nv "C-d" 'evil-scroll-down
              :nv "C-u" 'evil-scroll-up))))

  ;; Switch key map on mode enable/disable
  (add-hook! 'good-scroll-mode-hook #'my/toggle-bind-evil-smooth-scroll)

  ;; Enable good-scroll
  (good-scroll-mode 1)
)

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
  (map! :desc "Toggle GhostText server" :leader "t G" 'my/atomic-chrome-toggle-server)

  :config
  (setq atomic-chrome-buffer-open-style 'full)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("overleaf\\.com" . latex-mode)
          ("azuredatabricks\\.net" . python-mode))))

;; Enable vertico mouse extension (included with vertico)
(use-package! vertico-mouse
  :after vertico
  :config (vertico-mouse-mode 1))
