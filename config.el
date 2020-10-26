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
      doom-variable-pitch-font (font-spec :family "Overpass" :size 18)
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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

;; Consider all windows when resizing and stretch cursor to glyph size
(setq-default window-combination-resize t
              x-stretch-cursor t)

;; Raise undo-limit to 80Mb and enable auto save
(setq undo-limit 80000000
      auto-save-default t)

;; More granualr inset mode undos
(setq evil-want-fine-undo t)

;; Truncate ...
(setq truncate-string-ellipsis "…")

;; On laptops it's nice to know how much power you have
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

;; Iterate through CamelCase words
(global-subword-mode 1)

;; Start emacs in fullscreen maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Ask for file after splitting the window
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

;; Org mode as default mode
(setq-default major-mode 'org-mode)

;; Enable visual lines with word wrapping
(global-visual-line-mode t)

;; Make j/k move visual lines (gj/gk)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Scrolling margins
(setq scroll-margin 5)
(setq smooth-scroll-margin 5)

;; Define zenmode text scale
(setq +zen-text-scale 0.5)

;; AucTex settings
(setq TeX-command-force "LatexMk"
      TeX-PDF-mode t
      +latex-viewers '(pdf-tools okular sumatrapdf zathura skim evince))

;; Rebindings for TeX-font
(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-medium () (interactive) (TeX-font nil ?\C-m))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
(defun latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
(defun latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
(defun latex/font-normal () (interactive) (TeX-font nil ?\C-n))
(defun latex/font-serif () (interactive) (TeX-font nil ?\C-r))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
(defun latex/font-upright () (interactive) (TeX-font nil ?\C-u))

;; Latex Spacemacs keybindings
(map! (:when (featurep! :lang latex)
  (:map LaTeX-mode-map
    (:prefix ("z" . "TeX-fold")
      "=" 'TeX-fold-math
      "b" 'TeX-fold-buffer
      "B" 'TeX-fold-clearout-buffer
      "e" 'TeX-fold-env
      "I" 'TeX-fold-clearout-item
      "m" 'TeX-fold-macro
      "p" 'TeX-fold-paragraph
      "P" 'TeX-fold-clearout-paragraph
      "r" 'TeX-fold-region
      "R" 'TeX-fold-clearout-region
      "z" 'TeX-fold-dwim)
    :localleader
      "\\"  'TeX-insert-macro
      "-"   'TeX-recenter-output-buffer
      "%"   'TeX-comment-or-uncomment-paragraph
      ";"   'comment-or-uncomment-region
      "a"   'TeX-command-run-all
      "k"   'TeX-kill-job
      "l"   'TeX-recenter-output-buffer
      "m"   'TeX-insert-macro
      "n"   'TeX-next-error
      "N"   'TeX-previous-error
      "v"   'TeX-view
      "*"   'LaTeX-mark-section
      "."   'LaTeX-mark-environment
      "c"   'LaTeX-close-environment
      "e"   'LaTeX-environment
      "i"   'LaTeX-insert-item
      "s"   'LaTeX-section
      (:prefix ("f" . "LaTeX-fill")
        "e"  'LaTeX-fill-environment
        "p"  'LaTeX-fill-paragraph
        "r"  'LaTeX-fill-region
        "s"  'LaTeX-fill-section)
      (:prefix ("p" . "preview")
        "b"  'preview-buffer
        "c"  'preview-clearout
        "d"  'preview-document
        "e"  'preview-environment
        "f"  'preview-cache-preamble
        "p"  'preview-at-point
        "r"  'preview-region
        "s"  'preview-section)
      (:prefix ("x" . "text")
        "b"  'latex/font-bold
        "c"  'latex/font-code
        "e"  'latex/font-emphasis
        "i"  'latex/font-italic
        "r"  'latex/font-clear
        "o"  'latex/font-oblique
        "B"  'latex/font-medium
        "r"  'latex/font-clear
        (:prefix ("f" . "font")
          "c" 'latex/font-small-caps
          "f" 'latex/font-sans-serif
          "a" 'latex/font-calligraphic
          "n" 'latex/font-normal
          "u" 'latex/font-upright
          "r" 'latex/font-serif)))))
