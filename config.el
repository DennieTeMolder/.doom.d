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
(setq doom-font (font-spec :family "Fira Code" :size 20)
      doom-big-font (font-spec :family "Fira Code" :size 32)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 20)
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

;; Raise undo-limit to 80Mb, more granular insert mode undos and auto save
(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      truncate-string-ellipsis "…")

;; On laptops it's nice to know how much power you have
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

;; Iterate through CamelCase words
(global-subword-mode 1)

;; Start emacs in fullscreen maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; AucTex settings
(setq TeX-command-force "LatexMk"
      TeX-PDF-mode t
      ;; Tex-source-correlate-mode t
      +latex-viewers '(pdf-tools okular sumatrapdf zathura skim evince))

;; (setq TeX-source-correlate-method 'synctex
;;       TeX-view-program-list
;;       '(("Sumatra PDF" ("\"/mnt/c/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
;;                         (mode-io-correlate " -forward-search %b %n ") " %o"))))

;; (eval-after-load 'tex
;;  '(progn
;;    (assq-delete-all 'output-pdf TeX-view-program-selection)
;;    (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")))
;;  )
;; (server-start)
