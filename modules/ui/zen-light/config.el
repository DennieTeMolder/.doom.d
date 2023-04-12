;;; ui/zen-light/config.el -*- lexical-binding: t; -*-

(defvar +zenl-reading-modes
  '(org-mode LaTeX-mode markdown-mode gfm-mode helpful-mode Info-mode adoc-mode rst-mode)
  "What major-modes to enable `mixed-pitch-mode' in with `visual-fill-column-mode'.")

(defvar +zenl-reading-scale 1
  "Value to use for `text-scale-set' in `+zenl-reading-modes'.")

(use-package! visual-fill-column
  :commands visual-fill-column-mode
  :hook (visual-fill-column-mode . +zenl-visual-fill-column-h)
  :config
  ;; Text scaling is bugged: https://codeberg.org/joostkremers/visual-fill-column/issues/1
  (setq visual-fill-column-adjust-for-text-scale t
        visual-fill-column-fringes-outside-margins nil
        visual-fill-column-center-text t))

(use-package! mixed-pitch
  :hook (visual-fill-column-mode . +zenl-enable-mixed-pitch-mode-h)
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'solaire-line-number-face))
