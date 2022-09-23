;;; ui/zen-light/config.el -*- lexical-binding: t; -*-

(defvar +zenl-mixed-pitch-modes
  '(org-mode LaTeX-mode markdown-mode gfm-mode helpful-mode Info-mode adoc-mode rst-mode)
  "What major-modes to enable `mixed-pitch-mode' in with `visual-fill-column-mode'.")

(defvar +zenl-text-scale 1
  "The text-scaling level for `visual-fill-column-mode'.")

(use-package! visual-fill-column
  :commands visual-fill-column-mode
  :hook (visual-fill-column-mode . +zenl-visual-fill-column-h)
  :config
  ;; Text scaling is bugged: https://codeberg.org/joostkremers/visual-fill-column/issues/1
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t
        visual-fill-column-extra-text-width '(-5 . 5)
        visual-fill-column-adjust-for-text-scale nil))

(use-package! mixed-pitch
  :hook (visual-fill-column-mode . +zenl-enable-mixed-pitch-mode-h)
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'solaire-line-number-face))
