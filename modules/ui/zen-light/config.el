;;; ui/zen-light/config.el -*- lexical-binding: t; -*-

(defvar +zenl-reading-modes
  '(org-mode LaTeX-mode markdown-mode gfm-mode helpful-mode Info-mode adoc-mode rst-mode)
  "Major-modes with additional reading optimizations for `+zen-light-toggle'.
Most importantly this controls usage of `mixed-pitch-mode'.")

(defvar +zenl-reading-scale 1
  "Value to use for `text-scale-set' in `+zenl-reading-modes'.")

(use-package! mixed-pitch
  :defer t
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'solaire-line-number-face))
