;;; ui/zen-light/config.el -*- lexical-binding: t; -*-

(defvar +zenl-mixed-pitch-modes '(adoc-mode rst-mode markdown-mode org-mode)
  "What major-modes to enable `mixed-pitch-mode' in with `visual-fill-column-mode'.")

(defvar +zenl-text-scale 1
  "The text-scaling level for `visual-fill-column-mode'.")

(use-package! visual-fill-column
  :commands visual-fill-column-mode
  :config
  ;; Text scaling is bugged: https://codeberg.org/joostkremers/visual-fill-column/issues/1
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t
        visual-fill-column-extra-text-width '(-5 . 5)
        visual-fill-column-adjust-for-text-scale nil)

  (add-hook! 'visual-fill-column-mode-hook
    (defun +zenl-visual-fill-column-h ()
        (display-line-numbers-mode (if visual-fill-column-mode -1 +1))
        (text-scale-set (if visual-fill-column-mode +zenl-text-scale 0))
        (when (and (eq major-mode 'org-mode)
                   (featurep! :lang org))
          (+org-pretty-mode (if visual-fill-column-mode +1 -1)))
        (visual-fill-column-adjust))))

(use-package! mixed-pitch
  :hook (visual-fill-column-mode . +zenl-enable-mixed-pitch-mode-h)
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'solaire-line-number-face))
