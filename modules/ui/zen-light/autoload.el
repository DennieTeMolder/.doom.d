;;; ui/zen-light/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +zenl-visual-fill-column-h ()
  "Toggles additional adjustments to accompany `visual-fill-column-mode'."
      (text-scale-set (if visual-fill-column-mode +zenl-text-scale 0))
      (display-line-numbers-mode (if visual-fill-column-mode -1 1)))

;;;###autoload
(defun +zenl-enable-mixed-pitch-mode-h ()
  "Enable `mixed-pitch-mode' when in `+zenl-mixed-pitch-modes'."
  (when (apply #'derived-mode-p +zenl-mixed-pitch-modes)
    (mixed-pitch-mode (if visual-fill-column-mode +1 -1))))
