;;; ui/zen-light/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +zenl-visual-fill-column-h ()
  "Toggles additional adjustments to accompany `visual-fill-column-mode'."
  (unless visual-fill-column-width
    (setq-local visual-fill-column-width (+ fill-column 10)))
  (display-line-numbers-mode (if visual-fill-column-mode -1 +1)))

;;;###autoload
(defun +zenl-enable-mixed-pitch-mode-h ()
  "Enable `mixed-pitch-mode' and `+zenl-reading-scale' in `+zenl-reading-modes'."
  (when (apply #'derived-mode-p +zenl-reading-modes)
    (let ((mode-enabled visual-fill-column-mode))
      (text-scale-set (if mode-enabled +zenl-reading-scale 0))
      (mixed-pitch-mode (if mode-enabled +1 -1))
      (when mode-enabled (visual-fill-column-adjust)))))
