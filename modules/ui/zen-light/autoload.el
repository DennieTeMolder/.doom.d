;;; ui/zen-light/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +zenl-enable-mixed-pitch-mode-h ()
  "Enable `mixed-pitch-mode' when in `+zenl-mixed-pitch-modes'."
  (when (apply #'derived-mode-p +zenl-mixed-pitch-modes)
    (mixed-pitch-mode (if visual-fill-column-mode +1 -1))))
