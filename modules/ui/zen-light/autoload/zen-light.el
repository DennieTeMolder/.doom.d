;;; zen-light.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +zen-light-toggle (&optional arg)
  "Toggle zen/focus mode. Uses `+zenl-reading-modes' & `+zenl-reading-scale'."
  (interactive)
  (setq arg (if (numberp arg)
                (< 0 arg)
              (not (bound-and-true-p fill-column-center-mode))))
  (when (apply #'derived-mode-p +zenl-reading-modes)
    (text-scale-set (if arg +zenl-reading-scale 0))
    (mixed-pitch-mode (if arg 1 0)))
  (if arg
      (setq display-line-numbers nil)
    (unless display-line-numbers
      (setq display-line-numbers display-line-numbers-type)))
  (when (boundp 'vi-tilde-fringe-mode)
    (vi-tilde-fringe-mode (if arg 0 1)))
  (fill-column-center-mode (if arg 1 0)))
