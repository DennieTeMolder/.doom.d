;;; ui/zen-light/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +zenl-display-line-numbers-previous-state t)

;;;###autoload
(defun +zenl/hide-line-numbers ()
  "Hides line numbers while recording `+zenl-display-line-numbers-previous-state'"
  (interactive)
  (setq-local +zenl-display-line-numbers-previous-state display-line-numbers)
  (setq-local display-line-numbers nil))

;;;###autoload
(defun +zenl/restore-line-numbers ()
  "Restores line numbers to `+zenl-display-line-numbers-previous-state'"
  (interactive)
  (setq-local display-line-numbers +zenl-display-line-numbers-previous-state))
