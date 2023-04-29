;;; centered-buffer-mode.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode centered-buffer-mode
  "Adjust left margin to center `fill-column' in the current window."
  :init-value nil :lighter nil :global nil
  :after-hook (centered-buffer-adjust)
  (if centered-buffer-mode
      (progn
        (add-hook 'window-configuration-change-hook #'centered-buffer-adjust 'append 'local)
        (add-hook 'window-state-change-functions #'centered-buffer-adjust 'append 'local)
        (add-hook 'text-scale-mode-hook #'centered-buffer-adjust 'append 'local))
    (remove-hook 'window-configuration-change-hook #'centered-buffer-adjust 'local)
    (remove-hook 'window-state-change-functions #'centered-buffer-adjust 'local)
    (remove-hook 'text-scale-mode-hook #'centered-buffer-adjust 'local)))

(defun centered-buffer-reset (window)
  "Reset the min-margins, fringes, and margins of WINDOW."
  (set-window-parameter window 'min-margins nil)
  (set-window-fringes window nil)
  (set-window-margins window nil))

(defun centered-buffer-adjust (&optional window)
  "Adjust the margins of WINDOW according to `centered-buffer-mode'."
  (or window (setq window (selected-window)))
  (with-selected-window window
    (centered-buffer-reset window)
    (when centered-buffer-mode
      (let* ((scale (if (boundp 'text-scale-mode-amount)
                        (expt text-scale-mode-step text-scale-mode-amount)
                      1))
             (margin (- (window-width) (truncate (* fill-column scale))))
             (left (- (/ margin 2) (line-number-display-width))))
        (when (< 0 left)
          (set-window-parameter window 'min-margins '(0 . 0))
          (set-window-margins window left))))))
