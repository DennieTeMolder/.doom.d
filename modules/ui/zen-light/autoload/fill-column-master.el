;;; fill-column-master.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode fill-column-center-mode
  "Adjust left margin to center `fill-column' in the current window."
  :init-value nil :lighter nil :global nil
  :after-hook (fill-column-master-adjust)
  (when (not fill-column-visual-mode)
    (fill-column-master-manage-hooks fill-column-center-mode)))

;;;###autoload
(define-minor-mode fill-column-visual-mode
  "Adjust right margin to end the buffer at `fill-column' in the current window."
  :init-value nil :lighter nil :global nil
  :after-hook (fill-column-master-adjust)
  (when (not fill-column-center-mode)
    (fill-column-master-manage-hooks fill-column-visual-mode)))

(defun fill-column-master-manage-hooks (enable)
  (if enable
      (progn
        (add-hook 'window-configuration-change-hook #'fill-column-master-adjust 'append 'local)
        (add-hook 'display-line-numbers-mode-hook #'fill-column-master-adjust 'append 'local)
        (add-hook 'text-scale-mode-hook #'fill-column-master-adjust 'append 'local))
    (remove-hook 'window-configuration-change-hook #'fill-column-master-adjust 'local)
    (remove-hook 'display-line-numbers-mode-hook #'fill-column-master-adjust 'local)
    (remove-hook 'text-scale-mode-hook #'fill-column-master-adjust 'local)))

(defun fill-column-master-reset (window)
  "Reset the min-margins, fringes, and margins of WINDOW."
  (set-window-parameter window 'min-margins nil)
  (set-window-fringes window nil)
  (set-window-margins window nil))

(defun fill-column-master-adjust (&optional window)
  "Adjust WINDOW margins of `fill-column-center-mode' & `fill-column-visual-mode'."
  (or window (setq window (selected-window)))
  (with-selected-window window
    (fill-column-master-reset window)
    (when (or fill-column-center-mode fill-column-visual-mode)
      (let* ((scale (expt text-scale-mode-step text-scale-mode-amount))
             ;; +2 to match with `auto-fill-mode'
             (margin (- (window-width) (truncate (* (+ fill-column 2) scale)))))
        (when (< 0 margin)
          (let* ((left (if fill-column-center-mode (/ margin 2) 0))
                 (right (if fill-column-visual-mode (- margin left) 0)))
            (when display-line-numbers
              (let ((adjust (thread-last (line-number-display-width)
                                         (+ 2) (* scale) (truncate))))
                (if (zerop left)
                    (setq right (max 0 (- right adjust)))
                  (setq left (max 0 (- left adjust))))))
            (set-window-parameter window 'min-margins '(0 . 0))
            (set-window-margins window left right)))))))

;; HACK replace visual-fill-column mode
(unless (boundp 'visual-fill-column-mode)
  (defvaralias 'visual-fill-column-mode 'fill-column-visual-mode))
