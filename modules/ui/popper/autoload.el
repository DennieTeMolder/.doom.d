;;; ui/popper/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/popper-toggle-type ()
  "Extension of `popper-toggle-type' that also works on side-windows"
  (interactive)
  (let ((win (selected-window))
        (buf (current-buffer)))
    ;; If current buffer is in a side-window but not a popup, give it a new window
    (if (and (window-parameter win 'window-side)
             (not popper-popup-status))
        (progn
          (delete-window win)
          (pop-to-buffer buf))
      (popper-toggle-type buf))))

;;;###autoload
(defun my/popper-raise-popup ()
  "Raise open popup to its own dedicated window"
  (interactive)
  (let ((pop-win (caar popper-open-popup-alist)))
    (unless pop-win
      (user-error "No open popups!"))
    (with-selected-window pop-win
      (popper-toggle-type))))

;;;###autoload
(defun my/popper-kill-latest-popup-keep-open ()
  "Kill latest popup but keep popup window open"
  (interactive)
  (popper-kill-latest-popup)
  (popper-open-latest))
