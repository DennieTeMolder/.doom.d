;;; autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun dtm/atomic-chrome-toggle-server ()
  (interactive)
  (if (bound-and-true-p global-atomic-chrome-edit-mode)
      (progn
        (atomic-chrome-stop-server)
        (message "Stopped GhostText Server"))
    (progn
      (atomic-chrome-start-server)
      (message "Started GhostText Server"))))
