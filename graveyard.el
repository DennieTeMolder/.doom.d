;;; backlog.el -*- lexical-binding: t; -*-
;; This file contains unused functions I could not bear to delete

;;; Python
(defun my/python-shell-send-statment-and-step ()
  "Send statement to python shell and move to next"
  (interactive)
  (python-shell-send-region
   (save-excursion (python-nav-beginning-of-statement))
   (save-excursion (python-nav-end-of-statement)))
  (python-nav-forward-statement))

(defun my/python-shell-send-block-and-step ()
  "Send block to python shell and move to next statement"
  (interactive)
  (python-shell-send-region
   (save-excursion (python-nav-beginning-of-block))
   (save-excursion (python-nav-end-of-block)))
  (python-nav-end-of-block)
  (python-nav-forward-statement))

(defun my/python-send-current-and-step ()
  "Sends statement under point to python shell, if the statement starts a code
block, send the entire code block."
  (interactive)
  ;; Check for region, start of block, or other and act accordingly
  (cond ((region-active-p)
         (call-interactively #'python-shell-send-region))
        ((python-info-statement-starts-block-p)
         (call-interactively #'my/python-shell-send-block-and-step))
        (t
         (call-interactively #'my/python-shell-send-statment-and-step))))
