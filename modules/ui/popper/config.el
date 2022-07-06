;;; ui/popper/config.el -*- lexical-binding: t; -*-

(use-package! popper
  :commands popper-mode
  :init (add-hook! 'doom-init-ui-hook :append #'popper-mode)
  :config
  (setq popper-mode-line nil ; hides mode-line in popups
        popper-reference-buffers
        '("\\*Async Shell Command\\*"
          "\\*Local variables\\*"
          "\\*info\\*"
          "^\\*Customize"
          "^\\*Warnings"
          "^\\*Backtrace"
          "^\\*Calc"
          "^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*"
          "^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
          "^\\*\\(?:doom[: ]\\|Pp E\\)"
          "^\\*\\([Hh]elp\\|Apropos\\|refs\\)"
          "^\\*\\(?:Wo\\)?Man "
          (lambda (buf) (with-current-buffer buf
                     (derived-mode-p 'comint-mode 'compilation-mode)))))

  ;; Unbind `+default/search-project' (also bound to "SPC s p")
  (map! :leader "/" nil)
  (map! :leader :prefix ("/" . "popup")
        :desc "Show/hide" "/" #'popper-toggle-latest
        :desc "Next" "n" #'popper-cycle
        :desc "Kill" "k" #'my/popper-kill-latest-popup-keep-open
        :desc "Quit" "q" #'popper-kill-latest-popup
        :desc "Toggle popup/buffer" "t" #'my/popper-toggle-type
        :desc "Raise" "r" #'my/popper-raise-popup)

  (popper-mode +1))

(use-package! popper-echo
  :after popper
  :defer 3
  :config
  (defun my-popper-echo-transform (str)
    "Removes apostrophes and truncates descriptions before \":\" from STR."
    (replace-regexp-in-string  "^\\*\\|\\*$\\|\\(.\\).*\\(:\\)[[:space:]]?"
                               "\\1\\2"
                               str))

  (setq popper-echo-transform-function #'my-popper-echo-transform)

  (popper-echo-mode +1))

;;;###package windmove
;; Steal hack from popup module to make treemacs accessible using evil
(defadvice! +windmove-ignore-window-parameters-a (fn &rest args)
  "Allow *interactive* commands to enter windows with the `no-other-window' parameter."
  :around '(windmove-up windmove-down windmove-left windmove-right)
  (letf! (defun windmove-find-other-window (dir &optional arg window)
           (window-in-direction
            (pcase dir (`up 'above) (`down 'below) (_ dir))
            window t arg windmove-wrap-around t))
    (apply fn args)))
