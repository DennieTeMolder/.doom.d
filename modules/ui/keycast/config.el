;;; ui/keycast/config.el -*- lexical-binding: t; -*-

(use-package! keycast
  :commands keycast-mode keycast-log-mode
  :config
  (defvar keycast-mode-string '("" keycast-mode-line " ")
    "Element to insert into `global-mode-string' when `keycast-mode' is active")

  ;; Redefine keycast-mode to be compatible with doom-modeline
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the doom-modeline."
    :group 'mode-line
    :global t
    (if keycast-mode
        (progn
          (add-to-list 'global-mode-string keycast-mode-string)
          (add-hook 'pre-command-hook 'keycast--update t))
      (setq global-mode-string (delete keycast-mode-string global-mode-string))
      (remove-hook 'pre-command-hook 'keycast--update)))

  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
      :height 0.9)
    '(keycast-key :inherit custom-modified
      :weight bold)))
