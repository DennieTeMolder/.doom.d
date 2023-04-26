;;; config.el -*- lexical-binding: t; -*-

(use-package! atomic-chrome
  :commands atomic-chrome-start-server
  :config
  (setq atomic-chrome-buffer-open-style 'full
        atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("overleaf\\.com" . latex-mode)
          ("azuredatabricks\\.net" . python-mode))))
