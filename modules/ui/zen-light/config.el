;;; ui/zen-light/config.el -*- lexical-binding: t; -*-

(defvar +zenl-reading-modes
  '(org-mode LaTeX-mode markdown-mode gfm-mode helpful-mode Info-mode adoc-mode rst-mode)
  "Major-modes with additional reading optimizations for `+zen-light-toggle'.
Most importantly this controls usage of `mixed-pitch-mode'.")

(defvar +zenl-reading-scale 1
  "Value to use for `text-scale-set' in `+zenl-reading-modes'.")

;; HACK alias `visual-fill-column-mode' for `+word-wrap-mode' interop.
(unless (fboundp 'visual-fill-column-mode)
  (defalias 'visual-fill-column-mode #'fill-column-visual-mode)
  (defvaralias 'visual-fill-column-mode 'fill-column-visual-mode))

(use-package! mixed-pitch
  :defer t
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces
            'solaire-line-number-face
            'org-date
            'org-special-keyword
            'org-property-value
            'org-tag
            'org-todo-keyword-todo
            'org-todo-keyword-habt
            'org-todo-keyword-done
            'org-todo-keyword-wait
            'org-todo-keyword-kill
            'org-todo-keyword-outd
            'org-todo
            'org-done
            'org-table-row
            'org-modern-tag
            'org-modern-done
            'org-modern-todo
            'org-modern-habit
            'org-modern-label
            'org-modern-symbol
            'org-modern-priority
            'org-modern-block-name
            'org-modern-date-active
            'org-modern-time-active
            'org-modern-radio-target
            'org-modern-date-inactive
            'org-modern-time-inactive
            'org-modern-horizontal-rule
            'org-modern-internal-target
            'org-modern-progress-complete
            'org-modern-progress-incomplete
            'font-lock-comment-face))
