;;; doctor.el -*- lexical-binding: t; -*-

;; Jinx
(unless (executable-find "aspell")
  (warn! "Couldn't find aspell command. Jinx spell checking might be impaired"))

(unless (executable-find "pkg-config")
  (warn! "Couldn't find pkg-config command. Jinx module won't compile"))

(unless (condition-case nil
            (process-lines "pkg-config" "--cflags" "--libs" "enchant-2")
          (error nil))
  (warn! "Couldn't find libenchant2 headers. Jinx module won't compile"))

(unless (fboundp 'module-load)
  (warn! "Your emacs wasn't built with dynamic modules support. Jinx module won't compile"))

;; Flycheck
(unless (executable-find "proselint")
  (warn! "Couldn't find proselint command. Linting of text-mode buffers is impaired"))

;; Ox-odt
(unless (executable-find "soffice")
  (warn! "Couldn't find soffice command. Org to ODT export will be impaired."))

(unless (executable-find "dvipng")
  (warn! "Couldn't find dvipng command. Org to ODT export of LaTeX fragments is impaired."))

;; Ox-latex/auctex
(unless (executable-find "latexmk")
  (warn! "Couldn't find latexmk command. LaTeX exports might be impaired."))

;; Org-link-as-png
(unless (if (eq dtm-org-link-convert-executable 'executable-find)
            (executable-find "convert")
          (file-executable-p dtm-org-link-convert-executable))
  (warn! "Couldn't find imagemagick/convert command. Org-link-as-png won't work."))
