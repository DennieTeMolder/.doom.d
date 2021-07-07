;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "9f6ea920a49457d85096caa0e61f086a42b2908e")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "9f6ea920a49457d85096caa0e61f086a42b2908e"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "9f6ea920a49457d85096caa0e61f086a42b2908e"))
(when (featurep! :lang org)
  (package! org-ref :pin "8aa2bb45268f660956151547533689d4ec30378d"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "f7b5be2ce0b43dd4d842484fc0ec37fdc8526907"))
