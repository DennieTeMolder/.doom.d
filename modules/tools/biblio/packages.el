;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "b85662081de98077f13f1a9fac03764702325d28")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "b85662081de98077f13f1a9fac03764702325d28"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "b85662081de98077f13f1a9fac03764702325d28"))
(when (featurep! :completion vertico)
  (package! bibtex-actions :pin "25e757fdfb905682b4579bdae1dd124011e02320"))

(package! citeproc :pin "91d7630de1ec61ff5d5e62c27d820207ec5bb1c6")
