;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "bb47f355b0da8518aa3fb516019120c14c8747c9")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "bb47f355b0da8518aa3fb516019120c14c8747c9"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "bb47f355b0da8518aa3fb516019120c14c8747c9"))
(when (featurep! :completion vertico)
  (package! bibtex-actions :pin "8130de52d9bf246d56bda0299ffea08108f0463b"))

(package! citeproc :pin "91d7630de1ec61ff5d5e62c27d820207ec5bb1c6")
