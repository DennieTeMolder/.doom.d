;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "1bb81d77e08296a50de7ebfe5cf5b0c715b7f3d6")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "1bb81d77e08296a50de7ebfe5cf5b0c715b7f3d6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "1bb81d77e08296a50de7ebfe5cf5b0c715b7f3d6"))
(when (featurep! :lang org)
  (package! org-ref :pin "052a176b4cc1c080376c183e5e02ab37cb1f0f0a"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "f3d8add7f7decd624a1fabcec7a768ae563f946a"))
