;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;;; Doom modifications
;; Evil-escape will exit insert state after a key sequence ("jk")
;; Disable it as I am not using it and might create typing delay
(package! evil-escape :disable t)

;; Byte compiling tablist creates an error in `pdf-annot-list-annotations'
;; https://github.com/vedang/pdf-tools/issues/89
(package! tablist :recipe (:build (:not compile)))

;; Replace the stale ess-R-data-view with the newer ess-view-data
;; Pending in: https://github.com/doomemacs/doomemacs/pull/6455
(package! ess-R-data-view :disable t)
(package! ess-view-data :pin "aab369f84cadebbbca7c3825741d643ebeb6b722")

;;; Custom packages
(package! org-modern)
(package! interaction-log)
(package! good-scroll)
(package! atomic-chrome)
(package! vundo)
(package! trashed)
(package! dired-du)
(package! xr)
(package! info-colors)
(package! org-appear :pin "60ba267c5da336e75e603f8c7ab3f44e6f4e4dac"
  :recipe (:host github :repo "awth13/org-appear"))
(package! indent-tools :pin "9df62e1bebf93d2dd3ab572daf2704a3f34db859"
  :recipe (:host github :repo "DennieTeMolder/indent-tools"))
