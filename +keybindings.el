;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-

;;; <leader>
(map! :leader
      (:when (featurep! :ui zen-light)
       :desc "Zen writing mode"     "t z" #'visual-fill-column-mode)
      (:when (featurep! :ui popper)
       "/" nil ; Unbind `+default/search-project' (also bound to "SPC s p")
       (:prefix ("/" . "popup")
        :desc "Show/hide"           "/" #'popper-toggle-latest
        :desc "Next"                "n" #'popper-cycle
        :desc "Kill"                "k" #'my/popper-kill-latest-popup-keep-open
        :desc "Quit"                "q" #'popper-kill-latest-popup
        :desc "Toggle popup/buffer" "t" #'my/popper-toggle-type
        :desc "Raise"               "r" #'my/popper-raise-popup)))
