;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-

;;; Global
(map! "C-s" #'isearch-forward-word
      "C-l" #'+nav-flash/blink-cursor

      ;; Make "Z" bindings only kill buffers not the session
      :n "ZQ" #'kill-buffer-and-window
      :n "ZZ" #'doom/save-and-kill-buffer
      (:map with-editor-mode-map
       :n "ZQ" #'with-editor-cancel
       :n "ZZ" #'with-editor-finish)

      ;; Use mouse buttons to go forward/backward trough window configs
      :n [mouse-8] #'winner-undo
      :n [mouse-9] #'winner-redo
      (:map Info-mode-map
       :n [mouse-8] #'Info-history-back
       :n [mouse-9] #'Info-history-forward))

;;; <leader>
(map! :leader
      ;; Spacemacs style M-x
      ;; Old SPC SPC binding (projectile find file) is available under "SPC p f"
      ;; This frees up the "SPC :" to be a fallback for evil-ex
      :desc "M-x"             "SPC" #'execute-extended-command
      :desc "Evil ex command" ":"   #'evil-ex

      ;; General
      :desc "Indentation hydra"   ">" #'indent-tools-hydra/body
      :desc "Repeat last command" "r" #'repeat

      ;; Buffers
      :desc "Move buffer to workspace" "b TAB" #'my/buffer-move-to-workspace-prompt
                                       "b D"   #'kill-buffer-and-window
      :desc "Show undo history"        "b h"   #'vundo

      ;; Window management
      (:prefix "w"
       :desc "Adjust windows hydra"  "a" #'+hydra/window-nav/body
       :desc "Enlarge double height" "e" #'my/window-double-height
       :desc "Halve height"          "E" #'my/window-half-height)

      ;; Toggles
      (:prefix "t"
       :desc "Auto linebreaks"     "a" #'auto-fill-mode
       :desc "GhostText server"    "G" #'my/atomic-chrome-toggle-server
       :desc "Log interactions"    "L" #'my/interaction-log-mode-w-buffer
       :desc "Smooth scrolling"    "S" #'good-scroll-mode
       :desc "Trash deleted files" "T" #'my/toggle-trash-delete)

      ;; Roam
      (:prefix "n r"
       :desc "Open index"        "o"   #'my/org-roam-open-index
       :desc "Schedule headline" "d s" #'my/org-roam-dailies-schedule-time))

;;; Custom modules
(map! :leader
      (:when (featurep! :ui zen-light)
       :desc "Zen writing mode"     "t z" #'visual-fill-column-mode)

      (:when (featurep! :ui popper)
       "/" nil ; Unbind `+default/search-project' (also bound to "SPC s p")
       (:prefix ("/" . "popup")
        :desc "Show/hide"           "/" #'popper-toggle-latest
        :desc "Next"                "n" #'popper-cycle
        :desc "Kill"                "k" #'+popper/kill-latest-popup-keep-open
        :desc "Quit"                "q" #'popper-kill-latest-popup
        :desc "Toggle popup/buffer" "t" #'+popper/toggle-type
        :desc "Raise"               "r" #'+popper/raise-popup)))