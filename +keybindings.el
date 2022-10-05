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
      (:prefix "b"
       :desc "Move buffer to workspace" "TAB" #'dtm/buffer-move-to-workspace
                                        "D"   #'kill-buffer-and-window
       :desc "Show undo history"        "h"   #'vundo
       :desc "Switch orphan buffer"     "o"   #'dtm/switch-orphan-buffer)

      ;; Window management
      (:prefix "w"
       :desc "Adjust windows hydra"  "a" #'+hydra/window-nav/body
       :desc "Enlarge double height" "e" #'dtm/window-double-height
       :desc "Halve height"          "E" #'dtm/window-half-height)

      ;; Popups
      "/" nil ; Unbind `+default/search-project' (also bound to "SPC s p")
      (:prefix ("/" . "popup")
       :desc "Show/hide popup" "/" #'+popup/toggle
       :desc "Buffer to popup" "b" #'+popup/buffer
       :desc "Kill popup"      "k" #'dtm/popup-kill
       :desc "Popup to buffer" "r" #'dtm/popup-raise
       :desc "Restore popup"   "u" #'+popup/restore)

      ;; Open
      (:prefix "o"
       :desc "Folder sidebar" "s" #'dtm/dirvish-side
       :desc "Vterm here"     "T" #'vterm)

      ;; Toggles
      (:prefix "t"
       :desc "Auto linebreaks"     "a" #'auto-fill-mode
       :desc "GhostText server"    "G" #'dtm/atomic-chrome-toggle-server
       :desc "Interaction logging" "I" #'dtm/interaction-log-mode-w-buffer
       :desc "Left margin"         "L" #'dtm/window-toggle-left-margin
       :desc "Smooth scrolling"    "S" #'good-scroll-mode
       :desc "Trash deleted files" "T" #'dtm/toggle-trash-delete)

      ;; Roam
      (:prefix "n r"
       :desc "Open index"        "o"   #'dtm/org-roam-open-index
       :desc "Schedule headline" "d s" #'dtm/org-roam-dailies-schedule-time))

;;; Custom modules
(map! :leader
      ;; :when (modulep! :ui zen-light)
       :desc "Zen writing mode"     "t z" #'visual-fill-column-mode

      ;; :when (modulep! :ui popper)
       ;;"/" nil ; Unbind `+default/search-project' (also bound to "SPC s p")
       ;; (:prefix ("/" . "popup")
       ;;  :desc "Show/hide"           "/" #'popper-toggle-latest
       ;;  :desc "Next"                "n" #'popper-cycle
       ;;  :desc "Kill"                "k" #'+popper/kill-latest-popup-keep-open
       ;;  :desc "Quit"                "q" #'popper-kill-latest-popup
       ;;  :desc "Toggle popup/buffer" "t" #'+popper/toggle-type
       ;;  :desc "Raise"               "r" #'+popper/raise-popup)
       )
