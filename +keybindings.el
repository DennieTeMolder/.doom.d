;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-

;;* Global
(map! "C-s" #'ctrlf-forward-default
      "C-l" #'+nav-flash/blink-cursor

      :i "C-x C-s" #'dtm-tempel-complete-always
      :v "C-x C-s" #'tempel-insert

      :desc "Next window"              :n "] w" #'evil-window-next
      :desc "Previous window"          :n "[ w" #'evil-window-prev
      :desc "Next persp/workspace"     :n "] p" #'+workspace:switch-next
      :desc "Previous persp/workspace" :n "[ p" #'+workspace:switch-previous

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

;;* <leader>
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

      ;; Insert
      :desc "snippet" "i s" #'tempel-insert

      ;; Notes roam
      (:prefix "n r"
       :desc "Open index"        "o"   #'dtm/org-roam-open-index
       :desc "Schedule headline" "d s" #'dtm/org-roam-dailies-schedule-time)

      ;; Open
      (:prefix "o"
       :desc "Current dir"    "-" #'dirvish
       :desc "Current dir"    "d" #'dirvish
       :desc "Start debugger" "D" #'+debugger/start
       :desc "Folder sidebar" "s" #'dtm/dirvish-side
       :desc "Vterm here"     "T" #'vterm)

      ;; Toggles
      (:prefix "t"
       :desc "Auto linebreaks"     "a" #'auto-fill-mode
       :desc "GhostText server"    "G" #'dtm/atomic-chrome-toggle-server
       :desc "Keycast mode"        "k" #'keycast-mode
       :desc "Keycast log mode"    "K" #'keycast-log-mode
       :desc "Log interactions"    "L" #'dtm/interaction-log-mode-w-buffer
       :desc "Margin (left)"       "M" #'dtm/window-toggle-left-margin
       :desc "Recommend theme"     "R" #'dtm/load-recommended-theme
       :desc "Smooth scrolling"    "S" #'good-scroll-mode
       :desc "Trash deleted files" "T" #'dtm/toggle-trash-delete)

      ;; Window management
      (:prefix "w"
       :desc "Adjust windows hydra"  "a" #'+hydra/window-nav/body)

      ;; Popups
      "/" nil ; Unbind `+default/search-project' (also bound to "SPC s p")
      (:prefix ("/" . "popup")
       :desc "Show/hide popup" "/" #'+popup/toggle
       :desc "Buffer to popup" "b" #'+popup/buffer
       :desc "Kill popup"      "k" #'dtm/popup-kill
       :desc "Popup to buffer" "r" #'dtm/popup-raise))

;;* Custom modules
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
