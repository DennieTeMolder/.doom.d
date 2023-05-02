;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-

;;* Global
(map! :n "C-<right>" #'dtm/move-splitter-right
      :n "C-<left>"  #'dtm/move-splitter-left
      :n "C-<down>"  #'dtm/move-splitter-down
      :n "C-<up>"    #'dtm/move-splitter-up
      :g "C-s"       #'ctrlf-forward-default
      :n "C-l"       #'+nav-flash/blink-cursor

      :i "C-x C-s" #'dtm/tempel-complete-always
      :v "C-x C-s" #'tempel-insert

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
      :desc "Repeat last command" "r" #'repeat

      ;; Buffers
      (:prefix "b"
       :desc "Move buffer to workspace" "TAB" #'dtm/buffer-move-to-workspace
                                        "D"   #'kill-buffer-and-window
       :desc "Show undo history"        "h"   #'vundo
       :desc "Switch orphan buffer"     "o"   #'dtm/switch-orphan-buffer)

      ;; Help
      "h t" #'dtm/consult-theme

      ;; Insert
      :desc "snippet" "i s" #'tempel-insert

      ;; Notes roam
      (:prefix "n r"
       :desc "Open index"        "o"   #'dtm/org-roam-open-index
       :desc "Schedule headline" "d s" #'dtm/org-roam-dailies-schedule-time)

      ;; Open
      (:prefix "o"
       :desc "Current dir"             "-" #'dirvish-dwim
       :desc "ChatGPT"                 "c" #'gptel
       :desc "ChatGPT dedicated"       "C" #'dtm/gptel-new-chat
       :desc "Current dir (maximised)" "d" #'dirvish
       :desc "Start debugger"          "D" #'+debugger/start
       :desc "Folder sidebar"          "s" #'dtm/dirvish-side
       :desc "Vterm here"              "T" #'vterm)

      ;; Toggles
      (:prefix "t"
       :desc "Auto line-breaks"    "a" #'auto-fill-mode
       :desc "Line numbers"        "l" #'dtm/toggle-line-numbers
       :desc "Smooth scrolling"    "S" #'good-scroll-mode
       :desc "Trash deleted files" "T" #'dtm/toggle-trash-delete)

      ;; Window management
      (:prefix "w"
       :desc "window-as-frame" "F" #'tear-off-window
                               "e" #'evil-window-prev
                               "s" #'dtm/split-window-optimally
                               "S" #'evil-window-split
                               "T" #'transpose-frame)

      ;; Popups
      "/" nil ; Unbind `+default/search-project' (also bound to "SPC s p")
      (:prefix ("/" . "popup")
       :desc "Show/hide popup" "/" #'+popup/toggle
       :desc "Switch popup"    "." #'dtm/switch-popup-buffer
       :desc "Buffer to popup" "b" #'+popup/buffer
       :desc "Kill popup"      "k" #'dtm/popup-kill
       :desc "Popup to buffer" "r" #'dtm/popup-raise))

;;* Custom modules
(map! :leader
      ;; :when (modulep! :ui zen-light)
      :desc "Zen/focus mode"     "t z" #'+zen-light-toggle

      ;; :when (modulep! :ui keycast)
      ;; :desc "Keycast mode"        "t k" #'keycast-mode
      ;; :desc "Keycast log mode"    "t K" #'keycast-log-mode

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
