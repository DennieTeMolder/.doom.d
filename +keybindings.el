;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-

;;* Global
(map! :n  "C-<right>" #'dtm/move-splitter-right
      :n  "C-<left>"  #'dtm/move-splitter-left
      :n  "C-<down>"  #'dtm/move-splitter-down
      :n  "C-<up>"    #'dtm/move-splitter-up
      :g  "C-s"       #'ctrlf-forward-default
      :gn "C-l"       #'+nav-flash/blink-cursor

      :i "C-x C-k" #'dtm/cape-keyword-dict
      :i "C-x C-s" #'tempel-complete
      :i "C-x s"   #'dtm/spell-correct-previous

      ;; Make "Z" bindings only kill buffers not the session
      :n "ZQ" #'kill-buffer-and-window
      :n "ZZ" #'doom/save-and-kill-buffer

      ;; Use mouse buttons to go forward/backward through window configs
      :n [mouse-8] #'winner-undo
      :n [mouse-9] #'winner-redo)

;;* <leader>
(map! :leader
      ;; Spacemacs style M-x
      ;; Old SPC SPC binding (`projectile-find-file') is available under "SPC p f"
      ;; This frees up the "SPC :" to be a fallback for evil-ex
      :desc "M-x"             "SPC" #'execute-extended-command
      :desc "Evil ex command" ":"   #'evil-ex

      ;; General
      :desc "Repeat last command" "r" #'repeat

      ;; Buffers
      (:prefix "b"
       :desc "Move buffer to workspace" "TAB" #'dtm/buffer-move-to-workspace
       :desc "Kill buffer and window"   "D"   #'kill-buffer-and-window
       :desc "Switch hidden buffer"     "h"   #'dtm/switch-orphan-buffer
       :desc "Other window move"        "o"   #'dtm/buffer-move-to-window
       :desc "Undo history"             "u"   #'vundo
       :desc "Save buffer as root"      "U"   #'doom/sudo-save-buffer)

      ;; Code
      (:prefix "c"
       :desc "Describe error checker" "X" #'dtm/diagnostics-describe)

      ;; Insert
      :desc "snippet" "i s" #'tempel-insert

      ;; Notes roam
      (:prefix "n"
       :desc "Bibliography"      "b"     #'citar-open
       :desc "Open index"        "r o"   #'dtm/org-roam-open-index
       :desc "Schedule headline" "r d s" #'dtm/org-roam-dailies-schedule-time)

      ;; Open
      (:prefix "o"
       ;; :desc "ChatGPT"                 "c" #'gptel
       ;; :desc "ChatGPT dedicated"       "C" #'dtm/gptel-new-chat
       :desc "Folder sidebar"          "s" #'dtm/dirvish-side
       :desc "Find file in sidebar"    "S" #'+dired/dirvish-side-and-follow
       :desc "Vterm here"              "T" #'vterm)

      ;; Project
      :desc "List project todos" "p t" #'magit-todos-list

      ;; Toggles
      (:prefix "t"
       :desc "Auto line-breaks"   "a" #'auto-fill-mode
       :desc "Line numbers"       "l" #'dtm/toggle-line-numbers
       :desc "Smooth scrolling"   "S" #'pixel-scroll-precision-mode
       :desc "Trash/delete files" "T" #'dtm/toggle-trash-delete)

      ;; Window management
      (:prefix "w"
       :desc "Kill window and buffer" "D" #'kill-buffer-and-window
       :desc "Window as frame"        "F" #'tear-off-window
                                      "e" #'evil-window-prev
                                      "s" #'dtm/split-window-optimally
                                      "S" #'evil-window-split
                                      "T" #'transpose-frame)

      ;; Popups
      "/" nil       ; Unbind `+default/search-project' (also bound to "SPC s p")
      (:prefix ("/" . "popup")
       :desc "Show/hide popup" "/" #'+popup/toggle
       :desc "Switch popup"    "." #'dtm/switch-popup-buffer
       :desc "Buffer to popup" "b" #'+popup/buffer
       :desc "Kill popup"      "k" #'dtm/popup-kill
       :desc "Kill other"      "o" #'dtm/popup-only
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
