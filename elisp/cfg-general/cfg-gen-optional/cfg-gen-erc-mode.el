;; general-erc-mode:
;; https://www.emacswiki.org/emacs/ErcBindings

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'erc-mode-map
 :major-modes 'erc-mode
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "b" '(erc-iswitchb :which-key "iswitchb")
 "a" '(erc-input-action :which-key "input-action")
 "c" '(cfg/erc-chanlist :which-key "chanlist")
 "j" '(erc-join-channel :which-key "join-channel")
 "n" '(erc-channel-names :which-key "channel-names")
 "Q" '(erc-quit-server :which-key "quit-server")
 "f" '(cfg/erc-find-logfile :which-key "find-logfile")
 "o" '(erc-status-sidebar-open :which-key "status-sidebar-open")
 "i" '(erc-status-sidebar-kill :which-key "status-sidebar-kill")
 "." '(quoted-insert :which-key "quoted-insert")
 "," '(erc-cmd-QUERY :which-key "cmd-QUERY-private-msg")
 "g" '(erc-image-mode :which-key "toggle-image-mode"))

(provide 'cfg-gen-erc-mode)
;;; cfg-gen-erc-mode.el ends here
