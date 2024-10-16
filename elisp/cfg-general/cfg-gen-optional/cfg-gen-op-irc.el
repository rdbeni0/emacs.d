;;; cfg-gen-op-irc.el --- general.el for irc -*- lexical-binding: t -*-

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

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "aee"  '(erc :which-key "erc")
 "aeD"  '(cfg/erc-default-servers :which-key "erc-default-servers")
 "aeE"  '(erc-tls :which-key "erc-tls")
 "ael"  '(:ignore t :which-key "erc-view-log-mode")
 "aelf" '(cfg/erc-find-logfile :which-key "erc-find-logfile")
 "aelv" '(erc-view-log-mode :which-key "erc-view-log-mode")
 "aelr" '(erc-view-log-reload-file :which-key "erc-view-log-reload-file")
 "ael>" '(erc-view-log-next-mention :which-key "erc-view-log-next-mention")
 "ael<" '(erc-view-log-previous-mention :which-key "erc-view-log-previous-mention"))

(provide 'cfg-gen-op-irc)
;;; cfg-gen-op-irc.el ends here
