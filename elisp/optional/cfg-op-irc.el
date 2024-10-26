;;; cfg-op-irc.el --- configfuration for IRC via erc -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with IRC inside Emacs (via erc).
;; IRC:
;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
;;
;; Additional ERC examples:
;; https://www.reddit.com/r/emacs/comments/75i1bs/ircerc_setup/
;; https://gist.github.com/chumpage/1243771
;; https://github.com/bbatsov/emacs-dev-kit/blob/master/erc-config.el
;; http://home.thep.lu.se/~karlf/emacs.html#sec-26
;;
;; Spacemacs ERC layer:
;; https://github.com/syl20bnr/spacemacs/tree/develop/layers/%2Bchat/erc
;;
;;; Code:

(defun cfg/erc//servers (server-list)
  (dolist (s server-list)
    (setq s (cl-copy-list s))
    (apply (if
               (plist-get (cdr s) :ssl)
               (progn
                 (cl-remf (cdr s) :ssl)
                 'erc-tls)
             'erc)
           :server s)))

(defun cfg/erc-default-servers ()
  (interactive)
  (if erc-server-list
      (cfg/erc//servers erc-server-list)
    (message "You must define erc-server-list")))

(defun cfg/erc-chanlist ()
  "Show the list of current channels for the server."
  (interactive)
  (erc-cmd-LIST))

(defun cfg/erc-find-logfile ()
  "Find and open the current `erc-mode' buffers logfile."
  (interactive)
  (when (and (eq major-mode 'erc-mode) erc-log-mode)
    (find-file-other-window (erc-current-logfile))))

(use-package erc-image
  :ensure t
  :init
  (require 'erc-image)
  (add-to-list 'erc-modules 'image)
  ;; (setq erc-image-inline-rescale (quote window))
  (setq erc-image-inline-rescale 300)
  (setq erc-image-mode t))

(use-package erc-hl-nicks
  :ensure t
  :init
  (setq erc-hl-nicks-minimum-contrast-ratio 4.5))

(use-package erc-view-log
  :ensure t
  )

;; erc
(use-package erc
  ;;  :ensure t
  :init
  (require 'erc-services)
  (require 'erc-join)

  ;; buffers, tabs
  (setq erc-rename-buffers t)
  (setq erc-save-buffer-on-part t)
  (setq erc-save-queries-on-quit nil)
  (setq erc-server-coding-system '(utf-8 . utf-8))
  (setq erc-interpret-mirc-color t)

  ;; tls (optional)
  ;; (setq tls-program '("openssl s_client -connect %h:%p -tls1 -ign_eof -cert ~/path/to/you/cert.pem"))

  ;; logs
  ;;
  ;; The directory for log (*erc-log-channels-directory*) should be created by user:
  ;; the logs directory should exist, if ERC does not find the provided path, it will do nothing.

  (setq erc-log-channels-directory (expand-file-name "data/irc_logs/" user-emacs-directory))
  (setq erc-log-mode t) ;; optional...
  (setq erc-log-insert-log-on-open nil)
  (setq erc-log-matches-flag t)
  (setq erc-log-file-coding-system (quote utf-8))
  (setq erc-log-write-after-insert t)
  (setq erc-log-write-after-send t)
  (setq erc-generate-log-file-name-function (quote erc-generate-log-file-name-with-date))

  ;; nickserv/password
  (setq erc-prompt-for-nickserv-password nil)

  :config
  ;; timestamp
  ;; https://www.emacswiki.org/emacs/ErcStamp
  ;; Timestamp is enabled by default.
  ;; Optional format: `(setq erc-timestamp-format "%H:%M ")'
  (setq erc-insert-timestamp-function (quote erc-insert-timestamp-left))
  (setq erc-timestamp-use-align-to nil)
  (setq erc-timestamp-only-if-changed-flag nil)
  (setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (setq erc-nicklist-use-icons nil)

  ;; hide list
  (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK"))

  ;; https://www.emacswiki.org/emacs/ErcSpelling
  ;; https://www.emacswiki.org/emacs/ErcChannelTracking ==> more options!
  ;;
  ;; A quick hack so that query buffers are tracked as if everything contains your current nick, better reflecting the urgency of a private message.
  ;; Otherwise they just appear in the modeline with the default face and it can be easy to miss them.

  (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
    (if (erc-query-buffer-p)
	(setq ad-return-value (intern "erc-current-nick-face"))
      ad-do-it))

  (setq erc-track-exclude-server-buffer t) ;; do not track server buffer...
  (setq erc-track-position-in-mode-line t) ;; use global-mode-string as mode-line indicator...
  (setq erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
				  "324" "329" "332" "333" "353" "477"))

  ;; *WARNING - please check direct call to funcs!*
  ;; ! Example: `(setq erc-log-mode t)' will not work!
  ;; Only the direct call to erc-log-mode enables logging.
  ;; It can be confusing because unlike other configuration options, it is a call to a function.
  ;; As the documentation says:
  ;;
  ;; Erc Log Mode:
  ;; Non-nil if Erc-Log mode is enabled.
  ;; See the command `erc-log-mode' for a description of this minor mode.
  ;; Setting this variable directly does not take effect;
  ;; either customize it (see the info node `Easy Customization')
  ;; or call the function `erc-log-mode'.
  ;;
  ;; So in summary: setting variables with ... -mode directly does not take effect.
  ;; The best way will be add hooks:

  (add-hook 'erc-mode-hook
            (lambda ()
              (setq-local scroll-margin 1)
              (setq erc-track-mode t)
              (setq erc-track-minor-mode t)
              (erc-log-mode)
              (erc-hl-nicks-mode)
	      (erc-readonly-mode)
	      (erc-ring-mode)
	      (erc-services-mode)
	      (erc-pcomplete-mode)
	      (erc-menu-mode)
	      (erc-noncommands-mode)
	      (erc-list-mode)
	      (erc-match-mode)
	      (erc-button-mode)
	      (erc-move-to-prompt-mode)
	      (erc-services-mode)
	      (erc-autojoin-mode)
	      (erc-spelling-mode)
	      (erc-netsplit-mode)
	      (erc-networks-mode)
	      (erc-irccontrols-mode)

	      ;; *visual-line-mode* - I strongly prefer visual mode for ERC (over the standard erc-fill-mode):
	      ;;
	      ;; https://www.emacswiki.org/emacs/VisualLineMode
	      ;; https://www.youtube.com/watch?v=-XHz3B7NExQ
	      ;; https://emacs.stackexchange.com/questions/27278/soft-vs-hard-word-wrap-in-emacs
	      ;;
	      ;; There are also additional modes which could be checked: *auto-fill-mode* , *refill-mode* and *toggle-word-wrap*
	      ;; More informations ==> http://ergoemacs.org/emacs/emacs_long_line_wrap.html
	      ;;
	      ;; So in that situation (visual mode turned on): *erc-fill-mode* should be nil (disabled).
	      ;; Additional workaround: *erc-fill-column 1000* =>
	      ;; in this case this variable also will be disabled, even in case that erc-fill-mode will be turned on...
	      ;; Big number in that variable - like "1000" - in practice will disable this mode and will made it useless.

	      (visual-line-mode t)

	      ;;
	      ;; OPTIONAL - auto change fill column size (for erc-fill-mode):
	      ;; https://www.emacswiki.org/emacs/ErcFilling
	      ;;
	      ;;    (add-hook 'window-configuration-change-hook
	      ;;	   '(lambda ()
	      ;;	      (setq erc-fill-column (- (window-width) 2))))
	      ;;
	      ;;    (add-hook 'window-configuration-change-hook
	      ;;	   '(lambda ()
	      ;;	      (setq erc-fill-column "")))
	      ;;
	      ))

  (setq erc-fill-mode nil)
  (setq erc-fill-column 1000)
  (setq erc-fill-function (quote erc-fill-variable))
  (setq erc-fill-prefix "")
  (setq erc-fill-variable-maximum-indentation 0)
  (erc-update-modules)

  ;;  https://github.com/drewbarbs/erc-status-sidebar
  (defun cfg/erc-s-s/erc-window-reuse-condition (buf-name action)
    (with-current-buffer buf-name
      (if (eq major-mode 'erc-mode)
          ;; Don't override an explicit action
          (not action))))

  (add-to-list 'display-buffer-alist
               '(cfg/erc-s-s/erc-window-reuse-condition .
							;; NOTE: display-buffer-reuse-mode-window is only available in Emacs 26+
							(display-buffer-reuse-mode-window
							 (inhibit-same-window . t)
							 (inhibit-switch-frame . t)
							 (mode . erc-mode))))
  ;; Please create correct "lo-erc.el" file inside ~/.emacs.d/data/local/lo-erc.el (or other emacs dir)
  ;; Please add below variables and data inside this file:
  ;; (setq erc-server-list ...
  ;; (setq erc-autojoin-channels-alist ...
  ;; (setq erc-nickserv-passwords ...
  ;; (setq erc-spelling-dictionaries ...
  ;; (setq erc-track-exclude ...
  ;; (setq-default erc-ignore-list ...

  (if (file-readable-p (expand-file-name "data/local/lo-erc.el" user-emacs-directory))
      (require 'lo-erc) ; if true, load additional variables for erc
					; if false, then message with "WARNING" will appear during initialization of erc:
    (message "WARNING! File data/local/lo-erc.el inside your emacs.d is not readable (or not exist)! Please create it and add correct erc options!"))

  ;; load keybindings from general.el framework:
  (require 'cfg-gen-op-irc)
  ;; end of use-package
  )

(provide 'cfg-op-irc)
;;; cfg-op-irc.el ends here
