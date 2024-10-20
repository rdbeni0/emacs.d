;;; cfg-tramp-ssh.el --- configfuration for tramp, sudo and ssh -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Additional configuration for TRAMP and ssh connections.
;;
;;; Code:

;;;;;; tramp-term
;; Note: a large part of the data is also directly embedded in the "tramp-term.el" file (due to the fact that they are located directly in the package).
(use-package tramp-term
  :ensure t
  :config
  ;; other PS1 example:
  ;; export PS1=\"[\\u:\\$PWD]\n $ \"
  (add-hook 'tramp-term-after-initialized-hook
	    (lambda (host)
	      (term-send-raw-string (format "export PS1=\"[\\[\\033[1;35m\\]\\h\\[\\033[0m\\]:\\[\\033[1;32m\\]\\$PWD\\[\\033[0m\\]]\n$ \"
sed -i '/exec env ENV=.. HISTFILE=~..tramp_history/d' ~/.bash_history
LS_COLORS=$LS_COLORS'di=0;36' ; export LS_COLORS
echo \"\" > ~/.tramp_history
clear
export HISTFILE=~/.bash_history
clear
" host)))))

;;;;;; ssh-config-mode
;; https://github.com/jhgorrell/ssh-config-mode-el
(use-package ssh-config-mode
  :ensure t
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-ssh-config-mode))

;;;;;; ssh.el
;; https://github.com/ieure/ssh-el
;; https://github.com/ieure/ssh-el/pull/3

(defun cfg/-make-comint-file-name-prefix ()
  (require 'tramp)
  (format "/%s:%s%s:"
	  tramp-default-method
	  (if ssh-remote-user (format "%s@" ssh-remote-user) "")
	  ssh-host))

(use-package ssh
  :ensure t
  :config
  (add-hook 'ssh-mode-hook
	    (lambda()
	      ;; (ssh-directory-tracking-mode) -- causes an error on connection
	      (shell-dirtrack-mode t)
	      (setq ssh-directory-tracking-mode t)
	      (setq shell-dirtrackp t)
	      (setq comint-file-name-prefix (cfg/-make-comint-file-name-prefix)))))


(provide 'cfg-tramp-ssh)
;;; cfg-tramp-ssh.el ends here
