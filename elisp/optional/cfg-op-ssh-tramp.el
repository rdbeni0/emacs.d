;;; cfg-op-ssh-tramp.el --- configfuration for ssh via tramp and sudo -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Additional configuration for TRAMP and ssh connections.
;;
;;; Code:

;;;;;; tramp-term
;; Note: a large part of the data is also directly embedded in the "tramp-term.el" file (due to the fact that they are located directly in the package).
;; https://github.com/cuspymd/tramp-term.el
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

(provide 'cfg-op-ssh-tramp)
;;; cfg-op-ssh-tramp.el ends here
