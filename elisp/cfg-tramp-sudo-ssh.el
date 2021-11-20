;;; cfg-tramp-sudo-ssh.el --- configfuration for tramp, sudo and ssh -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for: TRAMP, sudo and ssh connections.

;;; Code:

;; sudo

(defun cfg/sudo-edit (&optional arg)
  "Edit buffer / file as sudo user"
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name parsed-method
                                                     parsed-user
                                                     parsed-host
                                                     nil
                                                     parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name "sudo"
                                                       "root"
                                                       parsed-host
                                                       parsed-localname
                                                       new-hop)))
           new-fname)))))
  )

(defun cfg/sudired ()
  "Open current directory via sudo and dired."
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir))))
  )

;; tramp
;; more examples for tramp-parse : https://github.com/abo-abo/swiper/issues/59

(setq my-tramp-ssh-completions
      '((tramp-parse-sconfig "~/.ssh/ssh_config")
        (tramp-parse-shosts "~/.ssh/known_hosts"))
      )
(mapc (lambda (method)
        (tramp-set-completion-function method my-tramp-ssh-completions))
      '("fcp" "rsync" "scp" "scpc" "scpx" "sftp" "ssh"))

(defun cfg-make-comint-file-name-prefix ()
  (require 'tramp)
  (format "/%s:%s%s:"
	  tramp-default-method
	  (if ssh-remote-user (format "%s@" ssh-remote-user) "")
	  ssh-host)
  )

;; ssh.el
;; https://github.com/ieure/ssh-el
;; https://github.com/ieure/ssh-el/pull/3

(use-package ssh
  :ensure t
  :config
  (add-hook 'ssh-mode-hook
	    (lambda()
	      ;; (ssh-directory-tracking-mode) -- causes an error on connection
	      (shell-dirtrack-mode t)
	      (setq ssh-directory-tracking-mode t)
	      (setq shell-dirtrackp t)
	      (setq comint-file-name-prefix (cfg-make-comint-file-name-prefix)))
	    )
  )


;; tramp-term
;; Note: a large part of the data is also directly embedded in the "tramp-term.el" file (due to the fact that they are located directly in the package).
;;
;; other PS1 example:
;; export PS1=\"[\\u:\\$PWD]\n $ \"
;;

(require 'tramp-term)
(add-hook 'tramp-term-after-initialized-hook
	  (lambda (host)
	    (term-send-raw-string (format "export PS1=\"[\\[\\033[1;35m\\]\\h\\[\\033[0m\\]:\\[\\033[1;32m\\]\\$PWD\\[\\033[0m\\]]\n$ \"
export HISTFILE=~/.bash_history
clear
" host))
	    )
	  )

(provide 'cfg-tramp-sudo-ssh)
;;; cfg-tramp-sudo-ssh.el ends here
