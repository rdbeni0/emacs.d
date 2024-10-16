;;; cfg-tramp-sudo.el --- configfuration for tramp and sudo -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for TRAMP and sudo.
;;
;;; Code:

;; tramp
;; more examples for tramp-parse : https://github.com/abo-abo/swiper/issues/59

(use-package tramp
  :config
  (setq my-tramp-ssh-completions
	'((tramp-parse-sconfig "~/.ssh/ssh_config")
          (tramp-parse-shosts "~/.ssh/known_hosts")))

  (mapc (lambda (method)
          (tramp-set-completion-function method my-tramp-ssh-completions))
	'("fcp" "rsync" "scp" "scpc" "scpx" "sftp" "ssh")))

;; sudo via tramp

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
           new-fname))))))

(defun cfg/sudired ()
  "Open current directory via sudo and dired."
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir)))))

(provide 'cfg-tramp-sudo)
;;; cfg-tramp-sudo.el ends here
