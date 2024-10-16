;;; cfg-vcs.el --- configfuration for vcs/git/magit  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with Version Control Systems (core version).
;;
;;; Code:

(setq version-control t)		; use version control
;; TODO : update path with <emacs-user-directory>
(setq vc-make-backup-files nil) 	; make backups file even when in version controlled dir
(setq vc-follow-symlinks t )		; don't ask for confirmation when opening symlinked file

(provide 'cfg-vcs)
;;; cfg-vcs.el ends here
