;;; cfg-vcs-magit.el --- configfuration for vcs/git/magit  -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with Magit and Version Control Systems.
;; https://magit.vc/

;;; Code:

;; native vcs

(setq version-control t)		; use version control
;; TODO : update path with <emacs-user-directory>
(setq vc-make-backup-files nil) 	; make backups file even when in version controlled dir
(setq vc-follow-symlinks t )		; don't ask for confirmation when opening symlinked file

;; magit
(use-package magit
  :ensure t
  :config
  ;; TODO: update config
  ;; https://stackoverflow.com/questions/9439702/how-to-open-magit-status-in-full-window
  (setq magit-status-buffer-switch-function 'switch-to-buffer) ;; old magit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)) ;; new magit

(provide 'cfg-vcs-magit)
;;; cfg-vcs-magit.el ends here
