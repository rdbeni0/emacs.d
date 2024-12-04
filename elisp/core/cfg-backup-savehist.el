;;; cfg-backup-savehist.el --- configure options for backups/savehist -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Basic setup for backups, savehist, and lockfiles.
;;
;;; Code:

;; https://stackoverflow.com/questions/25245134/stop-emacs-creating-autosave-files-in-the-same-directory
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html
;; and issues with .#recentf:
;; https://github.com/syl20bnr/spacemacs/issues/5554
;; https://github.com/syl20bnr/spacemacs/issues/5186
;; https://github.com/syl20bnr/spacemacs/issues/10347
;; So probably it's better to turn lockfiles off:
(setq create-lockfiles nil)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ; transform backups file name

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SAVEHIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save history of minibuffer

(savehist-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BACKUPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OPTIONAL - turn off backups:
;; (setq make-backup-files nil)

;; delete excess backup versions silently:
(setq delete-old-versions -1 )
;; which directory to put backups file:
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )

;; Don't litter file system with *~ backup files; put them all inside "~/.emacs.d/backups"
(defun cfg/-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "backups/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'cfg/-backup-file-name)

(provide 'cfg-backup-savehist)
;;; cfg-backup-savehist.el ends here
