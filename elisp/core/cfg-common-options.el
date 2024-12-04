;;; cfg-common-options.el --- configure common options -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Basic setup for backups, startup, lockfiles, utf-8 encoding
;;
;;; Code:


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html

;; issues with .#recentf:
;; https://github.com/syl20bnr/spacemacs/issues/5554
;; https://github.com/syl20bnr/spacemacs/issues/5186
;; https://github.com/syl20bnr/spacemacs/issues/10347
;; So probably it's better to turn it off:

;; don't ask about .dir-locals.el variables:
(setq-default enable-local-variables :all)

(setq create-lockfiles nil)

;; https://stackoverflow.com/questions/25245134/stop-emacs-creating-autosave-files-in-the-same-directory

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ; transform backups file name
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake

;; Fix archaic defaults
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(electric-indent-mode -1)               ; https://www.reddit.com/r/emacs/comments/2mu7yi/disable_electric_indent_mode/

;; turn off scratch buffer and startup screen:
;; (setq initial-scratch-message "AgaMacs") ; print a default message in the empty scratch buffer opened at startup
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq inhibit-startup-message t)
;; (setq initial-major-mode 'emacs-lisp-mode)
(set-cursor-color "#ffffff")

;;;; optional - use "M-x eshell" instead of *scratch*:
;; (add-hook 'emacs-startup-hook (eshell))
;; (kill-buffer "*scratch*")


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Resources.html
;; https://emacs.stackexchange.com/questions/13291/emacs-cursor-color-is-different-in-daemon-and-non-daemon-modes
(setq inhibit-x-resources t)

;; Minimal UI

(scroll-bar-mode)
;; (scroll-bar-mode -1)
;; (horizontal-scroll-bar-mode)
(tool-bar-mode   -1)
;; (tooltip-mode    -1)
;; (menu-bar-mode   -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 '(auth-source-save-behavior nil)
 '(warning-suppress-types '((frameset))))

;;
;; https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/init.el
;;

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; turn off backups - OPTIONAL:
;; (setq make-backup-files nil)

(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun cfg/-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "backups/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'cfg/-backup-file-name)

(provide 'cfg-common-options)
;;; cfg-common-options.el ends here
