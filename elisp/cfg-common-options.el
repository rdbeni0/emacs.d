;;; cfg-common-options.el --- configure common options -*- lexical-binding: t -*-
;;; Commentary:

;; Basic setup for backups, startup, lockfiles, utf-8 encoding

;;; Code:

;; TODO : update path with <emacs-user-directory>

(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t)		; use version control
(setq vc-make-backup-files nil) 	; make backups file even when in version controlled dir
(setq make-backup-files nil)	        ; turn off backups

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html

;; issues with .#recentf:
;; https://github.com/syl20bnr/spacemacs/issues/5554
;; https://github.com/syl20bnr/spacemacs/issues/5186
;; https://github.com/syl20bnr/spacemacs/issues/10347
;; So probably it's better to turn it off:

(setq create-lockfiles nil)

;; https://stackoverflow.com/questions/25245134/stop-emacs-creating-autosave-files-in-the-same-directory

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )		; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ; transform backups file name
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(electric-indent-mode -1)               ; https://www.reddit.com/r/emacs/comments/2mu7yi/disable_electric_indent_mode/

;; turn off scratch buffer and startup screen:
;; (setq initial-scratch-message "AgaMacs") ; print a default message in the empty scratch buffer opened at startup
;; (kill-buffer "*scratch*")
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq inhibit-startup-message t)
(set-cursor-color "#ffffff")

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
(global-hl-line-mode 1) ; highlight current line
(defalias 'yes-or-no-p 'y-or-n-p)


(provide 'cfg-common-options)
;;; cfg-common-options.el ends here
