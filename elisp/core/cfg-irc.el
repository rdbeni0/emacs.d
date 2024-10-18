;;; cfg-irc.el --- configfuration for IRC - via rcirc -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with IRC inside Emacs (via rcirc).
;; EmacsWiki: https://www.emacswiki.org/emacs/rcirc
;;
;;; Code:

(defun cfg/rcirc-rename-buffer-as-chan ()
  (let ((buffer (current-buffer)))
    (when (and (rcirc-buffer-process)
               (eq (process-status (rcirc-buffer-process)) 'open))
      (if (rcirc-channel-p rcirc-target)
	  (rename-buffer rcirc-target)))))

;; Dynamically set fill-column at redisplay time:
;; Source: https://www.emacswiki.org/emacs/rcircAutoFillColumn

(defvar rcirc-dynamic-fill-column-margin 3
  "Safety margin used to calculate fill-column depending on window-width")

(defun cfg/rcirc-dynamic-fill-column-window (window &optional margin)
  "Dynamically get window's width and adjust fill-column accordingly"
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'rcirc-mode)
      (setq fill-column
	    (- (window-width window)
	       (or margin rcirc-dynamic-fill-column-margin))))))

(defun cfg/rcirc-dynamic-fill-column (frame)
  "Dynamically tune fill-column for a frame's windows at redisplay time"
  (walk-windows 'cfg/rcirc-dynamic-fill-column-window 'no-minibuf frame))

(use-package rcirc
  :defer t
  :config

  ;; please create correct "rcirc.el" file inside ~/.emacs.d/data/local/rcirc.el (or other emacs dir)
  ;; Please add below variables inside this file:
  ;; (setq rcirc-server-alist ...
  ;; (setq rcirc-authinfo ...

  (if (file-readable-p (expand-file-name "data/local/rcirc.el" user-emacs-directory))
      (load (expand-file-name "data/local/rcirc.el" user-emacs-directory)) ; if true, load additional variables for rcirc
					; if false, then message with "WARNING" will appear during initialization of rcirc:
    (message "WARNING! File data/local/rcirc.el inside your emacs.d is not readable (or not exist)! Please create it and add correct rcirc options!"))

  (add-to-list 'window-size-change-functions 'cfg/rcirc-dynamic-fill-column)
  (setq rcirc-prompt "»» ")
  (setq rcirc-time-format "%H:%M ")
  (setq rcirc-fill-flag nil)
  (add-hook 'rcirc-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'rcirc-mode-hook 'cfg/rcirc-rename-buffer-as-chan)
  (add-hook 'rcirc-mode-hook 'rcirc-omit-mode)
  (evil-set-initial-state 'rcirc-mode 'insert)
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "MODE"))) ;; you can add "AWAY"

(provide 'cfg-irc)
;;; cfg-irc.el ends here
