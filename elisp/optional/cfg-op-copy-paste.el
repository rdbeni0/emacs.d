;;; cfg-op-copy-paste.el --- configfuration for copy/paste  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Manipulations with copy/paste (wsl/wayland/x11).
;;
;; KDE issues:
;; https://discuss.kde.org/t/plasma-6-2-arch-linux-copy-paste-issues/23162/7
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WAYLAND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Currently not required for KDE 6.1++ plasma (optional experiments):

;; https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4
;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;   (setq wl-copy-process (make-process :name "wl-copy"
;; 				      :buffer nil
;; 				      :command '("wl-copy" "-f" "-n")
;; 				      :connection-type 'pipe
;; 				      :noquery t))
;;   (process-send-string wl-copy-process text)
;;   (process-send-eof wl-copy-process))

;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil ; should return nil if we're the current paste owner
;;     (shell-command-to-string "wl-paste -n | tr -d \r")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; X11/XORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional experiments with copy/cut/paste (not required for wayland, but can be useful for X11):
;;
;;(use-package simpleclip
;;  :ensure t
;;  :bind (;; Remaps - use emacs native again:
;;         ([remap simpleclip-paste]                  . yank)
;;  	 )
;;  :config
;;  ;; (simpleclip-mode 1)
;;  )

(provide 'cfg-op-copy-paste)
;;; cfg-op-copy-paste.el ends here
