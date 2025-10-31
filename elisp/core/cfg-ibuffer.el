;;; cfg-ibuffer.el --- configuration for ibuffer -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for ibuffer.
;; https://olddeuteronomy.github.io/post/emacs-ibuffer-config/
;;
;;; Code:

;; ibuffer

(use-package ibuffer
  :config
  ;; turn off prompts "yes" or "no":
  (setq ibuffer-expert t)

  (defun cfg/toggle-ibuffer ()
    "If the current buffer is *Ibuffer*, use `cfg/alternate-buffer`. Otherwise, open ibuffer."
    (interactive)
    (if (eq major-mode 'ibuffer-mode)
	(cfg/alternate-buffer)
      (ibuffer)
      (keyboard-quit)))

  ;; load general.el and keybindings:
  (require 'cfg-gen-co-ibuffer-mode))

(provide 'cfg-ibuffer)
;;; cfg-ibuffer.el ends here
