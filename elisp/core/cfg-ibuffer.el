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
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-ibuffer-mode))

(provide 'cfg-ibuffer)
;;; cfg-ibuffer.el ends here
