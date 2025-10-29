;;; cfg-op-perspective.el --- configfuration for perspectives -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://github.com/nex3/perspective-el
;; WARNING! perspective-el is incompatible with desktop+ or desktop save session
;; Perspective does not work with Emacs desktop.el. 
;; This is because Perspective state stores buffer and window information in frame parameters, and desktop-save-mode does not support saving those types of data.
;;
;;; Code:

(use-package perspective
  :ensure t
  :config
  ;; MUST be here to avoid ugly warning:
  ;; more: https://stackoverflow.com/questions/58615798/how-to-use-leader-key-as-part-of-package-prefix
  (setq persp-mode-prefix-key (kbd "C-c p"))
  (setq persp-state-default-file (expand-file-name "desktops/.perspectives" user-emacs-directory))

  (persp-mode t) ;; TURN ON/OFF - comment/uncomment
  ;; (add-hook 'kill-emacs-hook #'persp-state-save)
  ;; https://github.com/nex3/perspective-el#sample-use-cases
  ;; (consult-customize consult--source-buffer :hidden t :default nil)
  ;; (add-to-list 'consult-buffer-sources persp-consult-source)
  ;; load keybindings from general.el framework:
  (require 'cfg-op-gen-perspective))

(provide 'cfg-op-perspective)
;;; cfg-op-perspective.el ends here
