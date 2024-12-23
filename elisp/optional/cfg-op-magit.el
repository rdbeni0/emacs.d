;;; cfg-op-magit.el --- configfuration for git/magit  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with Magit: https://magit.vc/
;;
;;; Code:

(use-package magit
  :ensure t
  :config
  ;; TODO: update config
  ;; https://stackoverflow.com/questions/9439702/how-to-open-magit-status-in-full-window
  (setq magit-status-buffer-switch-function 'switch-to-buffer) ;; old magit
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)  ;; new
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-magit))

(provide 'cfg-op-magit)
;;; cfg-op-magit.el ends here
