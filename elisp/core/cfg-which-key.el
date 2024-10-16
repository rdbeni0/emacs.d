;;; cfg-which-key.el --- which-key -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Setup for which-key mode
;;
;;; Code:

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-idle-delay 0.0)
  :config
  (which-key-mode))

(provide 'cfg-which-key)
;;; cfg-which-key.el ends here
