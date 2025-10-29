;;; cfg-op-quelpa.el --- Config for quelpa -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Setup for quelpa:
;; https://github.com/quelpa/quelpa
;;
;;; Code:

(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice))

(provide 'cfg-op-quelpa)
;;; cfg-op-quelpa.el ends here
