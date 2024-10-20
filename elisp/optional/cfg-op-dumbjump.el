;;; cfg-op-dumbjump.el --- configfuration for dumbjump package-*- lexical-binding: t -*-
;;; Commentary:
;;
;; dumb-jump - Dumb Jump is an Emacs "jump to definition" package with support for 50+ programming languages that favors "just working".
;; https://github.com/jacktasia/dumb-jump
;;
;;; Code:

(use-package dumb-jump
  :ensure t
  :hook ((prog-mode . cfg/dumb-jump-activate))
  :init (defun cfg/dumb-jump-activate ()
          (interactive)
	  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
	  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :config
  ;; https://github.com/jacktasia/dumb-jump#configuration
  )

(provide 'cfg-op-dumbjump)
;;; cfg-op-dumbjump.el ends here
