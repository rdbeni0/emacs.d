;;; cfg-flyspell.el --- flyspell -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Setup for flyspell mode.
;;
;;; Code:

;; Dependencies for flyspell: *aspell* dictionaries
;; For example - Arch Linux packages: aspell-en aspell-pl
(use-package flyspell
  :init
  :config
  (setq flyspell-default-dictionary "english"))

(provide 'cfg-flyspell)
;;; cfg-flyspell.el ends here
