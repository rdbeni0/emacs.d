;;; cfg-op-scheme.el --- configuration for scheme -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; https://codeberg.org/geiser/geiser
(use-package geiser
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-scheme))

;; https://codeberg.org/geiser/chibi
(use-package geiser-chibi
  :ensure t
  :after geiser
  :config
  (setq geiser-default-implementation 'chibi))

(provide 'cfg-op-scheme)
;;; cfg-op-scheme.el ends here
