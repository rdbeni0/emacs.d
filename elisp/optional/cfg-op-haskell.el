;;; cfg-op-haskell.el --- configfuration for haskell -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; https://github.com/haskell/haskell-mode
;; https://haskell.github.io/haskell-mode/manual/latest/
(use-package haskell-mode
  :ensure t
  :config

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-haskell-mode))

;; https://github.com/jyp/dante
;; Dante provides a frontend to GHCi features: type-checking, execution, completion and cross referencing.
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  ;; flycheck backend deprecated October 2022
  ;; (add-hook 'haskell-mode-hook 'flycheck-mode)

  ;; (add-hook 'haskell-mode-hook 'flymake-mode)
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'haskell-mode-hook 'dante-mode))


(provide 'cfg-op-haskell)
;;; cfg-op-haskell.el ends here
