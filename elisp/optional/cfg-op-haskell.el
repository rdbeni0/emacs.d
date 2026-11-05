;;; cfg-op-haskell.el --- configuration for Haskell -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; https://github.com/haskell/haskell-mode
;; https://haskell.github.io/haskell-mode/manual/latest/
(use-package haskell-mode
  :ensure t)

;; https://github.com/jyp/dante
;; Dante provides an interactive GHCi frontend:
;; type inspection, evaluation, completion and cross-referencing.
(use-package dante
  :ensure t
  :after haskell-mode
  :commands dante-mode
  :init

  ;; Prefer HLS/LSP for compiler diagnostics.
  ;; Keep HLint enabled for additional static analysis.
  ;; Disable redundant Flycheck GHC checkers.
  (defun cfg/-haskell-disable-flycheck-checkers ()
    (setq-local flycheck-disabled-checkers
                '(haskell-dante
                  haskell-stack-ghc
                  haskell-ghc)))

  ;; Apply after Flycheck has been loaded.
  (with-eval-after-load 'flycheck
    (add-hook 'haskell-mode-hook
              #'cfg/-haskell-disable-flycheck-checkers))

  ;; Keep Dante for interactive GHCi functionality.
  (add-hook 'haskell-mode-hook #'dante-mode))

;; Load general.el and keybindings:
(require 'cfg-gen-op-haskell)

(provide 'cfg-op-haskell)
;;; cfg-op-haskell.el ends here
