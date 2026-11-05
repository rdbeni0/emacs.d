;;; cfg-gen-op-haskell.el --- general.el for haskell -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package general
  :functions
  (general-define-key))

(general-define-key
  :states '(normal visual emacs)
  :keymaps '(haskell-mode-map haskell-ts-mode-map)
  :major-modes '(haskell-mode haskell-ts-mode)
  :prefix ","
  "="  '(:ignore t :which-key "format")
  "=n" '(haskell-mode-stylish-buffer :which-key "stylish-haskell"))

(provide 'cfg-gen-op-haskell)
;;; cfg-gen-op-haskell.el ends here
