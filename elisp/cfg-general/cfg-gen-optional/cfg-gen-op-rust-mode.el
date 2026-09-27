;;; cfg-gen-op-rust-mode.el --- general.el for rust-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package general
  :functions
  (general-define-key))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(rust-mode-map rust-ts-mode-map)
 :major-modes '(rust-mode rust-ts-mode)
 :prefix ","
 )

(provide 'cfg-gen-op-rust-mode)
;;; cfg-gen-op-rust-mode.el ends here
