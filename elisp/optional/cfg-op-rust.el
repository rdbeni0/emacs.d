;;; cfg-op-rust.el --- Config for rust -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://github.com/rust-lang/rust-mode
;;
;;; Code:

(use-package rust-mode
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-lua-mode))

(provide 'cfg-op-rust)
;;; cfg-op-rust.el ends here
