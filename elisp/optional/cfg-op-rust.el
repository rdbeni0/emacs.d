;;; cfg-op-rust.el --- Config for rust -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://github.com/rust-lang/rust-mode
;;
;;; Code:

(use-package rust-mode
  :ensure t
  :config

  (dolist (rs-hook '(rust-mode-hook rust-ts-mode-hook))
    (add-hook rs-hook
              (lambda ()
                (with-eval-after-load 'flycheck
                  (setq flycheck-rust-crate-type nil
                        flycheck-rust-cargo-args '("--bins")
                        flycheck-rust-check-tests nil))
                (setq-local flycheck-rust-cargo-manifest-path
                            (locate-dominating-file default-directory "Cargo.toml")))))

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-rust-mode))

(provide 'cfg-op-rust)
;;; cfg-op-rust.el ends here
