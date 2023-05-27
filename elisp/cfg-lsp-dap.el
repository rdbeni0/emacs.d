;;; cfg-lsp-dap.el --- dap and lsp-mode -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with lsp-mode and dap-mode for emacs.
;; https://emacs-lsp.github.io/lsp-mode/
;; https://github.com/emacs-lsp/dap-mode

;;; Code:

;; too buggy... but  if you really want use PLIST, then: try setting (setenv "LSP_USE_PLISTS" "1") in your init.el before lsp-mode is loaded.
;; (setenv "LSP_USE_PLISTS" "1") ;; "1" or "true"

;; https://emacs-lsp.github.io/lsp-mode/page/installation/
(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (sh-mode . lsp-deferred)
	 ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp))))
  :commands (lsp lsp-deferred)
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; main features: https://emacs-lsp.github.io/lsp-mode/page/main-features/
  ;; nice example of config: https://github.com/emacs-lsp/lsp-mode/issues/3497

  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-headerline-breadcrumb-enable nil)

  ;;lsp performance:
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#check-if-logging-is-switched-off
  ;; if set to true can cause a performance hit
  (setq lsp-log-io nil)

  ;; Optional: fine-tune lsp-idle-delay.
  ;; This variable determines how often lsp-mode will refresh the highlights, lenses, links, etc while you type.
  (setq lsp-idle-delay 0.6)

  ;; too buggy...
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
  ;; (setq lsp-use-plists t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  )

(use-package dap-mode
  :ensure t
  )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; PYTHON:
;; python : https://github.com/emacs-lsp/lsp-pyright
;; https://microsoft.github.io/pyright/#/installation
;; required: "pip install pyright"

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; C/C++:
;; clangd and ccls should be installed as system packages
;; default lsp client is clangd: https://emacs-lsp.github.io/lsp-mode/page/lsp-clangd/
;; https://github.com/clangd/clangd
;; but it seems that ccls is little more updated.
;; https://github.com/MaskRay/ccls
;; https://github.com/MaskRay/ccls/wiki/lsp-mode

(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "/usr/bin/ccls"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load keybindings from general.el framework:
(require 'cfg-gen-lsp-dap-mode)

(provide 'cfg-lsp-dap)
;;; cfg-lsp-dap.el ends here
