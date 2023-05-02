;;; cfg-lsp-dap.el --- dap and lsp-mode -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with lsp-mode and dap-mode for emacs.
;; https://emacs-lsp.github.io/lsp-mode/
;; https://github.com/emacs-lsp/dap-mode

;;; Code:

;; https://emacs-lsp.github.io/lsp-mode/page/installation/
(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (sh-mode . lsp-deferred)
	 ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp))))
  :commands (lsp lsp-deferred))

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-headerline-breadcrumb-enable nil)

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
