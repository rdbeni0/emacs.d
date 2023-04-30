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
	 ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (lsp))))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  )

(use-package dap-mode
  :ensure t
  )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;;;;;;;;;;;;;;;;;;;;;;;; PYTHON:

;; python : https://github.com/emacs-lsp/lsp-pyright
;; https://microsoft.github.io/pyright/#/installation
;; required: "pip install pyright"
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

;;;;;;;;;;;;;;;;;;;;;;;;; C/C++:
;; default is: https://emacs-lsp.github.io/lsp-mode/page/lsp-clangd/
;; should be installed as system package

;; https://github.com/MaskRay/ccls/wiki/lsp-mode
;; optional for C/C++:

;; (use-package ccls
;;   :ensure t
;;   :configure
;;   (setq ccls-executable "/usr/bin/ccls"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; https://github.com/emacs-lsp/lsp-mode/issues/1530
;; (evil-define-key 'normal lsp-mode-map (kbd "\\") lsp-command-map)
(general-def 'normal lsp-mode :definer 'minor-mode "\\" lsp-command-map)


(provide 'cfg-lsp-dap)
;;; cfg-lsp-dap.el ends here
