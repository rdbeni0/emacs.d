;;; cfg-op-python.el --- configfuration for python programming -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with python programming (but without lsp and code completion).
;; Additional and useful packages.
;;
;;; Code:

;;;; https://github.com/pythonic-emacs/anaconda-mode#pythonpath
;; probably something like this should be used in dir-locals.el:
;; (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/project")

;; load general.el and keybindings:
(require 'cfg-gen-op-python-mode)

;;;; pyimport
;; https://melpa.org/#/pyimport
(use-package pyimport
  :ensure t
  )

;;;; sphinx-doc - docstring generation
(use-package sphinx-doc
  :ensure t
  :config
  (add-hook 'python-mode-hook
	    (lambda ()
	      (sphinx-doc-mode t))))

(provide 'cfg-op-python)
;;; cfg-op-python.el ends here
