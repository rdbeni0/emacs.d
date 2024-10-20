;;; cfg-op-python.el --- configfuration for python programming -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with python programming (but without lsp).
;;
;;; Code:

(use-package anaconda-mode
  :ensure t
  :config
  ;; ggtags solution seems to be better and faster for "jump to definitions", but anaconda-mode is great for code completion.
  ;; it will be disabled by default:

  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode)

  ;; turn off anaconda-mode-map, because it seems it could overwrite ggtags keymap (and general.el):
  (setcdr anaconda-mode-map nil))

(defun cfg/enable-anaconda-mode ()
  "Enable anaconda-mode for all python buffers in the future (for corrent emacs session only)."
  (interactive)

  (add-hook 'python-mode-hook 'anaconda-mode)

  (if (bound-and-true-p anaconda-mode)
      (message "anaconda-mode is on")
    (anaconda-mode)))

;;;; https://github.com/pythonic-emacs/anaconda-mode#pythonpath
;; (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/project")

;;;; pyimport
;; https://melpa.org/#/pyimport
(use-package pyimport
  :ensure t
  )

;;;; sphinx-doc
;;;; docstring generation

(use-package sphinx-doc
  :ensure t
  :config
  (add-hook 'python-mode-hook
	  (lambda ()
	    (sphinx-doc-mode t))))

(provide 'cfg-op-python)
;;; cfg-op-python.el ends here
