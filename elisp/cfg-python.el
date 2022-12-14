;;; cfg-python.el --- configfuration for python programming -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with python programming. But without LSP.

;;; Code:

(use-package anaconda-mode
  :ensure t
  :config
  ;; ggtags solution seems to be better and faster for "jump to definitions", but anaconda-mode is great for code completion.
  ;; it will be disabled by default:

  ;; (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode)

  ;; turn off anaconda-mode-map, because it seems it could overwrite ggtags keymap (and general.el):
  (setcdr anaconda-mode-map nil)
  )

(defun cfg/enable-anaconda-mode ()
  "Enable anaconda-mode for all python buffers in the future (for corrent emacs session only)."
  (interactive)

  (add-hook 'python-mode-hook 'anaconda-mode)

  (if (bound-and-true-p anaconda-mode)
      (message "anaconda-mode is on")
    (anaconda-mode)))

;; https://github.com/pythonic-emacs/company-anaconda
(use-package company-anaconda
  :ensure t
  )

;; OPTIONAL:
;; https://github.com/emacsorphanage/company-jedi
;; (use-package company-jedi
;;   :ensure t
;; )

;;;; pyimport
;; https://melpa.org/#/pyimport

(use-package pyimport
  :ensure t
  )

;;;; sphinx-doc
;;;; docstring generation

(use-package sphinx-doc
  :ensure t
  )

;;;; python-mode - linters, checkers...
;;;; we need to know that flycheck has excellent support for python-mode in emacs, but it needs some executables to be installed

(setq flycheck-python-pylint-executable "~/.local/bin/pylint")
(setq flycheck-python-mypy-executable "~/.local/bin/mypy")
(setq flycheck-python-mypy-cache-dir¶ (expand-file-name ".cache/mypy" user-emacs-directory)) ;; https://github.com/python/mypy
(setq flycheck-pylintrc (expand-file-name ".pylintrc" user-emacs-directory))
;; https://www.reddit.com/r/emacs/comments/gqymvz/how_to_force_flycheck_to_select_a_specific_syntax/
;; https://www.flycheck.org/en/latest/languages.html#python
(flycheck-add-next-checker 'python-flake8 'python-pylint 'python-mypy)
;; (flycheck-add-next-checker 'python-flake8)

(add-hook 'python-mode-hook
	  (lambda ()
	    (sphinx-doc-mode t)))



(provide 'cfg-python)
;;; cfg-python.el ends here
