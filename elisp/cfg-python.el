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

;; https://github.com/pythonic-emacs/company-anaconda
(use-package company-anaconda
  :ensure t
)

;; OPTIONAL:
;; https://github.com/emacsorphanage/company-jedi
;; (use-package company-jedi
;;   :ensure t
;; )

;;;; python-mode and pylint

(add-hook 'python-mode-hook
          (lambda ()
            (setq flycheck-python-pylint-executable "~/.local/bin/pylint")
            (setq flycheck-pylintrc "~/.pylintrc")))

(provide 'cfg-python)
;;; cfg-python.el ends here
