;;; cfg-op-python-anaconda.el --- configfuration for anaconda-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Anaconda-mode is a light alternative to lsp servers
;; ggtags solution seems to be better and faster for "jump to definitions", but anaconda-mode is great for code completion.
;;
;;; Code:

;;;; https://github.com/pythonic-emacs/anaconda-mode#pythonpath
;; (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/project")

(use-package anaconda-mode
  :ensure t
  :config

  ;; it will be DISABLED by default:
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode)

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-python-anaconda-mode)

  ;; turn off anaconda-mode-map, because it seems it could overwrite ggtags keymap (and general.el):
  (setcdr anaconda-mode-map nil))


(defun cfg/enable-anaconda-mode ()
  "Enable anaconda-mode for all python buffers in the future (for corrent emacs session only)."
  (interactive)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (if (bound-and-true-p anaconda-mode)
      (message "anaconda-mode is on")
    (anaconda-mode)))

(provide 'cfg-op-python-anaconda)
;;; cfg-op-python-anaconda.el ends here
