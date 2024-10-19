;;; cfg-op-flycheck.el --- configfuration for flycheck -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with flycheck.
;;
;;; Code:

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  (global-flycheck-eglot-mode 1))

(use-package flycheck-checkbashisms
  :ensure t
  :config
  (flycheck-checkbashisms-setup))

;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
(defun cfg/flycheck-list-errors-below ()
  (interactive)
  (flycheck-list-errors)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer "*Flycheck errors*")))))

(provide 'cfg-op-flycheck)
;;; cfg-op-flycheck.el ends here
