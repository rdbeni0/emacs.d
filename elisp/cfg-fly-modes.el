;;; cfg-fly-modes.el --- configfuration for flycheck, flyspell, flymake -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with "fly-modes": flycheck, flyspell and flymake

;;; Code:

;; Dependencies for flyspell: *aspell* dictionaries
;; For example - Arch Linux packages: aspell-en aspell-pl

(use-package flyspell
  :init
  (setq flyspell-default-dictionary "english"))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  )

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


(provide 'cfg-fly-modes)
;;; cfg-fly-modes.el ends here
