;;; cfg-op-doom-mode-line.el --- doom mode-line in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for doome mode-line.
;;
;;; Code:

(setq column-number-mode t)

;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 23))


(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(provide 'cfg-op-doom-mode-line)
;;; cfg-op-doom-mode-line.el ends here
