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

;; (set-face-background #'mode-line-inactive "white")
(set-face-background #'mode-line-inactive "burlywood")
(set-face-foreground #'mode-line-inactive "black")

;; (set-face-background #'mode-line "purple4")
(set-face-background #'mode-line "DeepPink4")
(set-face-foreground #'mode-line "yellow")

(provide 'cfg-op-doom-mode-line)
;;; cfg-op-doom-mode-line.el ends here
