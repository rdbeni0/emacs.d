;;; cfg-mode-line-hl.el --- mode-line in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for various mode-line options.
;; "Each Emacs window (aside from minibuffer windows) typically has a mode line at the bottom, which displays status information about the buffer displayed in the window."
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html
;; https://www.emacswiki.org/emacs/ModeLineConfiguration

;;; Code:

(setq column-number-mode t)

;; https://github.com/seagle0128/doom-modeline

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 23))

(set-face-background #'mode-line-inactive "black")
(set-face-foreground #'mode-line-inactive "white")
;; (set-face-background #'mode-line "purple4")
(set-face-background #'mode-line "DeepPink4")
(set-face-foreground #'mode-line "yellow")

(global-hl-line-mode 1) ; highlight current line

(provide 'cfg-mode-line-hl)
;;; cfg-mode-line-hl.el ends here
