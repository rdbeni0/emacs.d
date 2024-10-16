;;; cfg-hideshow.el --- hideshow -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Setup for hideshow minor mode:
;; https://www.emacswiki.org/emacs/HideShow
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html
;; Please note that this package is useful and required for evil-mode (vim emulation).
;;
;;; Code:

;; hideshow can be enabled/disabled for particular mode - for example:
;; (add-hook 'perl-mode-hook 'hs-minor-mode)
(use-package hideshow
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(provide 'cfg-hideshow)
;;; cfg-hideshow.el ends here
