;;; cfg-folding-hideshow.el --- hideshow and code folding -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for hideshow minor mode and various code folding.
;; https://www.emacswiki.org/emacs/HideShow

;;; Code:

;; hideshow can be enabled/disabled for particular mode - for example:
;;
;; (add-hook 'perl-mode-hook 'hs-minor-mode)
;; ^ but - how to enable everywhere:
;; "In Emacs 24 or later, you can turn it on in all programming modes using prog-mode-hook."
;; https://stackoverflow.com/questions/12763566/how-to-permanently-enable-the-hs-minor-mode-in-emacs

(use-package hideshow
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  )

(provide 'cfg-folding-hideshow)
;;; cfg-folding-hideshow.el ends here
