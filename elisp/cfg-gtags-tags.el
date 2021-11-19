;;; cfg-gtags-tags.el --- configfuration for  tagging systems -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with tagging systems:
;; https://github.com/leoliu/ggtags
;; https://www.gnu.org/software/global/
;; https://github.com/universal-ctags/ctags
;;
;; before installation, ensure that you installed gtags and ctags, for example - Arch packages:
;;
;; https://archlinux.org/packages/community/x86_64/global/
;; https://archlinux.org/packages/extra/x86_64/ctags/
;;
;; pygments:
;; pygments is for less popular languages:
;; In order to look up symbol references for any language not in the built in parser you must use the pygments backend:
;; https://pygments.org/
;;
;; tutorial for spacemacs:
;; https://develop.spacemacs.org/layers/+tags/gtags/README.html
;;
;;; Code:

(use-package ggtags
  :ensure t
  :config
  (setq ggtags-completing-read-function nil)
  )

(use-package helm-gtags
  :ensure t
  :after '(helm ggtags)
  )


(provide 'cfg-gtags-tags)
;;; cfg-gtags-tags.el ends here
