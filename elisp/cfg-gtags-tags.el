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
;;
;; https://pygments.org/
;;
;; tutorial for spacemacs:
;; https://develop.spacemacs.org/layers/+tags/gtags/README.html
;;
;;; Code:

(use-package ggtags
  :ensure t
  )


(provide 'cfg-gtags-tags)
;;; cfg-gtags-tags.el ends here
