;;; cfg-ggtags.el --- configfuration for  tagging systems -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with tagging systems:
;; https://github.com/leoliu/ggtags
;; https://www.gnu.org/software/global/
;; https://github.com/universal-ctags/ctags
;;
;; Before installation, ensure that you installed gtags and ctags.
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
  (setq ggtags-executable-directory "~/.emacs.d/ggtags/bin"))

(use-package helm-gtags
  :ensure t
  :after '(helm ggtags))

;; now we can manipulate with GTAGS env variables:

(setenv "PATH"
        (concat
         (concat (getenv "HOME") "/.emacs.d/ggtags/bin") path-separator
         (getenv "PATH")))

(setenv "GTAGSCONF" (concat (getenv "HOME") "/.emacs.d/ggtags/gtags.conf"))
(setenv "GTAGSLABEL" "new-ctags") ;; it's universal-ctags, faster option than pygments
;; (setenv "GTAGSLABEL" "pygments") ;; if "universal-ctags" will not be enough

(provide 'cfg-ggtags)
;;; cfg-ggtags.el ends here
