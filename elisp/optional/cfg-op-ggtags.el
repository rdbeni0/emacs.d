;;; cfg-op-ggtags.el --- configfuration for tagging systems -*- lexical-binding: t -*-
;;; Commentary:
;;
;; ggtags - everything what is connected with tagging systems:
;; https://github.com/leoliu/ggtags
;; https://www.gnu.org/software/global/
;; https://github.com/universal-ctags/ctags
;;
;; Before installation, ensure that you installed gtags and ctags.
;;
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
  ;; optional and not required:
  (setq ggtags-executable-directory "~/.emacs.d/tools/bin")
  (add-hook 'prog-mode-hook
            (lambda ()
	      (ggtags-mode))))


;; customize PATH and exec-path:
;; https://www.emacswiki.org/emacs/ExecPath
;; https://emacs.stackexchange.com/questions/550/exec-path-and-path

(setenv "PATH" (concat (concat (getenv "HOME") "/.emacs.d/tools/bin") path-separator (getenv "PATH")))
(add-to-list 'exec-path (concat (getenv "HOME") "/.emacs.d/tools/bin"))

;; now we can configure GTAGS* env variables:

(setenv "GTAGSCONF" (concat (getenv "HOME") "/.emacs.d/tools/gtags.conf"))
(setenv "GTAGSLABEL" "new-ctags") ;; it's universal-ctags, faster option than pygments
;; (setenv "GTAGSLABEL" "pygments") ;; if "universal-ctags" will not be enough, then "pygments" can cover less popular languages
;; (setenv "GTAGSLABEL" "universalctags-pygments-native") ;; that config is slow for big codebase, use it only for small projects

;; load keybindings from general.el framework:
(require 'cfg-gen-op-ggtags)

(provide 'cfg-op-ggtags)
;;; cfg-op-ggtags.el ends here
