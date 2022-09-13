;;; cfg-ggtags-dumbjump.el --- configfuration for  tagging systems -*- lexical-binding: t -*-
;;; Commentary:
;;
;; dumb-jump - Dumb Jump is an Emacs "jump to definition" package with support for 50+ programming languages that favors "just working".
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

;; dumb-jump: https://github.com/jacktasia/dumb-jump

(use-package dumb-jump
  :ensure t
  :after helm
  :hook ((prog-mode . cfg/dumb-jump-activate))
  :init (defun cfg/dumb-jump-activate ()
          (interactive)
	  (if (version< emacs-version "28.1") (message "Your emacs version is too old! dumb-jump will work only via \"dumb-jump-go\""))
	  ;;
	  ;; ^ if this msg will appear:
	  ;; https://www.reddit.com/r/emacs/comments/pr7nh2/dumbjump/
	  ;; "(If you really want to use the old interface, dumb-jump-use-legacy-interface can be set to nil as to silence the warnings you would get from using obsolete functions.
	  ;; I would really recommend trying xref though.)"
	  ;;
	  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
	  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :config
  ;; https://github.com/jacktasia/dumb-jump#configuration
  )

;; tags:

(use-package ggtags
  :ensure t
  :config
  (setq ggtags-completing-read-function nil)
  ;; optional and not required:
  (setq ggtags-executable-directory "~/.emacs.d/ggtags/bin")
  (add-hook 'prog-mode-hook
            (lambda ()
	      (ggtags-mode))))

(use-package helm-gtags
  :ensure t
  )

;; execute two modes at once:

(add-hook 'ggtags-mode-hook
          (lambda ()
	    (helm-gtags-mode)
	    ))

;; customize PATH and exec-path:
;; https://www.emacswiki.org/emacs/ExecPath
;; https://emacs.stackexchange.com/questions/550/exec-path-and-path

(setenv "PATH" (concat (concat (getenv "HOME") "/.emacs.d/ggtags/bin") path-separator (getenv "PATH")))
(add-to-list 'exec-path (concat (getenv "HOME") "/.emacs.d/ggtags/bin"))

;; now we can configure GTAGS* env variables:

(setenv "GTAGSCONF" (concat (getenv "HOME") "/.emacs.d/ggtags/gtags.conf"))
(setenv "GTAGSLABEL" "new-ctags") ;; it's universal-ctags, faster option than pygments
;; (setenv "GTAGSLABEL" "pygments") ;; if "universal-ctags" will not be enough, then "pygments" can cover less popular languages
;; (setenv "GTAGSLABEL" "universalctags-pygments-native") ;; that config is slow for big codebase, use it only for small projects

(provide 'cfg-ggtags-dumbjump)
;;; cfg-ggtags-dumbjump.el ends here
