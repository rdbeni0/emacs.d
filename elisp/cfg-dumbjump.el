;;; cfg-dumbjump.el --- configfuration for dumbjump package-*- lexical-binding: t -*-
;;; Commentary:
;;
;; dumb-jump - Dumb Jump is an Emacs "jump to definition" package with support for 50+ programming languages that favors "just working".
;; https://github.com/jacktasia/dumb-jump
;;
;;; Code:

(use-package dumb-jump
  :ensure t
  :hook ((prog-mode . cfg/dumb-jump-activate))
  :init (defun cfg/dumb-jump-activate ()
          (interactive)
	  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
	  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :config
  ;; Note that the function xref-show-definitions-completing-read requires at least Xref 1.1.0.
  ;; This can either be downloaded from ELPA or is bundled with Emacs 28.1 or newer.
  (if (version< emacs-version "28.1")
      ;; (message "Your emacs version is too old! dumb-jump-use-legacy-interface can be used!")
      (load-file (expand-file-name "site-elisp/xref_dumb_jump.el" user-emacs-directory)))

  ;;
  ;; ^ alternative option:
  ;; https://www.reddit.com/r/emacs/comments/pr7nh2/dumbjump/
  ;; "(If you really want to use the old interface, dumb-jump-use-legacy-interface can be set to nil as to silence the warnings you would get from using obsolete functions.
  ;; I would really recommend trying xref though.)"
  ;;

  ;; https://github.com/jacktasia/dumb-jump#configuration
  )

(provide 'cfg-dumbjump)
;;; cfg-dumbjump.el ends here
