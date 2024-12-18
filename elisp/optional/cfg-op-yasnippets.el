;;; cfg-op-yasnippets.el --- configfuration for yasnippets -*- lexical-binding: t -*-
;;; Commentary:
;;
;; yasnippet:
;; https://github.com/joaotavora/yasnippet
;; https://www.emacswiki.org/emacs/Yasnippet
;; Documentation: https://joaotavora.github.io/yasnippet/
;;
;;; Code:

(use-package yasnippet
  :ensure t
  :bind (([remap company-indent-or-complete-common]             . cfg/yas-expand-or-company-complete))
  :config
  (when (file-directory-p (expand-file-name "data/yasnippets" user-emacs-directory))
    (setq yas-snippet-dirs (append yas-snippet-dirs (list (expand-file-name "data/yasnippets" user-emacs-directory)))))
  ;; add yas-minor-mode per MAJOR mode - not global:
  (add-hook 'php-mode-hook #'yas-minor-mode) ;; PHP
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode) ;; elisp
  (add-hook 'text-mode-hook #'yas-minor-mode) ;; text files
  (add-hook 'sh-mode-hook #'yas-minor-mode) ;; shell scripts, bash
  (add-hook 'web-mode-hook #'yas-minor-mode) ;; front end
  (add-hook 'c-mode-hook #'yas-minor-mode) ;; c files only
  (add-hook 'cperl-mode-hook #'yas-minor-mode) ;; Perl
  (add-hook 'python-mode-hook #'yas-minor-mode) ;; Python
  (add-hook 'nix-mode-hook #'yas-minor-mode) ;; Nix
  (add-hook 'json-mode-hook #'yas-minor-mode) ;; json
  (add-hook 'snippet-mode-hook #'yas-minor-mode) ;; snippets for yasnippets :-)
  (add-hook 'org-mode-hook #'yas-minor-mode) ;; org-mode
  (add-hook 'notmuch-message-mode-hook #'yas-minor-mode) ;; notmuch-message-mode
  (yas-reload-all)
  ;; alternative - add yasnippet to all backends:
  ;; (yas-global-mode 1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MANIPULATION'S WITH COMPANY-BACKENDS + YASNIPPETS.
  ;; IF YASNIPPET IS AVAILABLE, THEN FORCE YASNIPPETS ENABLED EVERYWHERE WITH COMPANY-MODE:
  ;; SOURCES:
  ;; https://github.com/company-mode/company-mode/issues/839
  ;; https://www.reddit.com/r/emacs/comments/bm8r3c/help_how_do_i_get_yasnippet_names_to_show_up_in/

  (when (require 'company nil 'noerror)
    (progn
      (defun cfg/company-backend-with-yas (backends)
	"Add :with company-yasnippet to ALL company BACKENDS - not only to one.
  Taken from https://github.com/syl20bnr/spacemacs/pull/179."
	(if (and (listp backends) (memq 'company-yasnippet backends))
	    backends
	  (append (if (consp backends)
		      backends
		    (list backends))
		  '(:with company-yasnippet))))
      ;; Shift-<TAB> - SHOULD BE USED WITH BOTH (SMART TAB):
      (defun cfg/yas-expand-or-company-complete (&optional arg)
	(interactive)
	(or
	 ;; (yas-expand)
	 (company-yasnippet)
	 (company-indent-or-complete-common arg)))

      (with-eval-after-load 'company
	(setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))
	(setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))))
  )

;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  :config
  ;;; NOT required anymore:
  ;;; Adding a custom/dynamic yasnippet directory:
  ;;; https://stackoverflow.com/questions/46696009/adding-a-custom-yasnippet-directory-to-spacemacs
  ;;; and dymamic dir - example of code:
  ;;
  ;; (setq yasnippets-dynamic-data-dir
  ;; 	(substring
  ;; 	 ;; (shell-command-to-string "find ~/.emacs.d/elpa/ -type d -iname snippets")
  ;; 	 (shell-command-to-string "ls -d ~/.emacs.d/elpa/yasnippet-snippets-*/snippets")
  ;; 	 0 -1))
  ;; (setq yas-snippet-dirs (append yas-snippet-dirs (list yasnippets-dynamic-data-dir)))
  )

(provide 'cfg-op-yasnippets)
;;; cfg-op-yasnippets.el ends here
