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
