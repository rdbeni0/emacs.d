;;; cfg-company-yasnippets.el --- configfuration for company-mode and yasnippets -*- lexical-binding: t -*-
;;; Commentary:

;; company-mode
;; https://company-mode.github.io/

;; yasnippet:
;; https://github.com/joaotavora/yasnippet
;; https://www.emacswiki.org/emacs/Yasnippet

;;; Code:

(use-package company
  :ensure t
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; common options:
  (global-company-mode 1)
  (setq company-idle-delay              0   ;; no delay at all
	;; https://emacs.stackexchange.com/questions/4011/i-want-company-mode-to-show-completions-list-after-the-second-character
	company-minimum-prefix-length   1   ;; show completion after 1 character (default is 3!)
	company-show-numbers            t
	company-tooltip-limit           40  ;; The maximum number of candidates in the tooltip
	;; transformers - could be changed "per mode": https://emacs.stackexchange.com/questions/68733/delete-duplicates-from-company-popups
	company-transformers '(company-sort-by-occurrence delete-dups))
  ;; Press SPACE will accept the highlighted candidate and insert a space
  ;; "M-x describe-variable company-auto-complete-chars" for details.
  ;; So that's BAD idea.
  (setq company-auto-complete nil)
  ;;
  ;;
  ;; NOT to load company-mode for certain major modes.
  ;; https://github.com/company-mode/company-mode/issues/29
  ;;
  (setq company-global-modes
        '(not
          eshell-mode
          comint-mode
          erc-mode
          gud-mode
          rcirc-mode
	  vterm-mode
          minibuffer-inactive-mode))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MANIPULATION'S WITH COMPANY-BACKENDS + YASNIPPETS.
  ;; FORCE YASNIPPETS ENABLED EVERYWHERE WITH COMPANY-MODE :
  ;; SOURCES:
  ;; https://github.com/company-mode/company-mode/issues/839
  ;; https://www.reddit.com/r/emacs/comments/bm8r3c/help_how_do_i_get_yasnippet_names_to_show_up_in/
  (defun cfg/company-backend-with-yas (backends)
    "Add :with company-yasnippet to ALL company BACKENDS - not only to one.
  Taken from https://github.com/syl20bnr/spacemacs/pull/179."
    (if (and (listp backends) (memq 'company-yasnippet backends))
	backends
      (append (if (consp backends)
		  backends
		(list backends))
	      '(:with company-yasnippet))))
  ;; <TAB> - SHOULD BE USED WITH BOTH (SMART TAB):
  (defun cfg/yas-expand-or-company-complete (&optional arg)
    (interactive)
    (or (yas-expand) (company-indent-or-complete-common arg)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MORE OPTIONS:
  (with-eval-after-load 'company
    (setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))
    (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; COMPANY BACKENDS:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; COMPANY BACKENDS - CONFIGURATION PER MODE (LOCALLY):
  ;; https://www.reddit.com/r/emacs/comments/ba6blj/company_looking_for_comprehensive_documentation/
  ;; ^ "(set (make-local-variable 'company-backends) '())" should be used (seems to be the most correct way)
  ;; OTHER EXAMPLE:
  ;; https://emacs.stackexchange.com/questions/23999/php-completion-with-company-does-not-work-on-local-variables
  ;;
  ;; Description of particular backends: https://company-mode.github.io/manual/Backends.html
  ;;
  ;; How TO MANIPULATE BACKENDS PER MAJOR MODE:
  ;; 1) clean (setq company-backends '()) - from now it will be empty
  ;; 2) add your preferred backends - choose only the best options (and not add everything! - it will be mess)
  ;; 2A) the less important  backends should be at the beginning
  ;; 2B) the most important - should be declared at the end
  ;; 3) run emacs and check variable "company-backends" as "describe-variable"
  ;; 4) OPTIONAL: check & change: company-transformers '(company-sort-by-occurrence) > sorting options
  ;; 5) OPTIONAL: no new line for particular mode: https://github.com/joaotavora/yasnippet/issues/192
  ;; just add: (setq require-final-newline nil) into particular mode hook

  (add-hook 'php-mode-hook (lambda ()
			     (set (make-local-variable 'company-backends) '())
			     (add-to-list 'company-backends 'company-capf)
			     (add-to-list 'company-backends '(company-dabbrev-code company-files company-gtags company-keywords company-dabbrev))
			     (setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends)))) ;; TODO -  company-ac-php-backend - the most important

  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (set (make-local-variable 'company-backends) '())
				    (add-to-list 'company-backends '(company-dabbrev-code company-files company-gtags company-keywords company-dabbrev))
				    (add-to-list 'company-backends '(company-capf company-files)) ;; capf is working great with elisp code
				    (setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))))

  (add-hook 'sh-mode-hook (lambda ()
			    (set (make-local-variable 'company-backends) '())
			    (add-to-list 'company-backends 'company-capf)
			    (add-to-list 'company-backends '(company-dabbrev-code company-files company-gtags company-keywords company-dabbrev))
			    (setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))))

  (add-hook 'cperl-mode-hook (lambda ()
			       (set (make-local-variable 'company-backends) '())
			       (add-to-list 'company-backends 'company-capf)
			       (add-to-list 'company-backends '(company-dabbrev-code company-files company-gtags company-keywords company-dabbrev))
			       (setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))))

  (add-hook 'python-mode-hook (lambda ()
				(set (make-local-variable 'company-backends) '())
				(add-to-list 'company-backends 'company-capf)
				(add-to-list 'company-backends '(company-anaconda company-dabbrev-code company-files company-keywords company-gtags company-dabbrev)) ;; anaconda-mode
				(setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))))

  (add-hook 'snippet-mode-hook (lambda ()
				 (set (make-local-variable 'company-backends) '())
				 (add-to-list 'company-backends 'company-capf)
				 (add-to-list 'company-backends 'company-gtags)
				 (add-to-list 'company-backends '(company-dabbrev-code company-files company-keywords company-dabbrev))
				 (setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))))

  (add-hook 'org-mode-hook (lambda ()
			     (set (make-local-variable 'company-backends) '())
			     (add-to-list 'company-backends 'company-capf)
			     (add-to-list 'company-backends 'company-gtags)
			     (add-to-list 'company-backends '(company-dabbrev-code company-files company-keywords company-dabbrev))
			     (setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))))

  ;; some optional manipulations: https://github.com/doomemacs/doomemacs/issues/3908
  (add-hook 'notmuch-message-mode-hook (lambda ()
					 (set (make-local-variable 'company-backends) '())
					 (add-to-list 'company-backends 'company-gtags)
					 (add-to-list 'company-backends 'company-capf)
					 (add-to-list 'company-backends '(notmuch-company company-files company-dabbrev company-dabbrev-code))
					 (setq company-backends (mapcar #'cfg/company-backend-with-yas company-backends))
					 (setq require-final-newline nil))) ;; no new lines after inserting snippet

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )

;; https://github.com/pythonic-emacs/company-anaconda
;; for python:

(use-package company-anaconda
  :after company
  :ensure t
  )

;; https://github.com/expez/company-quickhelp

(use-package company-quickhelp
  :after company
  :ensure t
  :config

  ;; https://emacs.stackexchange.com/questions/2762/jump-to-documentation-buffer-with-company-mode
  (defun cfg/company-show-doc-buffer-f1 ()
    "Temporarily show the documentation buffer for the selection."
    (interactive)
    (let* ((selected (nth company-selection company-candidates))
	   (doc-buffer (or (company-call-backend 'doc-buffer selected)
			   (error "No documentation available"))))
      (with-current-buffer doc-buffer
	(goto-char (point-min)))
      (display-buffer doc-buffer t))
    )
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.0)
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  (add-hook 'company-mode-hook #'company-quickhelp-mode)
  (define-key company-active-map (kbd "<f1>") #'cfg/company-show-doc-buffer-f1)) ; TODO - migrate to general.el


;;;;;;;;;;;;;;;;;;;;;; yasnippets
;; Documentation: https://joaotavora.github.io/yasnippet/

(use-package yasnippet
  :ensure t
  :config
  ;; add yasnippet to all backends:
  ;; (yas-global-mode 1)
  )

;; https://github.com/AndreaCrotti/yasnippet-snippets

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding a custom/dynamic yasnippet directory:
;; https://stackoverflow.com/questions/46696009/adding-a-custom-yasnippet-directory-to-spacemacs
;; and dymamic dir - example of code:
(setq yasnippets-dynamic-data-dir
      (substring
       ;;
       ;; EXAMPLES:
       ;; (shell-command-to-string "find ~/.emacs.d/elpa/ -type d -iname snippets")
       ;;
       (shell-command-to-string "ls -d ~/.emacs.d/elpa/yasnippet-snippets-*/snippets")
       0 -1))

;; and then:
;; (setq yas-snippet-dirs (append yas-snippet-dirs (list yasnippets-dynamic-data-dir)))
;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet-snippets
  :ensure t
  :config
  ;; add yas-minor-mode per MAJOR mode - not global:
  (add-hook 'php-mode-hook #'yas-minor-mode) ;; PHP
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode) ;; elisp
  (add-hook 'text-mode-hook #'yas-minor-mode) ;; text files
  (add-hook 'sh-mode-hook #'yas-minor-mode) ;; shell scripts, bash
  (add-hook 'cperl-mode-hook #'yas-minor-mode) ;; Perl
  (add-hook 'python-mode-hook #'yas-minor-mode) ;; Python
  (add-hook 'snippet-mode-hook #'yas-minor-mode) ;; snippets for yasnippets :-)
  (add-hook 'org-mode-hook #'yas-minor-mode) ;; org-mode
  (add-hook 'notmuch-message-mode-hook #'yas-minor-mode) ;; notmuch-message-mode
  (yas-reload-all))

(provide 'cfg-company-yasnippets)
;;; cfg-company-yasnippets.el ends here
