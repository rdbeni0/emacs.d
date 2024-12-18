;;; cfg-op-company.el --- configfuration for company-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; USE-PACKAGE:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://company-mode.github.io/
(use-package company
  :ensure t
  :bind (;; Remaps - emacs native:
         ([remap cfg/expand-abbrev]                 . company-abbrev)
         ([remap dabbrev-expand]                    . company-dabbrev)
	 )
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; common options:
  (global-company-mode 1)
  (setq company-idle-delay              0.5 ;; https://github.com/company-mode/company-mode/issues/255 - should be some delay, for example 0.5
	;; if (setq company-idle-delay nil), then autocompletion will be turned off
	;; https://emacs.stackexchange.com/questions/4011/i-want-company-mode-to-show-completions-list-after-the-second-character
	company-minimum-prefix-length   2   ;; show completion after 2 characters (default is 3!)
	company-show-numbers            t
	company-tooltip-limit           40  ;; The maximum number of candidates in the tooltip
	;; transformers - could be changed "per mode":
	;; https://github.com/company-mode/company-mode/issues/818
	;; https://emacs.stackexchange.com/questions/68733/delete-duplicates-from-company-popups
	;; https://company-mode.github.io/manual/Backends.html
	company-transformers '(company-sort-by-statistics company-sort-by-backend-importance delete-dups)) ;; or  / company-sort-by-occurrence
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
  ;; https://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text
  (setq company-dabbrev-downcase nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; COMPANY BACKENDS:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; COMPANY BACKENDS - CONFIGURATION PER MODE (LOCALLY, NOT GLOBALLY):
  ;; https://www.reddit.com/r/emacs/comments/ba6blj/company_looking_for_comprehensive_documentation/
  ;; ^ "(set (make-local-variable 'company-backends) '())" should be used (seems to be the most correct way)
  ;; OTHER EXAMPLE:
  ;; https://emacs.stackexchange.com/questions/23999/php-completion-with-company-does-not-work-on-local-variables
  ;;
  ;; Description of particular backends: https://company-mode.github.io/manual/Backends.html
  ;;
  ;; How TO MANIPULATE BACKENDS PER MAJOR MODE:
  ;; 1) clean and make local var (set (make-local-variable 'company-backends) '()) - from now it will be empty
  ;; 2) add your preferred backends - choose only the best options (and not add everything! - it will be messy and slow):
  ;; 2A) the most important - backends should be at the beginning (from the left side of the list)
  ;; 2B) the less important - should be declared at the end (from the right side of the list)
  ;; 3) run emacs and check variable `company-backends' as `describe-variable' and also use M-x `company-diag' in real examples
  ;; 4) OPTIONAL: check & change: company-transformers '(company-sort-by-occurrence) > sorting options for particular mode
  ;; 5) OPTIONAL: no new line for particular mode: https://github.com/joaotavora/yasnippet/issues/192
  ;; just add: (setq require-final-newline nil) into particular mode hook

  (when (require 'php-mode nil 'noerror)
    (progn
      (add-hook 'php-mode-hook (lambda ()
				 (set (make-local-variable 'company-backends) '())
				 ;; company-capf, company-gtags
				 (add-to-list 'company-backends '(company-abbrev :separate company-ac-php-backend company-keywords company-dabbrev-code company-files company-dabbrev))))))

  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (set (make-local-variable 'company-backends) '())
				    ;; capf is working great with elisp code, and company-elisp is obsolete
				    ;; company-capf, company-gtags
				    (add-to-list 'company-backends '(company-abbrev :separate company-keywords company-capf company-files company-dabbrev-code))))

  (add-hook 'sh-mode-hook (lambda ()
			    (set (make-local-variable 'company-backends) '())
			    ;; company-capf, company-gtags
			    (add-to-list 'company-backends '(company-abbrev :separate company-keywords company-dabbrev-code company-files company-dabbrev))))

  (add-hook 'nix-mode-hook (lambda ()
			     (set (make-local-variable 'company-backends) '())
			     ;; company-capf, company-gtags
			     (add-to-list 'company-backends '(company-abbrev :separate company-keywords company-dabbrev-code company-files company-dabbrev))))

  (add-hook 'cperl-mode-hook (lambda ()
			       (set (make-local-variable 'company-backends) '())
			       ;; company-capf, company-gtags
			       (add-to-list 'company-backends '(company-abbrev :separate company-keywords company-dabbrev-code company-files company-dabbrev))))

  (add-hook 'web-mode-hook (lambda ()
			     (set (make-local-variable 'company-backends) '())
			     ;; company-capf, company-gtags
			     (add-to-list 'company-backends '(company-abbrev :separate company-keywords company-dabbrev-code company-files company-dabbrev))))

  (add-hook 'python-mode-hook (lambda ()
				(set (make-local-variable 'company-backends) '())
				;; company-capf, company-gtags
				(add-to-list 'company-backends '(company-abbrev :separate company-anaconda company-dabbrev-code company-keywords company-files company-dabbrev))))

  (add-hook 'snippet-mode-hook (lambda ()
				 (set (make-local-variable 'company-backends) '())
				 ;; company-capf, company-gtags
				 (add-to-list 'company-backends '(company-abbrev :separate company-dabbrev-code company-files company-keywords company-dabbrev))))

  (add-hook 'org-mode-hook (lambda ()
			     (set (make-local-variable 'company-backends) '())
			     ;; company-capf, company-gtags
			     (add-to-list 'company-backends '(company-abbrev :separate company-dabbrev-code company-files company-keywords company-dabbrev))))

  ;; optional manipulations: https://github.com/doomemacs/doomemacs/issues/3908
  (add-hook 'notmuch-message-mode-hook (lambda ()
					 (set (make-local-variable 'company-backends) '())
					 ;; company-gtags
					 (add-to-list 'company-backends '(company-abbrev :separate company-capf notmuch-company company-files company-dabbrev company-dabbrev-code))
					 (setq require-final-newline nil))) ;; no new lines after inserting snippet

  (defun cfg/-hook-c-cpp-mode ()
    (set (make-local-variable 'company-backends) '())
    ;; company-capf, company-gtags
    (add-to-list 'company-backends '(company-abbrev :separate company-clang company-cmake company-dabbrev-code company-keywords company-files company-dabbrev)))

  ;; (add-hook 'c-mode-common-hook 'cfg/-hook-c-cpp-mode)
  (add-hook 'c-mode-hook 'cfg/-hook-c-cpp-mode)
  (add-hook 'c++-mode-hook 'cfg/-hook-c-cpp-mode)

  (defun cfg/-hook-SHARED-COMPANY-mode ()
    (set (make-local-variable 'company-backends) '())
    ;; company-gtags
    (add-to-list 'company-backends '(company-abbrev :separate company-capf company-dabbrev-code company-keywords company-files company-dabbrev)))

  (add-hook 'conf-mode 'cfg/-hook-SHARED-COMPANY-mode)
  (add-hook 'conf-unix-mode 'cfg/-hook-SHARED-COMPANY-mode)
  (add-hook 'conf-space-mode 'cfg/-hook-SHARED-COMPANY-mode)
  (add-hook 'conf-windows-mode 'cfg/-hook-SHARED-COMPANY-mode)
  (add-hook 'conf-xdefaults-mode 'cfg/-hook-SHARED-COMPANY-mode)
  (add-hook 'nxml-mode 'cfg/-hook-SHARED-COMPANY-mode)
  (add-hook 'js-json-mode 'cfg/-hook-SHARED-COMPANY-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ADDITIONAL PACKAGES FOR COMPANY (BACKENDS):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AI: https://github.com/TommyX12/company-tabnine

;; python:
;; https://github.com/pythonic-emacs/company-anaconda
(use-package company-anaconda
  :after anaconda-mode
  :ensure t
  )

;; python - OPTIONAL:
;; https://github.com/emacsorphanage/company-jedi
;; (use-package company-jedi
;;   :ensure t
;; )

(when (require 'php-mode nil 'noerror)
  ;; php
  ;; https://github.com/xcwen/ac-php
  (progn
    (use-package company-php
      :after company
      :ensure t
      :config
      ;; ac-php uses its own tags format. By default all tags located at ~/.ac-php/tags-<project-directory>. For example, if the real path of the project is /home/jim/ac-php/phptest, then tags will be placed at ~/.ac-php/tags-home-jim-ac-php-phptest/. And you can redefine the base path (~/.ac-php) using ac-php-tags-path variable.
      (setq ac-php-tags-path (expand-file-name ".cache/.ac-php" user-emacs-directory))
      (add-hook 'php-mode-hook (lambda ()
				 (require 'company-php)
				 (ac-php-core-eldoc-setup))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ADDITIONAL PACKAGES AND CONFIGURATION FOR COMPANY (NOT BACKENDS):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/company-mode/company-statistics
(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode)
  (setq company-statistics-size 5000)
  (setq company-statistics-file (expand-file-name ".cache/company-statistics-cache.el" user-emacs-directory)))

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
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  (add-hook 'company-mode-hook #'company-quickhelp-mode)
  (add-hook 'company-quickhelp-mode 'turn-on-tempbuf-mode)
  (define-key company-active-map (kbd "<f1>") #'cfg/company-show-doc-buffer-f1))

;; (company-quickhelp-mode 1)
;; (setq company-quickhelp-delay 0.2)
(custom-set-variables
 '(company-quickhelp-color-background "yellow") ;; "blue"
 '(company-quickhelp-color-foreground "black")  ;; "white"
 '(company-quickhelp-delay 0.0)
 '(company-quickhelp-mode t))

;; load keybindings from general.el framework:
(require 'cfg-gen-op-company)

(provide 'cfg-op-company)
;;; cfg-op-company.el ends here
