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
         ;; no working, DO NOT USE! See later in section "Update capf".
         ;; ([remap completion-at-point]               . company-capf) ;; completion-at-point-functions = CAPF
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
  ;; COMPANY FRONTENDS:
  ;;
  (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; COMPANY BACKENDS - CONFIGURATION PER MODE (LOCALLY, NOT GLOBALLY):
  ;;
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

  ;; php
  (when (require 'php-mode nil 'noerror)
    (progn
      (dolist (hook '(php-mode-hook php-ts-mode-hook))
        (add-hook hook
                  (lambda ()
                    (set (make-local-variable 'company-backends) '())
                    ;; company-capf, company-gtags
                    ;; OPTIONALLY load "company-php" as "company-ac-php-backend"
                    ;; (if lsp is not to be used, it is best to enable it then):
                    (if (require 'company-php nil 'noerror)
                        (add-to-list 'company-backends
                                     '(company-abbrev :separate
                                                      company-ac-php-backend
                                                      company-keywords
                                                      company-dabbrev-code
                                                      company-files
                                                      company-dabbrev))
                      (add-to-list 'company-backends
                                   '(company-capf :separate
                                                  company-abbrev :separate
                                                  company-keywords
                                                  company-dabbrev-code
                                                  company-files
                                                  company-dabbrev))))))
      ))

  ;; emacs lisp (elisp)
  (dolist (hook '(emacs-lisp-mode-hook emacs-lisp-ts-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; capf is working great with elisp code, and company-elisp is obsolete
                ;; company-capf, company-gtags
                (add-to-list 'company-backends
                             '(company-abbrev :separate
                                              company-keywords
                                              company-capf
                                              company-files
                                              company-dabbrev-code)))))

  ;; bash / shell
  (dolist (hook '(sh-mode-hook bash-ts-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; company-capf, company-gtags
                (add-to-list 'company-backends
                             '(company-abbrev :separate
                                              company-keywords
                                              company-dabbrev-code
                                              company-files
                                              company-dabbrev)))))


  ;; lua
  (dolist (hook '(lua-mode-hook lua-ts-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; company-capf, company-gtags
                (add-to-list 'company-backends
                             '(company-abbrev :separate
                                              company-keywords
                                              company-dabbrev-code
                                              company-files
                                              company-dabbrev)))))

  ;; nix
  (dolist (hook '(nix-mode-hook nix-ts-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; company-capf, company-gtags
                ;; (add-to-list 'company-backends
                ;;              '(company-capf :separate company-abbrev
                ;;                             :separate company-keywords
                ;;                             company-dabbrev-code
                ;;                             company-files
                ;;                             company-dabbrev)))
                (add-to-list 'company-backends
                             '(company-abbrev :separate
                                              company-keywords
                                              company-dabbrev-code
                                              company-files
                                              company-dabbrev)))))

  ;; perl
  (dolist (hook '(cperl-mode-hook perl-ts-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; company-capf, company-gtags
                (add-to-list 'company-backends
                             '(company-abbrev :separate
                                              company-keywords
                                              company-dabbrev-code
                                              company-files
                                              company-dabbrev)))))

  ;; web-mode / html
  (dolist (hook '(web-mode-hook
                  html-mode-hook
                  html-ts-mode-hook
                  mhtml-mode-hook
                  sgml-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; company-capf, company-gtags
                ;; (add-to-list 'company-backends
                ;;              '(company-abbrev :separate
                ;;                                company-keywords
                ;;                                company-dabbrev-code
                ;;                                company-files
                ;;                                company-dabbrev))
                (if (require 'company-web nil 'noerror)
                    ;; optionally add company-ac-php-backend if desired
                    (add-to-list 'company-backends
                                 '(company-web-html :separate
                                                    company-abbrev :separate
                                                    company-keywords
                                                    company-dabbrev-code
                                                    company-files
                                                    company-dabbrev))
                  (add-to-list 'company-backends
                               '(company-abbrev :separate
                                                company-keywords
                                                company-dabbrev-code
                                                company-files
                                                company-dabbrev))))))

  ;; Python
  (dolist (hook '(python-mode-hook python-ts-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; company-capf, company-gtags
                (if (require 'anaconda-mode nil 'noerror)
                    (add-to-list 'company-backends
                                 '(company-abbrev :separate
                                                  company-anaconda
                                                  company-dabbrev-code
                                                  company-keywords
                                                  company-files
                                                  company-dabbrev))
                  (add-to-list 'company-backends
                               '(company-abbrev :separate
                                                company-dabbrev-code
                                                company-keywords
                                                company-files
                                                company-dabbrev))))))

  ;; snippets
  (add-hook 'snippet-mode-hook (lambda ()
				 (set (make-local-variable 'company-backends) '())
				 ;; company-capf, company-gtags
				 (add-to-list 'company-backends '(company-abbrev :separate company-dabbrev-code company-files company-keywords company-dabbrev))))

  ;; org-mode
  (add-hook 'org-mode-hook (lambda ()
			     (set (make-local-variable 'company-backends) '())
			     ;; company-capf, company-gtags
			     (add-to-list 'company-backends '(company-abbrev :separate company-dabbrev-code company-files company-keywords company-dabbrev))))

  ;; Notmuch
  ;; optional manipulations: https://github.com/doomemacs/doomemacs/issues/3908
  (add-hook 'notmuch-message-mode-hook (lambda ()
					 (set (make-local-variable 'company-backends) '())
					 ;; company-gtags
					 (add-to-list 'company-backends '(company-abbrev :separate company-capf notmuch-company company-files company-dabbrev company-dabbrev-code))
					 (setq require-final-newline nil))) ;; no new lines after inserting snippet

  ;; C/C++
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; company-capf, company-gtags
                (add-to-list 'company-backends
                             '(company-abbrev :separate
                                              company-clang
                                              company-cmake
                                              company-dabbrev-code
                                              company-keywords
                                              company-files
                                              company-dabbrev)))))


  ;; SHARED, conf modes:
  (dolist (hook '(conf-mode-hook
                  conf-unix-mode-hook
                  conf-space-mode-hook
                  conf-windows-mode-hook
                  conf-xdefaults-mode-hook
                  nxml-mode-hook
                  yaml-ts-mode-hook
                  yaml-mode-hook
                  json-ts-mode-hook
                  json-mode-hook
                  js-json-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'company-backends) '())
                ;; company-gtags
                (add-to-list 'company-backends
                             '(company-abbrev :separate
                                              company-capf
                                              company-dabbrev-code
                                              company-keywords
                                              company-files
                                              company-dabbrev)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Update capf function:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ([remap completion-at-point]               . company-capf) ;; completion-at-point-functions = CAPF
;; ^ unfortunately, this DOESN'T WORK correctly, so we have to use consult and vertico:
;; https://github.com/minad/vertico?tab=readme-ov-file#completion-at-point-and-completion-in-region

;; If you want to make sure that libraries are actually available (not just available in the load path):
;;
;; `featurep' checks if a given package has already been loaded (provided),
;; but `require' with `noerror' attempts to load the package and doesn't report an error if it's not present.
;; This avoids Emacs trying to set `completion-in-region-function' to something that doesn't exist:

(when (and (require 'vertico nil 'noerror)
           (require 'consult nil 'noerror))
  (setq completion-in-region-function #'consult-completion-in-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ADDITIONAL PACKAGES FOR COMPANY (BACKEND):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AI: https://github.com/TommyX12/company-tabnine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ADDITIONAL PACKAGES AND CONFIGURATION FOR COMPANY (FRONTEND):
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
 '(company-quickhelp-color-background "black") ;; "blue" for wayland/pgtk
 '(company-quickhelp-color-foreground "white")  ;; "white" for waylant/pgtk
 '(company-quickhelp-use-propertized-text nil)
 '(company-quickhelp-delay 0.0)
 '(company-quickhelp-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load keybindings from general.el framework:
(require 'cfg-gen-op-company)

(provide 'cfg-op-company)
;;; cfg-op-company.el ends here
