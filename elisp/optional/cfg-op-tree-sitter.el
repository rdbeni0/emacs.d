;;; cfg-op-tree-sitter.el --- tree-sitter / treesit integration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree-sitter configuration for Emacs.
;;
;; This module uses the built-in Emacs 29+ `treesit-*' infrastructure.
;;
;; References:
;; https://emacs-tree-sitter.github.io/installation/
;; https://tree-sitter.github.io/tree-sitter/
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Legacy packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; External tree-sitter package (legacy but still useful for grammar handling):
;;;; "For Emacs 29+, please use the built-in integration instead of this package."
;; (use-package tree-sitter
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; (global-tree-sitter-mode)
;;   )
;;
;;;; Prebuilt grammars distributed via ELPA: should only be used in old Emacs.
;;;; Do NOT use `tree-sitter-langs' for newer versions of emacs.
;;(use-package tree-sitter-langs
;;  :defer t
;;  :ensure t
;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Core packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://cgit.git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
(require 'treesit)
(require 'cfg-gen-op-tree-sitter-mode)

;; https://github.com/emacs-tree-sitter/ts-fold
;; https://github.com/emacs-tree-sitter/treesit-fold
;; quelpa is required for installation:
(use-package treesit-fold
  :defer t
  :quelpa (treesit-fold :fetcher github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (global-treesit-fold-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom list of defined treesit grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grammars that should be buildable via `treesit-install-language-grammar'
;; You can use the sources from here:
;; https://github.com/renzmann/treesit-auto/blob/main/treesit-auto.el
;; https://github.com/search?q=treesit-language-source-alist+language%3A%22Emacs+Lisp%22++&type=code
(setq treesit-language-source-alist
      '(
        (pod  . ("https://github.com/tree-sitter-perl/tree-sitter-pod" "release"))
        (php  . ("https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))
        (phpdoc . ("https://github.com/claytonrcarter/tree-sitter-phpdoc" "master"))
        (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc" "master"))
        (lua  . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "main"))
        (bash        . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c           . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cmake       . ("https://github.com/uyha/tree-sitter-cmake"))
        (css         . ("https://github.com/tree-sitter/tree-sitter-css"))
        (dockerfile  . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (go          . ("https://github.com/tree-sitter/tree-sitter-go"))
        (html        . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript  . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (json        . ("https://github.com/tree-sitter/tree-sitter-json"))
        (make        . ("https://github.com/tree-sitter-grammars/tree-sitter-make"))
        (markdown    . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"))
        (nix         . ("https://github.com/nix-community/tree-sitter-nix"))
        (org         . ("https://github.com/milisims/tree-sitter-org"))
        (perl        . ("https://github.com/ganezdragon/tree-sitter-perl"))
        ;; (perl . ("https://github.com/tree-sitter-perl/tree-sitter-perl" "release"))
        (python      . ("https://github.com/tree-sitter/tree-sitter-python"))
        (sql         . ("https://github.com/DerekStride/tree-sitter-sql" "gh-pages"))
        (toml        . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (typescript  . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (vue         . ("https://github.com/tree-sitter-grammars/tree-sitter-vue"))
        (yaml        . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal helpers (non interactive)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Internal helpers are prefixed with `cfg/-treesit' and are not part of the
;; public configuration API. They exist solely to avoid duplication and keep
;; filesystem-related logic consistent.

(defun cfg/-treesit-user-ts-dir ()
  "Return the user tree-sitter directory used by Emacs."
  (expand-file-name "tree-sitter/" user-emacs-directory))

(defun cfg/-treesit-lib-path (lang)
  "Return full path to LANG shared library in user tree-sitter directory."
  (expand-file-name (format "libtree-sitter-%s.so" lang)
                    (cfg/-treesit-user-ts-dir)))

(defun cfg/-treesit-delete-if-exists (file)
  "Delete FILE if it exists, ignoring all filesystem errors."
  (when (file-exists-p file)
    (condition-case nil
        (delete-file file)
      (error nil))))

(defun cfg/-treesit-reinstall-all-grammars ()
  "Reinstall all grammars from `treesit-language-source-alist'.
For each language, the existing shared object (or symlink) is removed
before reinstalling the grammar from its source repository."
  (dolist (lang treesit-language-source-alist)
    (let ((name (car lang)))
      (cfg/-treesit-delete-if-exists (cfg/-treesit-lib-path name))
      (treesit-install-language-grammar name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Workarounds and defuns for `treesit' grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cfg/treesit-reinstall-full ()
  "Full reinstallation: delete the user tree-sitter directory,
and reinstall all declared grammars via different methods."
  (interactive)
  (let ((ts-target-dir (cfg/-treesit-user-ts-dir)))
    (when (file-directory-p ts-target-dir)
      (condition-case nil
          (delete-directory ts-target-dir t)
        (error nil)))
    (cfg/-treesit-reinstall-all-grammars)
    (treesit-auto-install-all)
    ;; PHP parsers should be installed separately:
    (require 'php-ts-mode)
    (php-ts-mode-install-parsers)))

(defun cfg/treesit-reinstall-grammar ()
  "Prompt for a language from `treesit-language-source-alist',
remove the existing shared object if present, and reinstall the grammar."
  (interactive)
  (let* ((langs (mapcar #'car treesit-language-source-alist))
         (choice (intern (completing-read "Choose language: " langs nil t))))
    (cfg/-treesit-delete-if-exists (cfg/-treesit-lib-path choice))
    (treesit-install-language-grammar choice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl-ts-mode

;; Tree-sitter based Perl major mode
(use-package perl-ts-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/nverno/emacs-lisp-ts-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/emacs-php/php-ts-mode

(if (require 'php-ts-mode nil 'noerror)
    (defun cfg/php-ts-mode-install-parsers ()
      "Install all the required treesitter parsers.
‘php-ts-mode--language-source-alist’ defines which parsers to install."
      (interactive)
      (require 'php-ts-mode)
      (php-ts-mode-install-parsers))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://github.com/renzmann/treesit-auto
;; Automatically prefer *-ts-mode variants when grammars are available

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist
   '(yaml bash lua php cperl perl typescript json css html python)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Major mode remapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Explicit major-mode remapping to tree-sitter based modes
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (php-mode . php-ts-mode)
        (js2-mode . js-ts-mode)
        (nix-mode . nix-ts-mode) ;; comment to disable nix-ts-mode
        (cperl-mode . perl-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (lua-mode . lua-ts-mode)
        (css-mode . css-ts-mode)
        ;; (mhtml-mode . html-ts-mode)
        (html-mode . html-ts-mode)
        (python-mode . python-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable treesitter for particular mode:

;; Explicitly remove remapping for modes where tree-sitter is undesirable
;; (setq major-mode-remap-alist ;; e.g. uncomment to disable nix-ts-mode
;;       (assq-delete-all 'nix-mode major-mode-remap-alist))

(use-package nix-ts-mode ;; comment to disable nix-ts-mode
  :ensure t
  )

(provide 'cfg-op-tree-sitter)
;;; cfg-op-tree-sitter.el ends here
