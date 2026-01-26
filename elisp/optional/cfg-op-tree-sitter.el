;;; cfg-op-tree-sitter.el --- tree-sitter / treesit integration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree-sitter configuration for Emacs.
;;
;; This module intentionally combines:
;; - the external `tree-sitter' / `tree-sitter-langs' packages
;; - the built-in Emacs 29+ `treesit-*' infrastructure
;;
;; The hybrid approach is used to:
;; - reuse prebuilt grammars from ELPA (tree-sitter-langs)
;; - keep compatibility with older configurations
;; - enable newer Emacs features such as *-ts-mode, treesit-fold and treesit-auto
;;
;; References:
;; https://emacs-tree-sitter.github.io/installation/
;; https://tree-sitter.github.io/tree-sitter/
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Core packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; External tree-sitter package (legacy but still useful for grammar handling)
(use-package tree-sitter
  :ensure t
  :config
  ;; (global-tree-sitter-mode)
  ;; Local glue code for tree-sitter related configuration
  (require 'cfg-gen-op-tree-sitter-mode))

;; Prebuilt grammars distributed via ELPA
(use-package tree-sitter-langs
  :ensure t)

;; https://github.com/emacs-tree-sitter/ts-fold
;; https://github.com/emacs-tree-sitter/treesit-fold
;; quelpa is required for installation:
(use-package treesit-fold
  :quelpa (treesit-fold :fetcher github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (global-treesit-fold-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom list of defined tree-sitter grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grammars that should be buildable via `treesit-install-language-grammar'
(setq treesit-language-source-alist
      '((perl . ("https://github.com/tree-sitter-perl/tree-sitter-perl" "release"))
        (pod  . ("https://github.com/tree-sitter-perl/tree-sitter-pod" "release"))
        (php  . ("https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))
        (lua  . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "main"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal helpers (non interactive)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Internal helpers are prefixed with `cfg/treesit--' and are not part of the
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Workarounds and defuns for "tree-sitter-langs" in ELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following commands manage symlinks and grammar binaries in
;; ~/.emacs.d/tree-sitter/. This is necessary because:
;; - `tree-sitter-langs` installs grammars inside ELPA directories
;; - `treesit` expects grammars to be found in a user tree-sitter directory

(defun cfg/treesit-full-reinstall ()
  "Full reinstallation: delete the user tree-sitter directory,
recreate symlinks, and reinstall all configured grammars."
  (interactive)
  (let ((ts-target-dir (cfg/-treesit-user-ts-dir)))
    (when (file-directory-p ts-target-dir)
      (condition-case nil
          (delete-directory ts-target-dir t)
        (error nil)))
    (cfg/treesit-link-ts-langs)
    (cfg/treesit-reinstall-all-grammars)))

(defun cfg/treesit-link-ts-langs ()
  "Create symlinks for tree-sitter-langs shared objects from ELPA
    into the user tree-sitter directory.

    Only symbolic links are removed and recreated. If symlink creation
    fails for any reason, the error is silently ignored."
  (interactive)
  (let* ((ts-elpa-dir (expand-file-name "elpa/" user-emacs-directory))
         (ts-lang-dir (car (file-expand-wildcards
                            (expand-file-name "tree-sitter-langs-*/bin" ts-elpa-dir))))
         (ts-target-dir (cfg/-treesit-user-ts-dir)))
    (when ts-lang-dir
      ;; Ensure the target directory exists
      (unless (file-directory-p ts-target-dir)
        (make-directory ts-target-dir t))

      ;; Remove only existing symlinks to shared objects
      (dolist (f (directory-files ts-target-dir t "\\.so$"))
        (when (file-symlink-p f)
          (cfg/-treesit-delete-if-exists f)))

      ;; Create fresh symlinks pointing to ELPA-installed grammars
      (dolist (file (directory-files ts-lang-dir t "\\.so$"))
        (let* ((lang (file-name-base file))
               (target (cfg/-treesit-lib-path lang)))
          (condition-case nil
              (make-symbolic-link file target t)
            (error nil)))))))

(defun cfg/treesit-reinstall-grammar ()
  "Prompt for a language from `treesit-language-source-alist',
remove the existing shared object if present, and reinstall the grammar."
  (interactive)
  (let* ((langs (mapcar #'car treesit-language-source-alist))
         (choice (intern (completing-read "Choose language: " langs nil t))))
    (cfg/-treesit-delete-if-exists (cfg/-treesit-lib-path choice))
    (treesit-install-language-grammar choice)))

(defun cfg/treesit-reinstall-all-grammars ()
  "Reinstall all grammars from `treesit-language-source-alist'.

For each language, the existing shared object (or symlink) is removed
before reinstalling the grammar from its source repository."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (let ((name (car lang)))
      (cfg/-treesit-delete-if-exists (cfg/-treesit-lib-path name))
      (treesit-install-language-grammar name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl-ts-mode

;; Tree-sitter based Perl major mode
(use-package perl-ts-mode
  :ensure t)

;; https://github.com/nverno/emacs-lisp-ts-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/emacs-php/php-ts-mode
;; Since php 8.4 is broken
;; (grammar / mode issues upstream; kept disabled intentionally)

;; (if (require 'php-ts-mode nil 'noerror)
;;     (message "php-ts-mode is already installed")
;;   (package-vc-install "https://github.com/emacs-php/php-ts-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; https://github.com/renzmann/treesit-auto
;; Automatically prefer *-ts-mode variants when grammars are available
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist
   '(yaml bash php cperl perl typescript json css html python)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Major mode remapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Explicit major-mode remapping to tree-sitter based modes
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (php-mode . php-ts-mode)
        (js2-mode . js-ts-mode)
        ;; (nix-mode . nix-ts-mode)
        (cperl-mode . perl-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        ;; (mhtml-mode . html-ts-mode)
        (html-mode . html-ts-mode)
        (python-mode . python-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable treesitter for particular mode:

;; Explicitly remove remapping for modes where tree-sitter is undesirable
(setq major-mode-remap-alist
      (assq-delete-all 'nix-mode major-mode-remap-alist))

;; (use-package nix-ts-mode
;;   :ensure t
;;   )

(provide 'cfg-op-tree-sitter)
;;; cfg-op-tree-sitter.el ends here
