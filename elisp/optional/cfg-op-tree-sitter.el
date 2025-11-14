;;; cfg-op-tree-sitter.el --- tree sitter -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree sitter for Emacs.
;;
;; https://emacs-tree-sitter.github.io/installation/
;; https://tree-sitter.github.io/tree-sitter/
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; "For Emacs 29+, please use the built-in integration instead."

;;; Code:

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t)

;; https://github.com/emacs-tree-sitter/ts-fold
;; https://github.com/emacs-tree-sitter/treesit-fold
;; quelpa is required for installation:
(use-package treesit-fold
  :quelpa (treesit-fold :fetcher github :repo "emacs-tree-sitter/treesit-fold"))
(global-treesit-fold-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom list of defined tree-sitter grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq treesit-language-source-alist
      '((perl . ("https://github.com/tree-sitter-perl/tree-sitter-perl" "release"))
        (pod  . ("https://github.com/tree-sitter-perl/tree-sitter-pod" "release"))
        (php  . ("https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))
        (lua  . ("https://github.com/tree-sitter-grammars/tree-sitter-lua" "main"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Workarounds and defuns for "tree-sitter-langs" in ELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cfg/treesit-link-ts-langs ()
  "Create symlinks for tree-sitter-langs shared objects from ELPA into user tree-sitter directory.
If symlink creation fails, do nothing."
  (interactive)
  (let* ((ts-elpa-dir (expand-file-name "elpa/" user-emacs-directory))
         ;; find the tree-sitter-langs directory regardless of version
         (ts-lang-dir (car (file-expand-wildcards
                            (expand-file-name "tree-sitter-langs-*/bin" ts-elpa-dir))))
         (ts-target-dir (expand-file-name "tree-sitter/" user-emacs-directory)))
    (when ts-lang-dir
      (unless (file-exists-p ts-target-dir)
        (make-directory ts-target-dir t))
      ;; iterate over each *.so file in bin/
      (dolist (file (directory-files ts-lang-dir t "\\.so$"))
        (let* ((lang (file-name-base file))
               (target (expand-file-name
                        (format "libtree-sitter-%s.so" lang)
                        ts-target-dir)))
          ;; create a symlink only if target does not exist
          (unless (file-exists-p target)
            (condition-case nil
                (make-symbolic-link file target)
              (error nil))))))))

(defun cfg/treesit-reinstall-grammar ()
  "Prompt for a language from `treesit-language-source-alist`,
remove the existing .so file if present, and reinstall the grammar."
  (interactive)
  (let* ((langs (mapcar #'car treesit-language-source-alist))
         ;; Prompt the user to choose a language from the list
         (choice (intern (completing-read "Choose language: " langs nil t)))
         ;; Build the path to the corresponding shared object file
         (libfile (expand-file-name
                   (format "libtree-sitter-%s.so" choice)
                   (expand-file-name "tree-sitter" user-emacs-directory))))
    ;; If the file exists (e.g., as a symlink or regular file), delete it
    (when (file-exists-p libfile)
      (delete-file libfile))
    ;; Reinstall the grammar for the chosen language
    (treesit-install-language-grammar choice)))

(defun cfg/treesit-reinstall-all-grammars ()
  "Reinstall all grammars from `treesit-language-source-alist`.
For each language, remove the existing .so file if present, then reinstall."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (let* ((name (car lang))
           ;; Build the path to the corresponding shared object file
           (libfile (expand-file-name
                     (format "libtree-sitter-%s.so" name)
                     (expand-file-name "tree-sitter" user-emacs-directory))))
      ;; If the file exists (regular file or symlink), delete it
      (when (file-exists-p libfile)
        (delete-file libfile))
      ;; Reinstall the grammar for the current language
      (treesit-install-language-grammar name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl-ts-mode

(use-package perl-ts-mode
  :ensure t
  )

(use-package nix-ts-mode
  :ensure t
  )

;; https://github.com/nverno/emacs-lisp-ts-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/emacs-php/php-ts-mode
;; Since php 8.4 is broken

;; (if (require 'php-ts-mode nil 'noerror)
;;     (message "php-ts-mode is already installed")
;;   (package-vc-install "https://github.com/emacs-php/php-ts-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
	(bash-mode . bash-ts-mode)
	(php-mode . php-ts-mode)
	(js2-mode . js-ts-mode)
	(nix-mode . nix-ts-mode)
	(cperl-mode . perl-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	;; (mhtml-mode . html-ts-mode)
	(html-mode . html-ts-mode)
	(python-mode . python-ts-mode)))

(require 'cfg-gen-op-tree-sitter-mode)

(provide 'cfg-op-tree-sitter)
;;; cfg-op-tree-sitter.el ends here
