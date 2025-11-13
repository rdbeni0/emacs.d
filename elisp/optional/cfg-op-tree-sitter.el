;;; cfg-op-tree-sitter.el --- tree sitter -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree sitter for Emacs.
;;
;; https://emacs-tree-sitter.github.io/installation/
;; https://tree-sitter.github.io/tree-sitter/
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; "For Emacs 29+, please use the built-in integration instead."
;; https://lists.gnu.org/archive/html/emacs-devel/2022-11/msg01443.html

;;; Code:

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t)

;; (setq treesit-language-source-alist
;;       '((bash        "https://github.com/tree-sitter/tree-sitter-bash")
;;         (c           "https://github.com/tree-sitter/tree-sitter-c")
;;         (cpp         "https://github.com/tree-sitter/tree-sitter-cpp")
;;         (css         "https://github.com/tree-sitter/tree-sitter-css")
;;         (dockerfile  "https://github.com/camdencheek/tree-sitter-dockerfile")
;;         (elisp       "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go          "https://github.com/tree-sitter/tree-sitter-go")
;;         (html        "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript  "https://github.com/tree-sitter/tree-sitter-javascript" "src")
;;         (json        "https://github.com/tree-sitter/tree-sitter-json")
;;         (lua         "https://github.com/tree-sitter-grammars/tree-sitter-lua")
;;         (make        "https://github.com/alemuller/tree-sitter-make")
;;         (markdown    "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python      "https://github.com/tree-sitter/tree-sitter-python")
;;         (rust        "https://github.com/tree-sitter/tree-sitter-rust")
;; 	(blade       "https://github.com/amaanq/tree-sitter-blade")
;;         (twig        "https://github.com/the-mikedavis/tree-sitter-twig")
;;         (toml        "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx         "https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src")
;;         (typescript  "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src")
;;         (yaml        "https://github.com/ikatyang/tree-sitter-yaml")))
;; (add-to-list 'treesit-language-source-alist
;;              '(php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))

(add-to-list 'treesit-language-source-alist
	     '(perl . ("https://github.com/tree-sitter-perl/tree-sitter-perl" "release")))
(add-to-list 'treesit-language-source-alist
	     '(pod . ("https://github.com/tree-sitter-perl/tree-sitter-pod" "release")))
(treesit-install-language-grammar 'perl)
(treesit-install-language-grammar 'pod)

;; Workaround for "tree-sitter-langs" in ELPA
(let* ((ts-elpa-dir (expand-file-name "elpa/" user-emacs-directory))
       ;; find the tree-sitter-langs directory regardless of version
       (ts-lang-dir (car (file-expand-wildcards
                          (expand-file-name "tree-sitter-langs-*/bin" ts-elpa-dir))))
       (ts-target-dir (expand-file-name "tree-sitter/" user-emacs-directory)))
  (when ts-lang-dir
    (unless (file-exists-p ts-target-dir)
      (make-directory ts-target-dir t))
    ;; for each *.so file in bin/
    (dolist (file (directory-files ts-lang-dir t "\\.so$"))
      (let* ((lang (file-name-base file))
             (target (expand-file-name
                      (format "libtree-sitter-%s.so" lang)
                      ts-target-dir)))
        ;; create a symlink (or copy if the system does not support symlinks
        (unless (file-exists-p target)
          (condition-case nil
              (make-symbolic-link file target)
            (error (copy-file file target))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl-ts-mode

(use-package perl-ts-mode
  :ensure t
  )

(use-package nix-ts-mode
  :ensure t
  )

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
	(fish-mode . fish-ts-mode)
	(python-mode . python-ts-mode)))

(provide 'cfg-op-tree-sitter)
;;; cfg-op-tree-sitter.el ends here
