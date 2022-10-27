;;; cfg-formatting.el ---  configuration for formatting -*- lexical-binding: t -*-
;;; Commentary:

;; Config for code formatters.
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; FORMAT-ALL-MODE :  https://github.com/lassik/emacs-format-all-the-code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package format-all
  :ensure t
  :hook (;; if you want format automatically after "save" file, then format-all-mode should be turned on.
	 ;; For example:
	 ;;
	 ;; (prog-mode . format-all-mode)
	 ;; ^ or any other, particular mode.
	 ;;
	 ;;;; do not ask for a choice and always use default formatter:
	 ;; (format-all-mode . format-all-ensure-formatter)
	 (prog-mode . format-all-ensure-formatter))
  :config

  ;;;; set to 't' if you want see detailed debug in the "Messages" buffer:
  ;; (setq format-all-debug t)
  (setq format-all-debug nil)

  ;; for more options please see source code of package:
  (custom-set-variables
   '(format-all-default-formatters
     '(("Assembly" asmfmt)
       ("C" clang-format)
       ("C#" clang-format)
       ("C++" clang-format)
       ("CMake" cmake-format)
       ("CSS" (prettier "--print-width" "185"))
       ("Dockerfile" dockfmt)
       ("Elixir" mix-format)
       ("Emacs Lisp" emacs-lisp)
       ("Fish" fish-indent)
       ("Fortran Free Form" fprettify)
       ("HTML" (html-tidy "-wrap" "185"))
       ("HTML+ERB" erb-format)
       ("Java" clang-format)
       ("JavaScript" (prettier "--print-width" "185"))
       ("JSON" (prettier "--print-width" "185"))
       ("JSON5" (prettier "--print-width" "185"))
       ("Jsonnet" jsonnetfmt)
       ("JSX" prettier)
       ("Kotlin" ktlint)
       ("LaTeX" latexindent)
       ("Less" (prettier "--print-width" "185"))
       ("Lua" lua-fmt)
       ("Markdown" (prettier "--print-width" "185"))
       ("Nix" nixpkgs-fmt)
       ("Objective-C" clang-format)
       ("OCaml" ocp-indent)
       ("Perl" (perltidy "--quiet" "--standard-error-output" "--perl-best-practices" "-l=185"))
       ("PHP" (prettier "--print-width" "185"))
       ("Python" (black "-l185"))
       ("SCSS" (prettier "--print-width" "185"))
       ("Shell" beautysh)
       ("SQL" sqlformat)
       ("Svelte" (prettier "--print-width" "185"))
       ("TOML" (prettier "--print-width" "185"))
       ("TSX" prettier)
       ("TypeScript" (prettier "--print-width" "185"))
       ("V" v-fmt)
       ("Verilog" istyle-verilog)
       ("Vue" prettier)
       ("XML" (html-tidy "-wrap" "185"))
       ("YAML" (prettier "--print-width" "185"))
       ("Zig" zig)
       ("_Angular" (prettier "--print-width" "185"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; PYTHON - OPTIONAL SETTINGS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/wbolster/emacs-python-black
;; (use-package python-black
;;   :demand t
;;   :after python
;;   :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; yapf formatter is optional - it has more options than black, but should be used with cautions:
;; https://github.com/JorisE/yapfify
(use-package yapfify
  :ensure t
  :defer t
  :config
  ;; (add-hook 'python-mode-hook 'yapf-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; PHP - OPTIONAL SETTINGS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cfg/-php-format ()
  "Run custom PHP formatter on the current file (via CLI)."
  (save-excursion
    (shell-command-on-region (point) (mark) (concat "prettier --parser php --print-width 180 --stdin-filepath " (buffer-file-name)) nil t)))

(defun cfg/php-custom-format ()
  "Format current php buffer."
  (interactive)
  (mark-whole-buffer)
  (cfg/-php-format)
  (message "Reformatted! In case of formatting errors, please undo buffer."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; PERL - OPTIONAL SETTINGS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar perl5-perltidy-executable "perltidy"
  "Location of perltidy executable.")

(defvar perl5-perltidy-options '("--quiet"
				 "--standard-error-output"
				 "--perl-best-practices"
				 "-l=185")
  "Command line options to pass to perltidy")

;; other and alternative option:
;;
;; (defun cfg/perltidy-format ()
;;     "Run perltidy on the current region."
;;    (interactive)
;;    (save-excursion
;;      (shell-command-on-region (point) (mark) "/usr/bin/perltidy -q" nil t)))

(defun cfg/perltidy-format ()
  "Format Perl5 code with perltidy.
   If region is active, operate on it, else operate on line."
  (interactive)
  (let ((old-point (point))
        (pos
         (if (use-region-p)
             (cons (region-beginning)
                   (if (char-equal ?\n (char-before (region-end)))
                       (region-end)
                     (save-excursion ;; must including terminating newline
                       (goto-char (region-end))
                       (1+ (line-end-position)))))
           (cons (line-beginning-position)
                 (1+ (line-end-position))))))
    (apply #'call-process-region (car pos) (cdr pos) perl5-perltidy-executable t '(t nil)
           perl5-perltidy-options)
    (goto-char old-point)))

(defun cfg/perltidy-format-buffer ()
  "Format current buffer with perltidy."
  (interactive)
  (mark-whole-buffer)
  (cfg/perltidy-format))

(defun cfg/perltidy-format-function ()
  "Format current function (sub) with perltidy."
  (interactive)
  (mark-defun)
  (cfg/perltidy-format))

(provide 'cfg-formatting)
;;; cfg-formatting.el ends here
