;;; cfg-perl-regexps.el --- configfuration for perl and regexps -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with perl programming and regular expressions

;;; Code:

(defun perl-quote-backslashing (plains func)
  (goto-char (point-min))
  (let ((re (concat plains "\\|\\(\\\\+\\)")))
    (while (re-search-forward re nil t)
      (if (match-beginning 1)
          (when (and (= 1 (logand 1 (- (match-end 1)  ;; an odd number
                                       (match-beginning 1))))
                     (> (point-max) (point)))
            (goto-char (1+ (point)))
            (funcall func (buffer-substring (- (point) 2) (point))))

        (funcall func (match-string 0)))))
  )

(defun perl-quote-single-backslashing (single-func)
  (perl-quote-backslashing
   "[$@']" (lambda (str)
             (funcall single-func
                      str
                      (cdr (assoc str '(("\\\"" . "\"")
                                        ("'"    . "\\'")
                                        ("\\$"  . "$")
                                        ("\\@"  . "@")
                                        ;; see perlop "Quote and Quote-like
                                        ;; Operators"
                                        ("\\t"  . "\t")
                                        ("\\n"  . "\n")
                                        ("\\r"  . "\r")
                                        ("\\f"  . "\f")
                                        ("\\b"  . "\b")
                                        ("\\a"  . "\a")
                                        ("\\e"  . "\e")))))))
  )

(defun perl-quote-single (&optional force)
  (interactive "P")
  (let ((orig-marker (point-marker))
        (add-parens  (and (looking-at "N?__")
                          (goto-char (match-end 0))))
        (beg         (point))
        (qq          nil))
    (or (looking-at "\"\\|\\(qq[{[{<]\\)")
        (error "Not a double-quoted string"))
    (setq qq (match-beginning 1))  ;; nil for "", number for qq{}

    (forward-sexp (if qq 2 1)) ;; to after string
    (let ((end (point)))
      (save-restriction
        (narrow-to-region beg end)

        ;; check for interpolation or unrecognised backslashing before
        ;; changing anything
        (unless force
          (perl-quote-single-backslashing
           (lambda (part rep)
             (unless rep
               (goto-char (- (point) (length part)))
               (if (= 1 (length part))
                   (error "Interpolation in string")
                 (error "Backslash in string, no replacement yet implemented"))))))

        (goto-char (point-min))
        (if qq
            (progn ;; qq{}
              (delete-region beg (1+ beg)) ;; qq{} becomes q{}
              (goto-char (1+ (point))))

          ;; plain ""
          (when add-parens
            (insert "(")
            (goto-char (point-max))
            (insert ")")
            (narrow-to-region (1+ (point-min)) (1- (point))))

          (delete-region (1- (point-max)) (point-max))
          (goto-char (point-max))
          (insert "'")


          (delete-region (point-min) (1+ (point-min)))
          (goto-char (point-min))
          (insert "'"))

        (narrow-to-region (point) (1- (point-max)))

        (perl-quote-single-backslashing
         (lambda (part rep)
           (when rep
             (delete-region (- (point) (length part))
                            (point))
             (insert rep))))))
    (goto-char orig-marker))
  )

(defun perl-quote-double ()
  (interactive)
  (let ((beg (point))
        (q  nil))
    (or (looking-at "'\\|\\(q[{[{<]\\)")
        (error "Not a single-quoted string"))
    (setq q (match-beginning 1))  ;; nil for ' number for q{

    (save-excursion
      (forward-sexp)
      (let ((end (point)))
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))

          (if q
              (insert "q")
            (goto-char (1- (point-max)))
            (delete-region (1- (point-max)) (point-max))
            (insert "\"")

            (delete-region (point-min) (1+ (point-min)))
            (goto-char (point-min))
            (insert "\"")

            (narrow-to-region (point) (1- (point-max))))

          (perl-quote-backslashing
           "[\"$@]" (lambda (part)
                      (let ((rep (or (cdr (assoc part '(("\\\'" . "'")
							("\""   . "\\\"")
							("$" . "\\$")
							("@" . "\\@")
							;; backslashed literals
							("\\\"" . "\\\"")
							("\\$"  . "\\$")
							("\\@"  . "\\@"))))
                                     (substring part 1))))
			(delete-region (- (point) (length part)) (point))
			(insert rep))))))))
  )

(defun perl-module-path (module-name)
  (let* ((file-name
	  (concat (replace-regexp-in-string "::" "/" module-name)
		  ".pm"))
	 (command-line
	  (concat "perl -M'"
		  module-name
		  "' -e'print $INC{q{"
		  file-name
		  "}}'"))
	 (path (shell-command-to-string command-line))
	 (cant-locate (string-match "^Can't locate " path)))
    (if cant-locate
	nil
      path))
  )

(defun find-perl-module (module-name)
  (interactive "sPerl module name: ")
  (let ((path (perl-module-path module-name)))
    (if path
	(find-file path)
      (error "Module '%s' not found" module-name)))
  )

(defvar perl5-perltidy-executable "perltidy"
  "Location of perltidy executable.")

(defvar perl5-perltidy-options '()
  "Command line options to pass to perltidy")

;; other option:
;;
;; (defun perltidy-format ()
;;     "Run perltidy on the current region."
;;    (interactive)
;;    (save-excursion
;;      (shell-command-on-region (point) (mark) "/usr/bin/perltidy -q" nil t)))

(defun perltidy-format ()
  "Format Perl code with perltidy.
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
           "--quiet"
           "--standard-error-output"
           perl5-perltidy-options)
    (goto-char old-point))
  )

(defun perltidy-format-buffer ()
  "Format current buffer with perltidy."
  (interactive)
  (mark-whole-buffer)
  (perltidy-format)
  )

(defun perltidy-format-function ()
  "Format current function with perltidy."
  (interactive)
  (mark-defun)
  (perltidy-format)
  )

(use-package cperl-mode
  ;;     :demand t
  :init
  (defalias 'perl-mode 'cperl-mode) ;; change this alias if you want back to the native perl-mode
  (eval-after-load "ffap" '(require 'ffap-perl-module))
  :config
  (setq cperl-electric-keywords nil)
  (clear-abbrev-table cperl-mode-abbrev-table)
  )

;; OPTIONAL package : https://github.com/aki2o/emacs-plsense
;;
;; (defun plsense-go()
;;   "Start plsense server and load buffer into it."
;;   (interactive)
;;   (plsense-server-start)
;;   (plsense-setup-current-buffer)
;;   )

;; Tools for Regular Expressions : https://github.com/jwiegley/regex-tool

(use-package regex-tool
  :ensure t
  :config
  (setq regex-tool-backend "perl")
  )

(provide 'cfg-perl-regexps)
;;; cfg-perl-regexps.el ends here
