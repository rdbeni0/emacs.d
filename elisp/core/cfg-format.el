;;; cfg-format.el --- configuration for code formatting  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; BUILT-IN FORMATTING via INDENTATION:
;; https://www.reddit.com/r/emacs/comments/q3rwes/anyone_using_code_formatter_for_elisp/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; formatting - built in function - the same as C-x h C-M-\:
(defun cfg/built-in-format-via-indent ()
  "Format code using built in processing and default indentation."
  (interactive)
  (indent-region (point-min) (point-max)))

;; do NOT use tabs for indentation:
;; https://web-mode.org/
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; XML (nxml) FORMATTING:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nxml-xmllint-executable "xmllint"
  "Location of xmllint executable.")

(defun cfg/xmllint-format-buffer ()
  "Run `xmllint --nonet --format -` on the current buffer."
  (interactive)
  (mark-whole-buffer)
  (save-excursion
    (shell-command-on-region (point) (mark) (concat nxml-xmllint-executable " --nonet --format -") nil t)))

;; formatting - built in function for nxml:
(defun cfg/built-in-format-nxml ()
  "Format xml code (nxml-mode) using built in processing."
  (interactive)
  (if (executable-find nxml-xmllint-executable)
      (cfg/xmllint-format-buffer)
    (cfg/built-in-format-via-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; PERL FORMATTING:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar perl5-perltidy-executable "perltidy"
  "Location of perltidy executable.")

(defvar perl5-perltidy-options '("--quiet"
				 "--standard-error-output"
				 "--perl-best-practices"
				 "-l=185")
  "Command line options to pass to perltidy")

(defun cfg/perltidy-format ()
  "Format Perl5 code with perltidy: f: fegion is active, operate on it, else operate on line."
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

;; formatting - built in function for cperl:
(defun cfg/built-in-format-perl ()
  "Format perl code using built in processing."
  (interactive)
  (if (executable-find perl5-perltidy-executable)
      (cfg/perltidy-format)
    (cfg/built-in-format-via-indent)))

;; other and alternative option:
;;
;; (defun cfg/perltidy-format ()
;;     "Run perltidy on the current region."
;;    (interactive)
;;    (save-excursion
;;      (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; GENERAL:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load general.el and keybindings:
(require 'cfg-gen-co-format)

(provide 'cfg-format)
;;; cfg-format.el ends here
