;;; perl-quote.el --- helpers for Perl quoted strings

;; Copyright 2008, 2009, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: languages, perl
;; URL: http://user42.tuxfamily.org/perl-quote/index.html
;; EmacsWiki: PerlLanguage

;; perl-quote.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; perl-quote.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This spot of code converts Perl single-quote '' strings to "", or back
;; again.  q{} and qq{} styles are supported, as are Locale::TextDomain
;; style __"" and N__"" translations.
;;
;; Move point to the start of the target string and use `perl-quote-single'
;; to make it a single '', or `perl-quote-double' to make it a double.  The
;; suggested key bindings are C-c ' and C-c " respectively, which
;; `perl-quote-keybindings' below can install.  See the docstrings for more.
;;
;; Both commands use `forward-sexp' to find the end of string, so if you're
;; not in perl-mode or cperl-mode then the mode will have to understand Perl
;; strings enough for `forward-sexp' to be right.  There's no real checking
;; of that, but for interactive use you can easily undo if it comes out
;; badly wrong.

;;; Install
;;
;; Put perl-quote.el in one of your `load-path' directories and in your
;; .emacs add
;;
;;     (autoload 'perl-quote-keybindings "perl-quote")
;;     (add-hook 'perl-mode-hook  'perl-quote-keybindings)
;;     (add-hook 'cperl-mode-hook 'perl-quote-keybindings)
;;
;; Or whichever perl-like modes you use, including maybe `pod-mode-hook' for
;; Perl code samples in POD files.
;;
;; There's autoload cookies for the functions and likely hook customize
;; options, if you know how to use `update-file-autoloads' and friends.

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - don't move point
;; Version 3 - new home page
;; Version 4 - allow double backslash \\
;; Version 5 - new email


;;; Code:

(defun perl-quote-backslashing (plains func)
  "An internal part of perl-quote.el to crunch backslashing.
Currently this is as follows:
PLAINS is a regexp char class string like \"[abc]\".
FUNC is called (FUNC str) for each occurrence of a PLAINS char or
of any backslash \"\\=\\X\", except for double backslash \"\\=\\\\=\\\".
Point is just after STR in the buffer."
  (goto-char (point-min))
  (let ((re (concat plains "\\|\\(\\\\+\\)")))
    (while (re-search-forward re nil t)
      (if (match-beginning 1)
          ;; backslashes
          (when (and (= 1 (logand 1 (- (match-end 1)  ;; an odd number
                                       (match-beginning 1))))
                     (> (point-max) (point)))
            (goto-char (1+ (point)))
            (funcall func (buffer-substring (- (point) 2) (point))))

        ;; plain
        (funcall func (match-string 0))))))

(defun perl-quote-single-backslashing (single-func)
  "An internal part of perl-quote.el to crunch backslashing.
SINGLE-FUNC is called (SINGLE-FUNC str rep) for STR each
occurrence of a $, @ or ' or a backslash \"\\=\\X\" except for
double backslash \"\\=\\\\=\\\".  Point is just after STR in the
buffer.

REP is a replacement string for putting STR into a single quote
string, or nil if there's no replacement.  There's no replacement
for interpolations $ or @, and currently none for some backslashing."

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
                                         ("\\e"  . "\e"))))))))

;;;###autoload
(defun perl-quote-single (&optional force)
  "Convert a Perl \"\" double-quoted string to '' single-quotes.
The forms converted are

    \"\"     ->  ''
    qq{}   ->  q{}
    qq[]   ->  q[]
    qq()   ->  q()
    qq<>   ->  q<>
    __\"\"   ->  __('')
    N__\"\"  ->  N__('')

Point must be at the start of the string, ie. the \" or q, or the
start of the __ or N__.

Parens are added for __ and N__ because __'' doesn't work, a
quote there is the old-style package name separator, it must be
__('').

Single-quotes in the string are escaped as \\=\\' for the new string
and the following backslash forms in the string are converted to
literal characters

    \\=\\\"   double-quote (no longer needs backslashing)
    \\=\\t   tab
    \\=\\n   newline
    \\=\\r   carriage-return
    \\=\\f   form-feed
    \\=\\b   backspace
    \\=\\a   bell (alert)
    \\=\\e   escape (0x1B)

Literal control characters in the source probably aren't a great
idea, but at least this gives a conversion.  Remember Emacs will
save a newline in the buffer as CRLF or CR under DOS or Mac
coding systems, which may not be what you want.

If there's other backslash forms or variable interpolations in
the string then the buffer is unchanged and an error is thrown
with point at the first offending part.  A \\[universal-argument] prefix forces
conversion.  Either way you'll have to edit to turn
interpolations etc into plain text or an expression.

The intention is to use `perl-quote-single' mainly on
double-quoted strings without variable interpolations, meaning
ones which don't need to be double-quotes and you'd prefer just
to have singles.

See `perl-quote-double' (\\[perl-quote-double]) for the converse
conversion to double."

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

          ;; closing " becomes '
          (delete-region (1- (point-max)) (point-max))
          (goto-char (point-max))
          (insert "'")

          ;; opening " becomes '
          (delete-region (point-min) (1+ (point-min)))
          (goto-char (point-min))
          (insert "'"))

        ;; without surrounding quotes
        (narrow-to-region (point) (1- (point-max)))

        (perl-quote-single-backslashing
         (lambda (part rep)
           (when rep
             (delete-region (- (point) (length part))
                            (point))
             (insert rep))))))

    ;; original point, but not a save-excursion since on error leave point
    ;; at the offending spot
    (goto-char orig-marker)))

;;;###autoload
(defun perl-quote-double ()
  "Convert a Perl '' single-quoted string to \"\" double-quotes.
The forms converted are

    ''   ->  \"\"
    q{}  ->  qq{}
    q[]  ->  qq[]
    q()  ->  qq()
    q<>  ->  qq<>

Point must be at the start of the string, ie. the ' or q.
Backslashing in the string is amended for the new double-quote
form,

    \\=\\' -> '   single-quote no longer needs backslashing
    \"  -> \\=\\\"  double-quote must be backslashed
    $  -> \\=\\$  dollar backslashed against interpolation
    @  -> \\=\\@  at backslashed against interpolation

Perl treats other backslashes in a single-quote string as just
the literal character, and `perl-quote-double' does the same,
removing the backslash (except for $, @ and \" to keep them
literal).

See `perl-quote-single' (\\[perl-quote-single]) for the converse
conversion to single."

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
              (insert "q") ;; q{} becomes qq{}

            (goto-char (1- (point-max)))
            (delete-region (1- (point-max)) (point-max))
            (insert "\"")

            (delete-region (point-min) (1+ (point-min)))
            (goto-char (point-min))
            (insert "\"")

            (narrow-to-region (point) (1- (point-max))))

          ;; "  becomes \"
          ;; \X becomes X, including \' becomes '
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
                      (insert rep)))))))))

;;;###autoload
(defun perl-quote-keybindings ()
  "Bind keys C-c ' and C-c \" to change string quote types.
This is designed for use from a major mode hook.  It makes the
following key bindings in the major mode keymap
\(`current-local-map')

    C-c '     `perl-quote-single'
    C-c \"     `perl-quote-double'

These are the suggested perl-quote.el keys, but of course you can
use anything you prefer.

The perl-quote home page is
URL `http://user42.tuxfamily.org/perl-quote/index.html'"

  (interactive)
  (define-key (current-local-map) [?\C-c ?']  'perl-quote-single)
  (define-key (current-local-map) [?\C-c ?\"] 'perl-quote-double))

;;;###autoload
(custom-add-option 'perl-mode-hook 'perl-quote-keybindings)
;;;###autoload
(custom-add-option 'cperl-mode-hook 'perl-quote-keybindings)
;;;###autoload
(custom-add-option 'pod-mode-hook 'perl-quote-keybindings)

;; LocalWords: qq docstrings el backslashing abc str FUNC ie Parens
;; LocalWords: backslashed TextDomain

(provide 'perl-quote)

;;; perl-quote.el ends here
