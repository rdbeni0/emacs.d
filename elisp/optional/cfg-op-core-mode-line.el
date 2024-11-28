;;; cfg-op-core-mode-line.el --- configure ediff, diff and vdiff -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mode-Line.html
;; "Each Emacs window (aside from minibuffer windows) typically has a mode line at the bottom, which displays status information about the buffer displayed in the window."
;;
;; A string is printed verbatim in the mode line except for %-constructs:
;;   %b -- print buffer name.      %f -- print visited file name.
;;   %F -- print frame name.
;;   %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;; 	%& is like %*, but ignore read-only-ness.
;; 	% means buffer is read-only and * means it is modified.
;; 	For a modified read-only buffer, %* gives % and %+ gives *.
;;   %s -- print process status.   %l -- print the current line number.
;;   %c -- print the current column number (this makes editing slower).
;;         Columns are numbered starting from the left margin, and the
;;         leftmost column is displayed as zero.
;;         To make the column number update correctly in all cases,
;; 	‘column-number-mode’ must be non-nil.
;;   %C -- Like %c, but the leftmost column is displayed as one.
;;   %i -- print the size of the buffer.
;;   %I -- like %i, but use k, M, G, etc., to abbreviate.
;;   %o -- print percent of window travel through buffer, or Top, Bot or All.
;;   %p -- print percent of buffer above top of window, or Top, Bot or All.
;;   %P -- print percent of buffer above bottom of window, perhaps plus Top,
;;         or print Bottom or All.
;;   %q -- print percent of buffer above both the top and the bottom of the
;;         window, separated by ‘-’, or ‘All’.
;;   %n -- print Narrow if appropriate.
;;   %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;;   %Z -- like %z, but including the end-of-line format.
;;   %e -- print error message about full memory.
;;   %@ -- print @ or hyphen.  @ means that default-directory is on a
;;         remote machine.
;;   %[ -- print one [ for each recursive editing level.  %] similar.
;;   %% -- print %.   %- -- print infinitely many dashes.
;; Decimal digits after the % specify field width to which to pad.
;;; Code:

;; To display the buffer size in a human-friendly format (e.g., in kilobytes or megabytes), you can use the size-indication-mode built-in functionality of Emacs, which provides this behavior.
(size-indication-mode 1)

(setq-default mode-line-format
	      '(
		;; " " ;; Adds a space at the beginning of the mode line
		"[%b] " ;; Display the buffer name
		;; " - "
		"["
		"l%l" ;; Display the current line number
		","
		"c%c" ;; Display the current column number
		"]"
		"[size:%I]" ;; Display the full file path
		"[%p%%]" ;; Display the percentage through the buffer
		"[modified:%*]" ;; Shows '*' if modified, '-' if not, and '%' if read-only
		"["mode-name"]" ;; Displays the major mode
		" [%f]" ;; Display the full path
		))

;; make mode-line smaller:

;; Smaller font for active mode line
;; (set-face-attribute 'mode-line nil :height 0.85)
;; ;; Smaller font for inactive mode line:
;; (set-face-attribute 'mode-line-inactive nil :height 0.85)

(provide 'cfg-op-core-mode-line)
;;; cfg-op-core-mode-line.el ends here
