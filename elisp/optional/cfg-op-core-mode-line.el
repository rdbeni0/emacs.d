;;; cfg-op-core-mode-line.el --- configure mode-line -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mode-Line.html
;; "Each Emacs window (aside from minibuffer windows) typically has a mode line at the bottom,
;; which displays status information about the buffer displayed in the window."
;;
;; A string is printed verbatim in the mode line except for %-constructs:
;;
;;   %b -- print buffer name.
;;   %f -- print visited file name.
;;   %F -- print frame name.
;;   %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;; 	   %& is like %*, but ignore read-only-ness.
;;         % means buffer is read-only and * means it is modified.
;; 	   For a modified read-only buffer, %* gives % and %+ gives *.
;;   %s -- print process status.
;;   %l -- print the current line number.
;;   %c -- print the current column number (this makes editing slower).
;;         Columns are numbered starting from the left margin, and the
;;         leftmost column is displayed as zero.
;;         To make the column number update correctly in all cases,
;; 	   `column-number-mode' must be non-nil.
;;   %C -- Like %c, but the leftmost column is displayed as one.
;;   %i -- print the size of the buffer.
;;   %I -- like %i, but use k, M, G, etc., to abbreviate.
;;   %o -- print percent of window travel through buffer, or Top, Bot or All.
;;   %p -- print percent of buffer above top of window, or Top, Bot or All.
;;   %P -- print percent of buffer above bottom of window, perhaps plus Top,
;;         or print Bottom or All.
;;   %q -- print percent of buffer above both the top and the bottom of the
;;         window, separated by `-', or `All'.
;;   %n -- print Narrow if appropriate.
;;   %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;;   %Z -- like %z, but including the end-of-line format.
;;   %e -- print error message about full memory.
;;   %@ -- print @ or hyphen.
;;         @ means that default-directory is on a remote machine.
;;   %[ -- print one [ for each recursive editing level.  %] similar.
;;   %% -- print %.
;;   %- -- print infinitely many dashes.
;;         Decimal digits after the % specify field width to which to pad.
;;
;;
;; MINIMALIST EXAMPLE:
;;
;; (setq-default mode-line-format
;; 	      '(
;; 		" " ;; Adds a space at the beginning of the mode line
;; 		"[%b] " ;; Display the buffer name
;; 		" - "
;; 		"["
;; 		"l%l" ;; Display the current line number
;; 		","
;; 		"c%c" ;; Display the current column number
;; 		"]"
;; 		"[size:%I]"  ;; Size in human-friendly format
;; 		"[%p%%]" ;; Display the percentage through the buffer
;; 		"[modified:%*]" ;; Shows `*' if modified, `-' if not, and `%' if read-only
;; 		"["mode-name"]" ;; Displays the major mode
;; 		" [%f]" ;; Display the full path
;; 		))
;;
;;; Code:

;; To display the buffer size in a human-friendly format (e.g., in kilobytes or megabytes),
;; you can use the size-indication-mode built-in functionality of Emacs, which provides this behavior:
(size-indication-mode 1)

;; (setq column-number-mode t)

;; This information is useless for most:
(setopt display-time-default-load-average nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a custom face for displaying buffer path in the mode-line
(defface cfg/mode-line-path-face
  ;; '((t :foreground "LightSkyBlue" :weight bold))
  '((t :foreground "cyan" :weight bold))
  "Face used to highlight buffer path or name in the mode-line.")

;; Define a custom face for mode-name in the mode-line
(defface cfg/mode-line-mode-name-face
  '((t :foreground "white" :weight bold))
  "Face used to highlight the major mode name in the mode-line.")

(defun cfg/-mode-line-buffer-or-path ()
  "Return buffer file path truncated to 55% of window width, or buffer name if no file.
Expands $HOME to ~ for readability, and applies a custom face for styling."
  (let* ((win-width (window-total-width))          ;; total width of the current window
         (max-len (floor (* win-width 0.55)))      ;; maximum allowed length (55% of current window width)
         (fname (buffer-file-name))                ;; full file path, or nil if buffer is not visiting a file
         (text                                    ;; string to display in the mode-line
          (cond
           ;; if no file is associated, show buffer name
           ((not fname)
            (buffer-name))
           ;; if file path length fits within the limit, show full path (with ~)
           ((<= (length fname) max-len)
            (abbreviate-file-name fname))
           ;; otherwise, show only the file name (basename)
           (t
            (file-name-nondirectory fname)))))
    ;; Apply the custom face to the resulting string
    (propertize text 'face 'cfg/mode-line-path-face)))

(defun cfg/-mode-line-encoding ()
  "Return a short string for buffer encoding (e.g. U8 for UTF-8)."
  (let* ((cs (or buffer-file-coding-system 'undecided))
         (base (symbol-name (coding-system-base cs))))
    (cond
     ;; global
     ((string-match "utf-8" base) "U8")
     ((string-match "utf-16" base) "U16")
     ;; ASCII family
     ((string-match "us-ascii" base) "ASC")
     ((string-match "ansi_x3.4-1968" base) "ASC")
     ((string-match "cp437" base) "CP437")
     ;; Western Europe
     ((string-match "iso-8859-1" base) "ISO-1")
     ((string-match "windows-1252" base) "CP1252")
     ((string-match "iso-8859-15" base) "ISO-15")
     ;; Central Europe
     ((string-match "iso-8859-2" base) "ISO-2")
     ((string-match "windows-1250" base) "CP1250")
     ;; fallback
     (t base))))

(defun cfg/-mode-line-eol ()
  "Return a short string for end-of-line style (Unix, DOS, Mac)."
  (pcase (coding-system-eol-type buffer-file-coding-system)
    (0 "LF")   ;; LF -> Unix
    (1 "CRLF")    ;; CRLF -> DOS
    (2 "CR")    ;; CR -> Mac
    (_ "?")))

(defun cfg/-mode-line-mode-name ()
  "Return the current major mode name, styled with a custom face."
  (propertize mode-name 'face 'cfg/mode-line-mode-name-face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck

(when (and (require 'flycheck nil :noerror)
           (featurep 'flycheck))

  ;; faces
  (defface cfg/flycheck-ok-face
    '((t (:foreground "green" :weight bold)))
    "Face for Flycheck OK.")

  (defface cfg/flycheck-error-face
    '((t (:foreground "deep sky blue" :weight bold)))
    "Face for Flycheck errors.")

  (defface cfg/flycheck-warning-face
    ;; '((t (:foreground "gray" :weight bold)))
    '((t (:foreground "wheat" :weight bold)))
    "Face for Flycheck warnings.")

  (defface cfg/flycheck-info-face
    ;; '((t (:foreground "#8B4513" :weight bold)))
    '((t (:foreground "magenta" :weight bold)))
    "Face for Flycheck info.")

  ;; vars
  (defvar cfg/flycheck-last-result " "
    "Cached Flycheck indicator for mode-line.")

  ;; Cache update function
  (defun cfg/-flycheck-update-cache (&rest _)
    "Update cached Flycheck indicator after Flycheck status changes."
    (setq cfg/flycheck-last-result
          (pcase flycheck-last-status-change
            ('finished
             (let* ((counts (flycheck-count-errors flycheck-current-errors))
                    (e (or (cdr (assq 'error   counts)) 0))
                    (w (or (cdr (assq 'warning counts)) 0))
                    (i (or (cdr (assq 'info    counts)) 0)))
               (cond
                ((> e 0) (propertize (format "E%d" e) 'face 'cfg/flycheck-error-face))
                ((> w 0) (propertize (format "W%d" w) 'face 'cfg/flycheck-warning-face))
                ((> i 0) (propertize (format "I%d" i) 'face 'cfg/flycheck-info-face))
                (t       (propertize "OK"     'face 'cfg/flycheck-ok-face)))))
            ('running     (propertize "LIVE" 'face 'cfg/flycheck-info-face))
            ('no-checker  (propertize "NoChecker"   'face 'cfg/flycheck-warning-face))
            ('errored     (propertize "ERR"  'face 'cfg/flycheck-error-face))
            ('interrupted (propertize "STOP"  'face 'cfg/flycheck-warning-face))
            ('suspicious  (propertize "???"   'face 'cfg/flycheck-warning-face))
            (_            (propertize "---"    'face 'cfg/flycheck-info-face)))))

  ;; Hooks -> async updates:
  (add-hook 'flycheck-status-changed-functions #'cfg/-flycheck-update-cache)
  (add-hook 'flycheck-after-syntax-check-hook  #'cfg/-flycheck-update-cache))

;;; mode-line function
(defun cfg/-mode-line-flycheck ()
  "Return cached Flycheck status for mode-line."
  (if (and (bound-and-true-p flycheck-mode)
           (featurep 'flycheck))
      cfg/flycheck-last-result
    "no"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default mode-line-format
              '(
                ;; "[" (:eval (cfg/-mode-line-buffer-or-path)) "]   "
                "[FlyC:" (:eval (cfg/-mode-line-flycheck)) "]"
                "[l%l,c%c]"  ;; Display the current column + line number
                "[mod:%*]" ;; Shows `*' if modified, `-' if not, and `%' if read-only
                ;; "[enc:" (:eval (cfg/-mode-line-encoding)) "]"
                "[" (:eval (cfg/-mode-line-encoding)) "]"
                ;; "[eol:" (:eval (cfg/-mode-line-eol)) "]"
                "[" (:eval (cfg/-mode-line-eol)) "]"
                ;; "[" mode-name "]" ;; Displays the major mode
                "[" (:eval (cfg/-mode-line-mode-name)) "]" ;; major mode with custom face
                "[size:%I]" ;; Size in human-friendly format
                "[%p]" ;; Display the percentage through the buffer
                " [" (:eval (cfg/-mode-line-buffer-or-path)) "]"
                " " (:eval (anzu--update-mode-line)) "" ;; Move anzu counter until the very end
                ))

;; https://github.com/emacsorphanage/evil-anzu
;; https://github.com/emacsorphanage/anzu
(setq anzu-cons-mode-line-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make mode-line smaller:

;;;; Smaller font for active mode line
;; (set-face-attribute 'mode-line nil :height 0.90)
;;;; Smaller font for inactive mode line:
;; (set-face-attribute 'mode-line-inactive nil :height 0.90)

(provide 'cfg-op-core-mode-line)
;;; cfg-op-core-mode-line.el ends here
