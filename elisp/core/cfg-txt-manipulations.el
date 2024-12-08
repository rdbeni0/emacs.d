;;; cfg-txt-manipulations.el --- various text manipulations -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://github.com/bbatsov/crux/blob/master/crux.el
;;
;;; Code:

(require 'cfg-gen-co-txt-manipulations)

(defun cfg/capitalize-region (beg end)
  "`capitalize-region' when `transient-mark-mode' is on and region is active."
  (interactive "*r")
  (when (use-region-p)
    (capitalize-region beg end)))

(defun cfg/downcase-region (beg end)
  "`downcase-region' when `transient-mark-mode' is on and region is active."
  (interactive "*r")
  (when (use-region-p)
    (downcase-region beg end)))

;; https://stackoverflow.com/questions/18257573/how-to-toggle-letter-cases-in-a-region-in-emacs

(defun cfg/toggle-case ()
  (interactive)
  (when (region-active-p)
    (let ((i 0)
	  (return-string "")
	  (input (buffer-substring-no-properties (region-beginning) (region-end))))
      (while (< i (- (region-end) (region-beginning)))
	(let ((current-char (substring input i (+ i 1))))
	  (if (string= (substring input i (+ i 1)) (downcase (substring input i (+ i 1))))
              (setq return-string
		    (concat return-string (upcase (substring input i (+ i 1)))))
            (setq return-string
		  (concat return-string (downcase (substring input i (+ i 1)))))))
	(setq i (+ i 1)))
      (delete-region (region-beginning) (region-end))
      (insert return-string))))

(defun cfg/toggle-case-active ()
  "Toggle the letter case of current word or text selection (active region).
Toggles between: 'all lower', 'Init Caps', 'ALL CAPS'."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

(defun cfg/join-lines-in-region (start end)
  "Join all lines in the selected region into one line, handling both Unix and Windows EOLs."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\r?\n+" end t)
      (replace-match ""))))

(defun cfg/join-lines-in-region-add-spc (start end)
  "Join all lines in the selected region into one line, handling both Unix and Windows EOLs.
  Add space instead of EOL."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\r?\n+" end t)
      (replace-match " "))))

;; https://www.emacswiki.org/emacs/DosToUnix
(defun cfg/dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

(defun cfg/unix2dos (buffer)
  "Automate replacing C-q C-j (\\n) with C-q C-m C-j (\\r\\n)."
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-j) nil t)
      (replace-match (string ?\C-m ?\C-j) nil t))))

(defun cfg/-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun cfg/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (cfg/-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun cfg/duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (cfg/-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (dotimes (_ arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(provide 'cfg-txt-manipulations)
;;; cfg-txt-manipulations.el ends here
