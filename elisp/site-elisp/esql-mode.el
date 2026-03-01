;;; esql-mode.el --- Major mode for esql files  -*- lexical-binding: t; -*-

;; Author: rdbeni0
;; Keywords: esql
;; Version: 0.0.3
;; Package-Requires: ((emacs "28.5"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.markdown/licenses/>.

(require 'prog-mode)

(defgroup esql nil
  "Major mode for editing esql files."
  :group 'languages)

(defvar esql-mode-hook nil
  "Hook run when entering `esql-mode'.")

(defvar esql-mode-map
  (let ((map (make-sparse-keymap)))
    ;; You can add keyboard shortcuts here:
    map)
  "Keymap for `esql-mode'.")

;; =============================
;; Syntax table
;; =============================
(defvar esql-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; -- comments
    (modify-syntax-entry ?-  ". 12b" st)
    (modify-syntax-entry ?\n "> b"   st)
    ;; /* */ comments
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23"   st)
    ;; 'strings'
    (modify-syntax-entry ?'  "\""     st)
    ;; _ as part of a word
    (modify-syntax-entry ?_  "w"      st)
    ;; . in field names (InputRoot.Message.A etc)
    (modify-syntax-entry ?.  "_"      st)
    st)
  "Syntax table for `esql-mode'.")

;; =============================
;; Keywords / types / constants
;; =============================
(defconst esql--keywords
  '("ALL" "AND" "ANY" "AS" "ATTACH" "BEGIN" "BETWEEN" "BY" "CALL" "CASE"
    "CAST" "CATALOG" "CREATE" "DATABASE" "DECLARE" "DELETE" "DISTINCT"
    "DO" "ELSE" "ELSEIF" "END" "EVALUATE" "EXTERNAL" "FALSE" "FINALIZE"
    "FROM" "FOR" "FUNCTION" "IF" "IN" "INPUT" "INSERT" "INTO" "ITEM" "LANGUAGE"
    "LIKE" "MODULE" "NOT" "NULL" "OR" "OUTPUT" "PASSTHROUGH" "PROPAGATE"
    "REFERENCE" "REPEAT" "RESIGNAL" "RETURN" "RETURNS" "ROW" "SELECT"
    "SET" "SIGNAL" "THEN" "THROW" "TO" "TRUE" "UPDATE" "VALUES" "WHEN"
    "WHILE"))

(defconst esql--types
  '("BOOLEAN" "BYTE" "CHARACTER" "CHAR" "VARCHAR" "INTEGER" "INT"
    "DECIMAL" "FLOAT" "REAL" "DOUBLE" "TIMESTAMP" "DATE" "TIME"
    "INTERVAL" "BLOB" "BIT" "ROW" "REFERENCE" "LIST" "SHARED"))

(defconst esql--constants
  '("TRUE" "FALSE" "NULL" "UNKNOWN"))

(defconst esql--special-vars
  '("InputRoot" "OutputRoot" "LocalEnvironment" "Environment"
    "ExceptionList" "ExceptionData" "Message" "Tree" "Properties"))

(defconst esql-font-lock-keywords
  `(
    (,(regexp-opt esql--keywords 'words)
     . font-lock-keyword-face)

    (,(regexp-opt esql--types 'words)
     . font-lock-type-face)

    (,(regexp-opt esql--constants 'words)
     . font-lock-constant-face)

    (,(regexp-opt esql--special-vars 'words)
     . font-lock-variable-name-face)

    ;; CREATE FUNCTION / PROCEDURE
    ("\\<\\(CREATE\\)\\s-+\\(FUNCTION\\|PROCEDURE\\)\\s-+\\([[:alnum:]_]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face)
     (3 font-lock-function-name-face))

    ;; CREATE COMPUTE MODULE
    ("\\<\\(CREATE\\)\\s-+\\(COMPUTE\\)\\s-+\\(MODULE\\)\\s-+\\([[:alnum:]_\\.]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face)
     (3 font-lock-keyword-face)
     (4 font-lock-function-name-face))
    ))

;; =============================
;; Indentation
;; =============================

(defun esql--indent-line ()
  "Indent current line according to ESQL block structure."
  (let* ((save-excursion-point (point))
         (this-line-start (progn (beginning-of-line) (point)))
         (this-line-text (buffer-substring-no-properties
                          this-line-start (line-end-position)))
         (indent 0)
         found-anchor)
    (save-excursion
      ;; We go up looking for checkpoints
      (while (and (not found-anchor) (not (bobp)))
        (forward-line -1)
        (beginning-of-line)
        (let ((line (buffer-substring-no-properties
                     (point) (line-end-position))))
          (cond
           ;; Comment or empty line -> omitted
           ((string-match-p "^[ \t]*\\(?:--.*\\)?$" line)
            nil)
           ;; END ... (END, END IF, END WHILE, END CASE, END FOR, END TRY etc.)
           ((string-match-p
             "^[ \t]*END\\(?:[ \t]+\\(?:IF\\|WHILE\\|FOR\\|CASE\\|TRY\\|CATCH\\|FUNCTION\\|PROCEDURE\\|COMPUTE\\|MODULE\\)\\)?\\(?:[ \t]*;.*\\)?$"
             line)
            (setq indent (current-indentation))
            (setq found-anchor t))
           ;; BEGIN / THEN / ELSE / DO / TRY / CATCH / WHEN ... THEN / EXCEPTION
           ((string-match-p
             "\\(?:^\\|[^[:alnum:]_]\\)\\(BEGIN\\|THEN\\|DO\\|TRY\\|CATCH\\|EXCEPTION\\)\\(?:[ \t]*\\|$\\|--\\)"
             line)
            (setq indent (+ (current-indentation) tab-width))
            (setq found-anchor t))
           ;; ELSE / ELSEIF (at the same level as IF/CASE)
           ((string-match-p "^[ \t]*\\(ELSE\\|ELSEIF\\)\\b" line)
            (setq indent (+ (current-indentation) tab-width))
            (setq found-anchor t))
           ;; CASE / WHEN / OTHERWISE (WHEN and OTHERWISE are at CASE+tab level)
           ((string-match-p "\\<\\(CASE\\)\\b" line)
            (setq indent (+ (current-indentation) tab-width))
            (setq found-anchor t))
           ;; CREATE COMPUTE MODULE / FUNCTION / PROCEDURE
           ((string-match-p "^[ \t]*CREATE\\s-+\\(COMPUTE\\s-+\\(?:MODULE\\|FUNCTION\\)\\|FUNCTION\\|PROCEDURE\\)\\b" line)
            (setq indent (+ (current-indentation) tab-width))
            (setq found-anchor t))
           ;; WHEN / OTHERWISE after CASE (indent + tab)
           ((and (string-match-p "\\<\\(WHEN\\|OTHERWISE\\)\\b" line)
                 (save-excursion
                   (while (and (not (looking-at-p "\\<CASE\\b"))
                               (not (bobp)))
                     (forward-line -1))
                   (looking-at-p "\\<CASE\\b")))
            (setq indent (+ (current-indentation) tab-width))
            (setq found-anchor t))
           ;; Default case: we take the indentation of the current line
           (t
            (setq indent (current-indentation))
            (setq found-anchor t))))))
    ;; Now we modify the indentation of the current line
    (when (string-match-p
           "^[ \t]*\\(END\\|ELSE\\|ELSEIF\\|WHEN\\|OTHERWISE\\|BEGIN\\)\\b"
           this-line-text)
      (setq indent (max 0 (- indent tab-width))))
    ;; We never go below zero
    (setq indent (max 0 indent))
    ;; Proper indentation
    (goto-char this-line-start)
    (when (not (looking-at-p "^[ \t]*$"))
      (let ((current (current-indentation)))
        (if (/= indent current)
            (progn
              (delete-region (point) (+ (point) current))
              (indent-to indent)))))
    ;; We restore the cursor position
    (goto-char save-excursion-point)))

;; Add to mode
(defun esql-mode-setup-indent ()
  (setq indent-line-function 'esql--indent-line)
  (setq tab-width 2)               ; <- 2 spaces
  (setq indent-tabs-mode nil))     ; <- spaces instead of tabs

(add-hook 'esql-mode-hook #'esql-mode-setup-indent)

;; =============================
;; Mode definition
;; =============================

;;;###autoload
(define-derived-mode esql-mode prog-mode "ESQL"
  "Major mode for editing esql files."
  :syntax-table esql-mode-syntax-table
  :group 'esql

  (setq-local font-lock-defaults
              '(esql-font-lock-keywords
                nil        ;; keywords-only
                t          ;; case-fold (IMPORTANT)
                nil
                nil))
  (setq-local indent-line-function #'esql--indent-line)

  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "--+\\s-*")
  (setq-local comment-column 40)

  ;; Helps with assembling blocks BEGIN...END
  (setq-local outline-regexp "^[ \t]*\\(CREATE\\|BEGIN\\|END\\)\\_>")

  ;; Imenu
  (setq-local imenu-generic-expression
              '(("Functions/Procedures" "^\\s-*CREATE\\s-+\\(FUNCTION\\|PROCEDURE\\)\\s-+\\([[:alnum:]_]+\\)" 2)
                ("Modules" "^\\s-*CREATE\\s-+COMPUTE\\s-+MODULE\\s-+\\([[:alnum:]_\\.]+\\)" 1)))

  (run-hooks 'esql-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.esql\\'" . esql-mode))

(provide 'esql-mode)
;;; esql-mode.el ends here
