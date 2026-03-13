;;; esql-mode.el --- Major mode for esql files  -*- lexical-binding: t; -*-

;; Author: rdbeni0
;; Keywords: esql
;; Version: 0.0.5
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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

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
;; Reserved keywords
;; =============================
(defconst esql--reserved-keywords
  '("BEGIN" "IF" "ALL" "ASYMMETRIC" "BOTH" "CASE" "DISTINCT" "ELSE" "ELSEIF" "END" "FROM" "ITEM" "LEADING" "NOT" "SYMMETRIC" "THEN" "TRAILING" "WHEN" "FOR" "DO" "OR" "AND" "WHERE" "WHILE" "UNTIL"))

;; =============================
;; Non-reserved keywords
;; =============================
(defconst esql--non-reserved-keywords
  '("ANY" "AS" "ATOMIC" "ATTACH" "BETWEEN" "BIT"  "BY" "IN" "IS" "TO"
    "CALL" "CATALOG"   "COMPUTE" "CONDITION"
    "CONTINUE" "COORDINATED" "COUNT" "CREATE" "CURRENT_DATE" "CURRENT_GMTDATE"
    "CURRENT_GMTTIME" "CURRENT_GMTTIMESTAMP" "CURRENT_TIME" "CURRENT_TIMESTAMP"
    "DATA" "DATABASE" "DAY" "DAYOFWEEK" "DAYOFYEAR" "DAYS"
    "DECLARE" "DEFAULT" "DELETE" "DETACH"  "DOMAIN" "DYNAMIC"
    "ENCODING" "ENVIRONMENT" "ESCAPE" "ESQL" "EVAL" "EVENT" "EXCEPTION"
    "EXISTS" "EXIT" "EXTERNAL" "FALSE" "FIELD" "FILTER" "FINALIZE" "FIRSTCHILD"
    "FORMAT" "FOUND" "FULL" "FUNCTION" "GMTTIME" "GMTTIMESTAMP"
    "GROUP" "HANDLER" "HAVING" "HOUR" "IDENTITY" "INF" "INFINITY"
    "INOUT" "INSERT"  "INTERVAL" "INTO" "ISLEAPYEAR" "ITERATE"
    "JAVA" "LABEL" "LANGUAGE" "LAST" "LASTCHILD" "LEAVE" "LIKE" "LIST"
    "LOCALTIMEZONE" "LOG" "LOOP" "MAX" "MESSAGE" "MIN" "MINUTE" "MODIFIES"
    "MODULE" "MONTH" "MONTHS" "MOVE" "NAME" "NAMESPACE" "NAN" "NEXTSIBLING"
    "NONE"  "NUM" "NUMBER" "OF" "OPTIONS" "ORDER" "OUT" "PARSE"
    "PASSTHRU" "PATH" "PLACING" "PREVIOUSSIBLING" "PROCEDURE" "PROPAGATE"
    "QUARTEROFYEAR" "QUARTERS" "READS"  "REPEAT" "RESIGNAL" "RESULT"
    "RETURN" "RETURNS" "ROW" "SAMEFIELD" "SCHEMA" "SECOND" "SELECT" "SET" "SETS"
    "SEVERITY" "SHARED" "SHORT" "SOME" "SQL" "SQLCODE" "SQLERRORTEXT"
    "SQLEXCEPTION" "SQLNATIVEERROR" "SQLSTATE" "SQLWARNING" "SUM" "TERMINAL"
    "THE"  "THROW" "TIME" "TIMESTAMP" "TRACE" "TRUE" "TYPE"
    "UNCOORDINATED" "UNKNOWN"  "UPDATE" "USER" "UUIDASBLOB" "UUIDASCHAR"
    "VALUE" "VALUES" "WEEKOFMONTH" "WEEKOFYEAR" "WEEKS"  "YEAR" "LASTMOVE"))

;; =============================
;; Types / constants / special vars
;; =============================
(defconst esql--types
  '("BOOLEAN" "BYTE" "CHARACTER" "CCSID" "CHAR" "VARCHAR" "INTEGER" "INT"
    "DECIMAL" "FLOAT" "REAL" "DOUBLE" "TIMESTAMP" "DATE" "TIME"
    "INTERVAL" "BLOB" "BIT" "ROW" "REFERENCE" "LIST" "SHARED" "NAMESPACE"
    "GMTTIME" "GMTTIMESTAMP" "CHARACTER" "DECIMAL" "INT" "INTEGER" "REFERENCE" "BLOB" "BOOLEAN" "NULL" "CCSID" "CHAR" "FLOAT" "CONSTANT" "DATE"))

(defconst esql--constants
  '("TRUE" "FALSE" "NULL" "UNKNOWN"))

(defconst esql--special-vars
  '("InputRoot" "OutputRoot" "LocalEnvironment" "InputLocalEnvironment"
    "OutputLocalEnvironment" "Environment" "InputExceptionList"
    "OutputExceptionList" "ExceptionList" "ExceptionData" "Message" "MQMD"
    "Tree" "Variables" "Destination" "Properties" "DFDL" "XMLNSC" "JSON"))

;; =============================
;; Font-lock keywords
;; =============================

;; M-x list-faces-display
(defconst esql-font-lock-keywords
  (list
   ;; 1. Reserved keywords
   `(,(regexp-opt esql--reserved-keywords 'words)
     . font-lock-builtin-face)

   ;; 2. Non-reserved keywords
   `(,(regexp-opt esql--non-reserved-keywords 'words)
     . font-lock-keyword-face)

   ;; 3. Types
   `(,(regexp-opt esql--types 'words)
     . font-lock-type-face)

   ;; 4. Constants
   `(,(regexp-opt esql--constants 'words)
     . font-lock-constant-face)

   ;; 5. Special variables
   `(,(regexp-opt esql--special-vars 'words)
     . font-lock-variable-name-face)

   ;; CREATE FUNCTION / PROCEDURE
   '("\\<\\(CREATE\\)\\s-+\\(FUNCTION\\|PROCEDURE\\)\\s-+\\([[:alnum:]_]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face)
     (3 font-lock-function-name-face))

   ;; CREATE COMPUTE MODULE
   '("\\<\\(CREATE\\)\\s-+\\(COMPUTE\\)\\s-+\\(MODULE\\)\\s-+\\([[:alnum:]_\\.]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face)
     (3 font-lock-keyword-face)
     (4 font-lock-function-name-face))))

;; =============================
;; Indentation
;; =============================

(require 'cl-lib)

(defun esql--indent-line ()
  "Indent current line according to ESQL block structure."
  (interactive)
  (let* ((savep (point))
         (bol (progn (beginning-of-line) (point)))
         (text (buffer-substring-no-properties bol (line-end-position)))
         (tab tab-width)
         indent)

    (cl-labels
        ((skip-line-p (s)
           ;; Blank line or just a comment line
           (string-match-p "^[ \t]*\\(--.*\\)?$" s))

         ;; BEGIN / THEN / DO / TRY / CATCH / CASE
         (opens-block-p (s)
           (string-match-p
            "\\_<\\(BEGIN\\|THEN\\|DO\\|TRY\\|CATCH\\|CASE\\)\\_>"
            s))

         ;; We treat each END ... as the closure of the block
         (closes-block-p (s)
           (string-match-p "^[ \t]*END\\b" s))

         ;; ELSE / ELSEIF / WHEN / OTHERWISE
         (middle-block-p (s)
           (string-match-p "^[ \t]*\\(ELSE\\|ELSEIF\\|WHEN\\|OTHERWISE\\)\\b" s)))

      ;; Find the previous significant line as an anchor
      (save-excursion
        (setq indent nil)
        (while (and (not (bobp)) (null indent))
          (forward-line -1)
          (let ((l (string-trim-right
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))))
            (unless (skip-line-p l)
              (cond
               ;; END ...
               ((closes-block-p l)
                (setq indent (current-indentation)))

               ;; ELSE / WHEN / OTHERWISE
               ((middle-block-p l)
                (setq indent (+ (current-indentation) tab)))

               ;; BEGIN / THEN / DO / TRY / CATCH / EXCEPTION / CASE
               ((opens-block-p l)
                (setq indent (+ (current-indentation) tab)))

               ;; Only MODULE is a CREATE block
               ((string-match-p "^[ \t]*CREATE\\s-+COMPUTE\\s-+MODULE\\b" l)
                (setq indent (+ (current-indentation) tab)))

               ;; Normal line – we take over its indentation
               (t
                (setq indent (current-indentation))))))))

      ;; If we didn't find anything – level 0
      (unless indent
        (setq indent 0))

      ;; The current line closes the block or is ELSE/WHEN – we go back a level
      (when (or (closes-block-p text)
                (middle-block-p text))
        (setq indent (max 0 (- indent tab))))

      ;; Set the indent
      (goto-char bol)
      (unless (looking-at "^[ \t]*$")
        (let ((cur (current-indentation)))
          (unless (= cur indent)
            (delete-horizontal-space)
            (indent-to indent))))

      ;; Restore cursor position
      (goto-char savep))))

(defun esql-mode-setup-indent ()
  (setq indent-line-function 'esql--indent-line)
  (setq tab-width 4)
  (setq indent-tabs-mode t))

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
                nil        ;; t = case-fold (IMPORTANT for esql)
                nil
                nil))
  (setq-local indent-line-function #'esql--indent-line)

  ;; The search should have a case-sensitive context:
  (setq-local case-fold-search nil)

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

  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.esql\\'" . esql-mode))

(provide 'esql-mode)
;;; esql-mode.el ends here
