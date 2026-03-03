;;; esql-mode.el --- Major mode for esql files  -*- lexical-binding: t; -*-

;; Author: rdbeni0
;; Keywords: esql
;; Version: 0.0.4
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
    "CAST" "CATALOG" "CREATE" "FIELD" "DATABASE" "DECLARE" "DELETE" "DISTINCT"
    "DO" "ELSE" "ELSEIF" "END" "EVALUATE" "EXTERNAL" "FALSE" "FINALIZE"
    "FROM" "FOR" "FORMAT" "FUNCTION" "IF" "IN" "INOUT" "INPUT" "INSERT" "INTO" "ITEM" "LANGUAGE"
    "LIKE" "MODULE" "NOT" "NULL" "OR" "OUTPUT" "PASSTHROUGH" "PROPAGATE"
    "REPEAT" "RESIGNAL" "RETURN" "RETURNS" "ROW" "SELECT"
    "SET" "SIGNAL" "THEN" "THROW" "TO" "TRUE" "UPDATE" "VALUES" "WHEN"
    "WHILE" "BROKER" "SCHEMA" "PATH" "AFTER" "BEFORE" ))

(defconst esql--types
  '("BOOLEAN" "BYTE" "CHARACTER" "CCSID" "CHAR" "VARCHAR" "INTEGER" "INT"
    "DECIMAL" "FLOAT" "REAL" "DOUBLE" "TIMESTAMP" "DATE" "TIME"
    "INTERVAL" "BLOB" "BIT" "ROW" "REFERENCE" "LIST" "SHARED" "NAMESPACE" "GMTTIME" "GMTTIMESTAMP"))

(defconst esql--constants
  '("TRUE" "FALSE" "NULL" "UNKNOWN"))

(defconst esql--special-vars
  '("InputRoot" "OutputRoot" "LocalEnvironment" "InputLocalEnvironment" "OutputLocalEnvironment" "Environment" "InputExceptionList" "OutputExceptionList"
    "ExceptionList" "ExceptionData" "Message" "Tree" "Properties" "DFDL" "XMLNSC"))

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
           ;; Pusta linia lub sama linia komentarza
           (string-match-p "^[ \t]*\\(--.*\\)?$" s))

         ;; BEGIN / THEN / DO / TRY / CATCH / EXCEPTION / CASE
         (opens-block-p (s)
           (string-match-p
            "\\_<\\(BEGIN\\|THEN\\|DO\\|TRY\\|CATCH\\|EXCEPTION\\|CASE\\)\\_>"
            s))

         ;; Każde END ... traktujemy jako zamknięcie bloku
         (closes-block-p (s)
           (string-match-p "^[ \t]*END\\b" s))

         ;; ELSE / ELSEIF / WHEN / OTHERWISE
         (middle-block-p (s)
           (string-match-p "^[ \t]*\\(ELSE\\|ELSEIF\\|WHEN\\|OTHERWISE\\)\\b" s)))

      ;; Znajdź poprzednią istotną linię jako kotwicę
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

               ;; Tylko MODULE jest blokiem CREATE
               ((string-match-p "^[ \t]*CREATE\\s-+COMPUTE\\s-+MODULE\\b" l)
                (setq indent (+ (current-indentation) tab)))

               ;; Normalna linia – przejmujemy jej wcięcie
               (t
                (setq indent (current-indentation))))))))

      ;; Jeśli nic nie znaleźliśmy – poziom 0
      (unless indent
        (setq indent 0))

      ;; Bieżąca linia zamyka blok lub jest ELSE/WHEN – cofamy poziom
      (when (or (closes-block-p text)
                (middle-block-p text))
        (setq indent (max 0 (- indent tab))))

      ;; Ustaw wcięcie
      (goto-char bol)
      (unless (looking-at "^[ \t]*$")
        (let ((cur (current-indentation)))
          (unless (= cur indent)
            (delete-horizontal-space)
            (indent-to indent))))

      ;; Przywróć pozycję kursora
      (goto-char savep))))

(defun esql-mode-setup-indent ()
  (setq indent-line-function 'esql--indent-line) ;; TODO! Not working correctly
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
