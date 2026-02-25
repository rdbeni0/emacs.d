;;; init_core.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NATIVE COMPILATION AND PERFORMANCE OPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic setup for performance tweaks and options.
;; And also native compilation (Emacs ver >= 28.5)
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Native-Compilation.html
;; https://news.ycombinator.com/item?id=24117853
;;

;; Optimization level for native compilation, a number between -1 and 3.
;; -1 functions are kept in bytecode form and no native compilation is performed.
;;  0 native compilation is performed with no optimizations.
;;  1 light optimizations.
;;  2 max optimization level fully adherent to the language semantic.
;;  3 max optimization level, to be used only when necessary.
;;    Warning: with 3, the compiler is free to perform dangerous optimizations.

(setq comp-speed 2)
(setq native-comp-speed 2)

;; additional options:

(setq native-comp-deferred-compilation t) ; should be t, and default is t
(setq native-comp-async-query-on-exit t) ; no risky

;; aggressive performance experiment:
;; default is 0, but this value could be risky (the best option is default value)
;; (setq native-comp-async-jobs-number 4) ; very risky, but possibility of better performance during startup

;; no warnings when compilation is ongoing:
(setq native-comp-async-report-warnings-errors nil)
(setq comp-async-report-warnings-errors nil)

;; kill unnecessary compilation buffer:
;; please check cfg- file with tempbuf configuration or try to experiment with (kill-buffer ) ...

;; manipulations with font rendering:
;; https://www.reddit.com/r/emacs/comments/14c4l8j/way_to_make_emacs_feel_smoother/
(setq jit-lock-stealth-time 1.25)
(setq jit-lock-stealth-nice 0.5) ;; Seconds between font locking.
(setq jit-lock-chunk-size 4096)
(setq jit-lock-defer-time 0)

;; DEFUNS FOR COMPILATION OF PARTICULAR DIRECTORIES:
;; All below defuns should me executed manually via M-x or via CLI scripts:

(defun cfg/eln-compile-elisp ()
  "Compile ALL files inside \"~/.emacs.d/elisp\" (and subfolders) into eln.
  WARNING! Could cause errors and hang emacs."
  (interactive)
  (native-compile-async (expand-file-name "elisp/" user-emacs-directory) 2 t))

(defun cfg/eln-compile-site-elisp ()
  "Compile all files inside \"~/.emacs.d/site-elisp\" into eln."
  (interactive)
  (native-compile-async (expand-file-name "elisp/site-elisp/" user-emacs-directory) 2 t))

;; overall performance options:
;; see Doom Emacs for inspiration:
;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; Other examples:
;; https://news.ycombinator.com/item?id=39190110
;; https://www.reddit.com/r/emacs/comments/r7qah6/emacs_is_bloat_and_memory_intensive/

;; Set garbage collection threshold to 8GB:
(setq gc-cons-threshold #x200000000)
;;(setq gc-cons-threshold most-positive-fixnum)

(setq read-process-output-max (* 1024 1024))

;; https://akrl.sdf.org/#orgc15a10d
;; When idle for 30sec run the GC no matter what:
;; This action ss to set a timer using run-with-idle-timer.
;; That means that every time Emacs will be idle for 30 secs we'll garbage collect once.
;; The assumption is that the probability that we are going to input a command exactly after 30 secs is rather low.

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defvar k-gc-timer
  (run-with-idle-timer 30 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONFIGURATION FOR PACKAGES AND REPOSITORIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic setup for use-package and additional repositories.
;; https://jwiegley.github.io/use-package/
;;

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
;; files/packages from "optional" namespace should be loaded manually, via (require '):
(add-to-list 'load-path (expand-file-name "elisp/optional" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/abbrevs-tempo" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/site-elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "data/local" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "elisp/cfg-general" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/cfg-general/cfg-gen-optional" user-emacs-directory))


;; TODO: add also subdirs
;;
;; found here: https://stackoverflow.com/questions/56799992/how-can-i-register-a-recursive-load-path
;; This piece of code doesn't work:
;; (let ((default-directory (expand-file-name "site-elisp/" user-emacs-directory))) (normal-top-level-add-subdirs-to-load-path))

;; https://www.reddit.com/r/emacs/comments/1rdstn/set_packageenableatstartup_to_nil_for_slightly/
(require 'package)
(setq package-enable-at-startup nil)
(require 'use-package)

;; The following lines tell emacs where on the internet to look up for new packages (elisp repositories):
;; WARNING! The same settings could be used separately for epm (.epm.el) package - so please also look at .epm.el file
;; WARNING! Do not use marmalade - is an obsolete repository : https://marmalade-repo.org/#download

(setq package-archives '(
                         ("gnu"    . "http://elpa.gnu.org/packages/")
                         ("melpa"  . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ))

;; defuns

(defun cfg/load-all-el-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded.
  This function is to avoid re-loading a library when both .el and .elc versions are present.
  More info and solutions:
  https://stackoverflow.com/questions/18706250/emacs-require-all-files-in-a-directory
  https://www.emacswiki.org/emacs/LoadingLispFiles"
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

(defun cfg/load-all-el-in-directory-alt (dir)
  "`load' all elisp libraries in directory DIR - alternative version."
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DECLARATIVE LIST OF CORE PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Additional core packages: packages which are required for core config.
;; Just make sure that all pkgs in the list are installed and nothing more.
;; For additional config - "use-package" should be used.
;;

(dolist (core-packages
	 '(
	   which-key
	   evil
	   annalist ;; required for "evil-collection"
	   evil-collection
	   evil-org
	   general
	   wgrep
	   ))
  (unless (package-installed-p core-packages) (package-install core-packages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FULL LIST FOR AUTO-MODE-ALIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Full list with major modes and files:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html
;; https://www.emacswiki.org/emacs/AutoModeAlist
;;

(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot:
       '(("\\.*_conf_file\\'" . conf-mode)
	 ("\\.npmrc\\'" . conf-mode)
	 ("\\.bash_aliases\\'" . conf-mode)
	 ("\\.muttrc\\'" . conf-mode)
	 ("fish_variables\\'" . conf-mode)
	 ("\\pkgs_arch.txt\\'" . conf-mode)
	 ("\\.tmux.conf_x11\\'" . conf-mode)
	 ("\\.Xresources\\'" . conf-xdefaults-mode)
	 ("\\abbrev_defs\\'" . emacs-lisp-mode)
	 ("\\.gcs\\'" . text-mode)
	 ("\\.zsh\\'" . sh-mode)
	 ("\\.bashrc\\'" . sh-mode)
	 ("\\.bash_profile\\'" . sh-mode)
	 ("zlogin\\'" . sh-mode)
	 ("zshenv\\'" . sh-mode)
	 ("zshrc\\'" . sh-mode))
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONFIGURATION FOR ENVIRONMENT VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; if true and dedicated file exists, load additional, local env variables
;;

(if (file-readable-p (expand-file-name "data/local/lo-env.el" user-emacs-directory))
    (require 'lo-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EVIL MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is related to evil-mode for Emacs.
;;

;; These variables must be in this place (and not inside `use-package'),
;; it is related to a configuration for `evil-collection':
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)  ;; https://jeffkreeftmeijer.com/emacs-evil-org-tab/
  :config
  (evil-mode 1)
  (setq evil-auto-indent nil)

  ;; implementation of evil commands:
  (evil-ex-define-cmd "e[dit]" 'find-file)
  (evil-ex-define-cmd "b[uffers]" 'ibuffer)
  (evil-ex-define-cmd "E[x]" 'dired-jump)

  ;; standard undo emacs system
  (evil-set-undo-system 'undo-redo)
  ;; evil search module
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; https://github.com/proofgeneral/pg/issues/174
  ;; https://github.com/syl20bnr/spacemacs/issues/8853
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  ;; visual-line-mode
  ;; https://www.reddit.com/r/spacemacs/comments/f9w7r1/move_to_end_of_line_with_in_visuallinemode/
  (setq evil-respect-visual-line-mode t)

  ;; woarkarounds to add ESC as "quit" button everywhere :
  (defun cfg/minibuffer-keyboard-quit ()
    "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'cfg/minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state))

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :config
  ;; remove `term' integration from evil-collection:
  (setq evil-collection-mode-list
        (cl-remove-if
         (lambda (mode)
           (and (listp mode)
                (eq (car mode) 'term)))
         evil-collection-mode-list))
  ;;
  (evil-collection-init))

;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GENERAL.EL AND ALL KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup for general.el
;;

(use-package general
  :after evil
  :config

  ;; Split whole general.el mapping into small pieces
  (require 'cfg-gen-for-all-modes)
  (require 'cfg-gen-for-all-modes-fkeys)
  (require 'cfg-gen-for-many-modes)
  (require 'cfg-gen-core)
  

  ;; https://github.com/noctuid/general.el/issues/99
  ;; general-override-mode
  ;; :keymaps 'override
  ;; ^override evil keybindings
  (general-override-mode 1)

  ;; https://github.com/noctuid/general.el#automatic-key-unbinding
  ;; "To automatically prevent Key sequence starts with a non-prefix key errors without the need to explicitly unbind non-prefix keys, you can add (general-auto-unbind-keys) to your configuration file. This will advise define-key to unbind any bound subsequence of the KEY."
  (general-auto-unbind-keys))

;; Load general.el for all modes (global scope) and for many modes (but not all; local scope)...
;; (cfg/load-all-el-in-directory (expand-file-name "elisp/cfg-general" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONFIGURATION FOR CODE FORMATTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;; BUILT-IN FORMATTING via INDENTATION:
;; https://www.reddit.com/r/emacs/comments/q3rwes/anyone_using_code_formatter_for_elisp/

;; formatting - built in function - the same as C-x h C-M-\:
(defun cfg/built-in-format-via-indent ()
  "Format code using built in processing and default indentation."
  (interactive)
  (indent-region (point-min) (point-max)))

;; do NOT use tabs for indentation:
;; https://web-mode.org/
(setq-default indent-tabs-mode nil)

;; XML (nxml) FORMATTING:

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

;; PERL FORMATTING:

(defvar perl5-perltidy-executable "perltidy"
  "Location of perltidy executable.")

(defvar perl5-perltidy-options '("--quiet"
				 "--standard-error-output"
				 "--perl-best-practices"
				 "-l=185")
  "Command line options to pass to perltidy")

(defun cfg/perltidy-format ()
  "Format Perl5 code with perltidy: if region is active, operate on it, else operate on line."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONFIGFURATION FOR COMMENTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(use-package newcomment
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XREF AND FFAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(use-package xref
  :config

  (defun cfg/ffap ()
    "Standard verion of `ffatp' command (without embark)."
    (interactive)
    (ffap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VARIOUS TEXT MANIPULATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://github.com/bbatsov/crux/blob/master/crux.el
;;

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

(defun cfg/eol-analyze ()
  "Analyze the end-of-line style in the current buffer or active region.
If a region is active, analyze only that region; otherwise analyze the whole buffer.
Display one of: \"unix LF\", \"dos (windows) CRLF\", \"mac CR\", or \"mixed\"."
  (interactive)
  (let* ((use-region (use-region-p))
         (start (if use-region (region-beginning) (point-min)))
         (end   (if use-region (region-end)       (point-max)))
         (crlf 0)
         (lf   0)
         (cr   0))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\(\r\n\\|\n\\|\r\\)" end t)
        (let ((match (match-string 0)))
          (cond
           ((string= match "\r\n") (setq crlf (1+ crlf)))
           ((string= match "\n")   (setq lf   (1+ lf)))
           ((string= match "\r")   (setq cr   (1+ cr)))))))

    (let* ((type
            (cond
             ;; pure DOS/Windows
             ((and (> crlf 0) (= lf 0) (= cr 0)) "dos (winndows) EOL: CRLF")
             ;; pure Unix
             ((and (> lf 0) (= crlf 0) (= cr 0)) "unix EOL: LF")
             ;; pure old Mac
             ((and (> cr 0) (= lf 0) (= crlf 0)) "mac EOL: CR")
             ;; nothing found (empty buffer or region)
             ((and (= crlf 0) (= lf 0) (= cr 0)) "no EOL found")
             ;; mixed
             (t "mixed EOL types")))
           (scope (if use-region
                      "EOL for selected region -> "
                    "EOL for entire buffer -> ")))
      (message "%s%s" scope type))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HIDESHOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup for hideshow minor mode:
;; https://www.emacswiki.org/emacs/HideShow
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html
;; Please note that this package is useful and required for evil-mode.
;;

;; hideshow can be enabled/disabled for particular mode - for example:
;; (add-hook 'perl-mode-hook 'hs-minor-mode)
(use-package hideshow
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WHICH-KEY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup for which-key mode
;;

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-idle-delay 0.0)
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TAB-BAR-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(use-package tab-bar
  :ensure nil
  :defer t
  :bind
  (("C-x t <left>" . tab-bar-history-back)
   ("C-x t <right>" . tab-bar-history-forward)
   ("C-x t P" . #'cfg/tab-group-from-project)
   ("C-x t g" . #'cfg/tab-switch-to-group))
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  ;; (tab-bar-close-button-show nil)
  (tab-bar-close-button-show t)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  (tab-bar-separator "  ")
  (tab-bar-format '(tab-bar-format-tabs-groups
                    tab-bar-separator))
  :init
  ;;; --- OPTIONAL INTERNAL FN OVERRIDES TO DECORATE NAMES
  ;;
  ;; Below config will produce numeric tabs like:
  ;; >1< , >2< , >3< ...
  ;;

  ;; (defun tab-bar-tab-name-format-hints (name _tab i)
  ;;   (if tab-bar-tab-hints (concat (format "»%d«" i) "") name))

  ;; (defun tab-bar-tab-group-format-default (tab _i &optional current-p)
  ;;   (propertize
  ;;    (concat (funcall tab-bar-tab-group-function tab))
  ;;    'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

  ;;; --- UTILITIES FUNCTIONS
  (defun cfg/tab-group-from-project ()
    "Call `tab-group' with the current project name as the group."
    (interactive)
    (when-let* ((proj (project-current))
                (name (file-name-nondirectory
                       (directory-file-name (project-root proj)))))
      (tab-group (format "[%s]" name))))

  (defun cfg/tab-switch-to-group ()
    "Prompt for a tab group and switch to its first tab.
Uses position instead of index field."
    (interactive)
    (let* ((tabs (funcall tab-bar-tabs-function)))
      (let* ((groups (delete-dups (mapcar (lambda (tab)
                                            (funcall tab-bar-tab-group-function tab))
                                          tabs)))
             (group (completing-read "Switch to group: " groups nil t)))
        (let ((i 1) (found nil))
          (dolist (tab tabs)
            (let ((tab-group (funcall tab-bar-tab-group-function tab)))
              (when (and (not found)
                         (string= tab-group group))
                (setq found t)
                (tab-bar-select-tab i)))
            (setq i (1+ i)))))))

  ;;; --- TURNS ON BY DEFAULT
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EGLOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with eglot for emacs.
;; https://www.gnu.org/software/emacs/manual/html_node/eglot/Customizing-Eglot.html
;; https://github.com/joaotavora/eglot
;; https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/
;;

(use-package eglot
  :config
  (setq eglot-report-progress nil)
  (setq eglot-events-buffer-size 0)
  (setq eglot-autoshutdown t)
  ;; Experiments with different servers:
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  ;; (add-hook 'python-mode-hook 'eglot-ensure)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EDIFF, DIFF AND VDIFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customization for ediff:
;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html#Customization
;;

(use-package ediff
  :config
  ;; https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
  (setq-default ediff-forward-word-function 'forward-char)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IBUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configuration for ibuffer.
;; https://olddeuteronomy.github.io/post/emacs-ibuffer-config/
;;

(use-package ibuffer
  :config
  ;; turn off prompts "yes" or "no":
  (setq ibuffer-expert t)

  (defun cfg/toggle-ibuffer ()
    "If the current buffer is *Ibuffer*, use `cfg/alternate-buffer'. Otherwise, open `ibuffer'."
    (interactive)
    (if (eq major-mode 'ibuffer-mode)
	(cfg/alternate-buffer)
      (ibuffer)
      ;; (when (minibufferp)
      ;; 	(abort-recursive-edit))
      (deactivate-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONFIGURATION FOR TEMPBUF AND BUFFERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configuration for tempbuf (tempbuf will kill unused buffers after XX seconds) and buffer-menu (M-x Buffer-menu).
;; Additional defuns for manipulating with file path and buffers.
;;

;; tempbuf

;; (WARNING - this option could be aggresive!)
;;
;; More examples of configuration:
;; https://www.emacswiki.org/emacs/TempbufMode
;; https://www.emacswiki.org/emacs/tempbuf.el < source
;; https://github.com/DarwinAwardWinner/dotemacs-old/blob/master/site-lisp/settings/tempbuf-settings.el
;; https://github.com/biern/.emacs.d/blob/master/conf/tempbuf.el
;;

(require 'tempbuf)

;; clean native-compile buffer
(when (get-buffer "*Async-native-compile-log*")
  (setq tempbuf-minimum-timeout 5) ;; 5 seconds
  (switch-to-buffer "*Async-native-compile-log*")
  (turn-on-tempbuf-mode)
  (switch-to-buffer "*scratch*"))

;; tempbuf is working well and it will clean junk buffers:
(setq tempbuf-minimum-timeout 30)

;; example of usage:
;; (add-hook 'foo-mode-hook 'turn-on-tempbuf-mode)

;; defuns:

;; Don't ask for confirmation when refreshing the buffer
(defun cfg/revert-buffer ()
  "Revert the current buffer from its file without any confirmation."
  (interactive)
  (revert-buffer t t t)) ; ignore-auto, noconfirm, preserve-modes
(define-key global-map [remap revert-buffer] #'cfg/revert-buffer)

;; Define `cfg/switch-to-buffer' differently depending on whether
;; the `consult' package is available at the time this configuration is loaded.
(cond
 ((fboundp 'consult-buffer)
  ;; consult is installed and loaded (or will be loaded before this file finishes):
  (defun cfg/switch-to-buffer ()
    "Switch to another buffer using `consult-buffer'.
    If invoked from the minibuffer, abort the minibuffer instead."
    (interactive)
    (if (minibufferp)
        (abort-minibuffers)
      (call-interactively #'consult-buffer))))

 (t
  ;; consult is not available -> fall back to the built-in command:
  (defun cfg/switch-to-buffer ()
    "Switch to another buffer using the built-in `switch-to-buffer'.
    If invoked from the minibuffer, abort the minibuffer instead."
    (interactive)
    (if (minibufferp)
        (abort-minibuffers)
      (call-interactively #'switch-to-buffer)))))

(define-key global-map [remap switch-to-buffer] #'cfg/switch-to-buffer)

;;;###autoload
(defun cfg/kill-other-buffers (&optional arg)
  "Kill all other buffers.
  If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

;;;###autoload
(defun cfg/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window)))
     nil t)))

;;;###autoload
(defun cfg/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (yes-or-no-p
           (format "Are you sure you want to delete this file: '%s'?" name))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

;;;###autoload
(defun cfg/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
  If the buffer isn't visiting a file, ask if it should
  be saved to a file, or just renamed.

  If called without a prefix argument, the prompt is
  initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

;;;###autoload
(defun cfg/new-empty-buffer ()
  "Create a new empty buffer.
  New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
  It returns the buffer (for elisp programing).
  URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
  Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

;;;###autoload
(defun cfg/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

;; https://stackoverflow.com/questions/12715376/emacs-copy-pwd-of-the-current-buffer-to-clipboard

;;;###autoload
(defun cfg/show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

;; Copy file path

(defun cfg/-directory-path ()
  "Retrieve the directory path of the current buffer.

  If the buffer is not visiting a file, use the `list-buffers-directory' variable
  as a fallback to display the directory, useful in buffers like the ones created
  by `magit' and `dired'.

  Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
                                (file-name-directory file-name)
                              list-buffers-directory))
    (file-truename directory-name)))

(defun cfg/-file-path ()
  "Retrieve the file path of the current buffer.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun cfg/-file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (cfg/-file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun cfg/-file-path-with-line-column ()
  "Retrieve the file path of the current buffer, including line and column number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (cfg/-file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
                            ;; Emacs 26 introduced this variable. Remove this
                            ;; check once 26 becomes the minimum version.
                            (boundp column-number-indicator-zero-based)
                            (not column-number-indicator-zero-based))
                           (1+ (current-column))
                         (current-column))))))

;;;###autoload
(defun cfg/copy-directory-path ()
  "Copy and show the directory path of the current buffer.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (cfg/-directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

;;;###autoload
(defun cfg/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (cfg/-file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

;;;###autoload
(defun cfg/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (cfg/-file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

;;;###autoload
(defun cfg/copy-buffer-name ()
  "Copy and show the name of the current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

;;;###autoload
(defun cfg/copy-file-name-base ()
  "Copy and show the file name without its final extension of the current buffer."
  (interactive)
  (if-let (file-name (file-name-base (cfg/-file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

;;;###autoload
(defun cfg/copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (cfg/-file-path-with-line))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

;;;###autoload
(defun cfg/copy-file-path-with-line-column ()
  "Copy and show the file path of the current buffer, including line and column number.
  This function respects the value of the `column-number-indicator-zero-based' variable."
  (interactive)
  (if-let (file-path (cfg/-file-path-with-line-column))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

;;;###autoload
(defun cfg/open-with (arg)
  "Open visited file in default external program.
  When in dired mode, open file under the cursor.
  With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (derived-mode-p 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DIAGNOSE EMACS CONFFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Various tools for diagnosing Emacs config.
;; Inspired by doom Emacs doctor.el
;; For example:
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/sh/doctor.el
;; https://github.com/doomemacs/doomemacs/tree/master/modules/lang/erlang/doctor.el
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/elixir/doctor.el
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/javascript/doctor.el
;;

(defun cfg/diagnose-exec-find ()
  "Check that all CLI commands exist or not and make raport in dedicated buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*exec-find*")))
    (setq buffer-read-only nil)
    (goto-char (point-max))
    ;; (set-buffer buffer)
    (with-current-buffer "*exec-find*" (insert "executable-find raport:\n\n")
                         ;; (unless (executable-find "foo") (insert "foo : NOT FOUND\n"))
                         (if (executable-find "rg") (insert "rg : FOUND\n") (insert "rg (ripgrep) : NOT FOUND\n"))
                         (if (executable-find "git") (insert "git : FOUND\n") (insert "git : NOT FOUND\n"))
                         (if (executable-find "find") (insert "find : FOUND\n") (insert "find : NOT FOUND\n"))
                         (if (executable-find "fd") (insert "fd : FOUND\n") (insert "fd : NOT FOUND. Project filtering will be corrupted!\n"))
                         (if (executable-find "clang-format") (insert "clang-format : FOUND\n") (insert "clang-format : NOT FOUND : c/c++ formatting will not work!\n"))
                         (if (executable-find "make") 
                             (insert "make : FOUND\n") 
                           (insert "make : NOT FOUND : makefile will not be used!\n"))
                         (if (executable-find "xmllint") 
                             (insert "xmllint : FOUND\n") 
                           (insert "xmllint : NOT FOUND : formatting and nxml-mode will not work correctly!\n"))
                         (if (executable-find "perltidy") 
                             (insert "perltidy : FOUND\n") 
                           (insert "perltidy : NOT FOUND : formatting and cperl-mode will not work correctly!\n"))
                         (if (file-exists-p "~/.local/share/fonts/NFM.ttf") 
                             (insert "file/font NFM.ttf : FOUND\n") 
                           (insert "font NFM.ttf : NOT FOUND : doom-modeline will not work correctly: https://github.com/seagle0128/doom-modeline ! \n"))
                         ;; lsp:
                         (if (executable-find "pyright") 
                             (insert "pyright : FOUND\n") 
                           (insert "pyright : NOT FOUND : lsp for python will not work!\n"))
                         (if (executable-find "ccls") (insert "ccls : FOUND\n") (insert "ccls : NOT FOUND : lsp for c/c++ will not work!\n"))
                         (if (executable-find "clangd") (insert "clangd : FOUND\n") (insert "clangd : NOT FOUND : lsp for c/c++ will not work!\n"))
                         ;;
                         )
    (switch-to-buffer buffer)
    ;; (pop-to-buffer buffer)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMMON OPTIONS FOR EMACS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Example:
;; https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/init.el
;;

;; don't ask about .dir-locals.el variables:
(setq-default enable-local-variables :all)

;; https://www.reddit.com/r/emacs/comments/2mu7yi/disable_electric_indent_mode/
(electric-indent-mode -1)

;; STARTUP

;; print a default message in the empty scratch buffer opened at startup
;; (setq initial-scratch-message "Welcome message!")

(setq inhibit-startup-screen t)	; inhibit useless and old-school startup screen
(setq inhibit-startup-message t)
;; (setq initial-major-mode 'emacs-lisp-mode)
;;;; optional - use "M-x eshell" instead of *scratch*:
;; (add-hook 'emacs-startup-hook (eshell))

;; turn off scratch buffer and startup screen:
;; (kill-buffer "*scratch*")

;; Probably not required anymore - only used on xorg/xserver:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Resources.html
;; https://emacs.stackexchange.com/questions/13291/emacs-cursor-color-is-different-in-daemon-and-non-daemon-modes
;; (setq inhibit-x-resources t)

;; Reduce rendering overhead
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Disable bidirectional text scanning for performance
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)

;; MINIMAL UI AND BASIC APPEARANCE

(setq use-dialog-box nil)
(setq use-file-dialog nil)
;; (setq tooltip-use-echo-area t)

;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              (concat "Emacs - [p] " (project-name project))
            (concat "Emacs - " (buffer-name))))))

;; silent bell when you make a mistake:
(setq ring-bell-function 'ignore )

;; Fix archaic defaults:
;; sentence SHOULD end with only a point.
(setq sentence-end-double-space nil)

;; toggle wrapping text at the 80th character
(setq default-fill-column 80)
(set-cursor-color "#ffffff")

;; (scroll-bar-mode)
(scroll-bar-mode -1)
;; (horizontal-scroll-bar-mode)
(tool-bar-mode   -1)
;; (tooltip-mode    -1)
(menu-bar-mode   -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 '(auth-source-save-behavior nil)
 '(warning-suppress-types '((frameset))))

;; Optional Misc. UI tweaks
;; (blink-cursor-mode -1)                                ; Steady cursor
;; (pixel-scroll-precision-mode)                         ; Smooth scrolling

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Switching-Buffers.html
;; OPTIONAL - Make switching buffers more consistent:
;; (setopt switch-to-buffer-obey-display-actions t)


;; MOUSE INTEGRATION

;; Make right-click do something sensible
(context-menu-mode)

;;OPTIONAL - Enable horizontal scrolling
;;To Be Checked:
;; (setopt mouse-wheel-tilt-scroll t)
;; (setopt mouse-wheel-flip-direction t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CODING SYSTEMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UTF-8 and various coding systems
;;

;; use utf-8 by default:
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )
(setq default-process-coding-system '(utf-8 . utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BACKUPS AND SAVEHIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic setup for backups, savehist, and lockfiles.
;;

;; https://stackoverflow.com/questions/25245134/stop-emacs-creating-autosave-files-in-the-same-directory
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html
;; and issues with .#recentf:
;; https://github.com/syl20bnr/spacemacs/issues/5554
;; https://github.com/syl20bnr/spacemacs/issues/5186
;; https://github.com/syl20bnr/spacemacs/issues/10347
;; So probably it's better to turn lockfiles off:
(setq create-lockfiles nil)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ; transform backups file name

;; SAVEHIST
;; Save history of minibuffer

(savehist-mode)

;; BACKUPS
;; OPTIONAL - turn off backups:
;; (setq make-backup-files nil)

;; delete excess backup versions silently:
(setq delete-old-versions -1 )
;; which directory to put backups file:
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )

;; Don't litter file system with *~ backup files; put them all inside "~/.emacs.d/backups"
(defun cfg/-backup-file-name (fpath)
  "Return a new file path of a given file path.
  If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "backups/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setopt make-backup-file-name-function 'cfg/-backup-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LINE NUMBERS AND HIGHLIGHTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; More options:
;; https://www.emacswiki.org/emacs/LineNumbers
;; Disable line numbers in some major modes - via emacswiki:
;; "To disable this in certain major modes you can redefine display-line-numbers--turn-on:"
;;

(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes '(eshell-mode shell-mode ansi-term-mode erc-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement.  
  If you want customize (add new elements), then try to use push or add-to-list: 
  https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Variables.html "
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on () ;; no cfg/  here
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode)
(global-hl-line-mode 1) ; highlight current line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EMACS-LISP MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(use-package elisp-mode
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with built-in python programming.
;;

(use-package python
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PERL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with perl programming.
;;

;; perl-quote
;; https://user42.tuxfamily.org/perl-quote/index.html
;; ^ latest, updated and patched version will be located in the "elisp/site-elisp":
(use-package perl-quote )

(use-package cperl-mode
  ;;     :demand t
  :init
  (defalias 'perl-mode 'cperl-mode) ;; change this alias if you want back to the native perl-mode
  (eval-after-load "ffap" '(require 'ffap-perl-module))
  :config
  (setq cperl-electric-keywords nil)
  (clear-abbrev-table cperl-mode-abbrev-table)
  (setq cperl-indent-level 4))

;; perl find modules
;; https://www.emacswiki.org/emacs/CPerlMode#h5o-9

(defun cfg/-perl-module-path (module-name)
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
      path)))

(defun cfg/find-perl-module (module-name)
  (interactive "sPerl module name: ")
  (let ((path (cfg/-perl-module-path module-name)))
    (if path
	(find-file path)
      (error "Module '%s' not found" module-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CC-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; "CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C, Java, CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code."
;; cc-mode:
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
;; https://wikemacs.org/wiki/CC-mode
;; https://cc-mode.sourceforge.net/html-manual/index.html
;;

(use-package cc-mode
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMPILATION-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configuration for "compilation-mode" and various building's under emacs.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Building.html
;;

(use-package compile
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SGML/HTML/MHTML/XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; XML files (nxml)
(use-package nxml-mode
  :config

  (defun cfg/xml-xsd-validate (xsd-schema)
    "Validate the xml file with schema and xmllint."
    (interactive (list (read-file-name "Select XSD Schema: ")))
    (setq validate-file (buffer-file-name))
    (save-window-excursion
      (let ((buffer (get-buffer-create "*XSD Validator*")))
        (set-buffer buffer)
        (nxml-mode)
        (setq-local rng-validate-mode nil)
        (setq buffer-read-only nil)
        (goto-char (point-max))
        (pop-to-buffer buffer)
        (setq xml-valid-cmd (format "xmllint --schema %s --noout %s" xsd-schema validate-file))
        (shell-command xml-valid-cmd "*XSD Validator*")
        (setq buffer-read-only t)
        (bury-buffer)))
    (message "XSD validation finished. Please check result buffer.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; JS AND JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; js-mode is for js and json
;;

(use-package js
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SH-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; shell-script-mode (shell, bash - for editing files):
;; "shell-script-mode is an alias for `sh-mode' in `sh-script.el'."
;;

(use-package sh-script
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SHELL-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; "shell-mode" is major mode for interacting with an inferior shell.
;;

;; M-x shell default shell:
(use-package shell
  :config
  ;; https://stackoverflow.com/questions/9514495/how-to-define-a-function-to-run-multiple-shells-on-emacs
  (defun cfg/C-u-M-x-shell ()
    "Equivalent to C-u M-x shell RET"
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'shell)))
  ;; dirtrack-mode:
  (shell-dirtrack-mode t)
  (setq explicit-shell-file-name "bash"))

;; comint
(setq comint-prompt-read-only t)
(setq comint-process-echoes t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TERM-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customization for term-mode:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Term-Mode.html
;;

(use-package term
  :config
  ;; The maximum size in lines for term buffers.
  ;; Term buffers are truncated from the top to be no greater than this number.
  ;; Notice that a setting of "0" means "don’t truncate anything".
  (setq term-buffer-maximum-size 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GREP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with grep.
;; https://www.emacswiki.org/emacs/RecentFiles
;;

(use-package wgrep
  :config
  )

(defun cfg/grep-recentf (filepattern pattern)
  (interactive "sFiles regexp: \nsSearch regexp: ")
  (let ((files (if filepattern
                   (cl-remove-if-not (lambda (item) (string-match filepattern item))
                                     recentf-list)
                 recentf-list))
        (limit 50)
        (grep-use-null-device nil))
    (if (> (length files) limit)
        (subseq files 0 limit))

    (let* ((tempfile (make-temp-file "emacs"))
           (orig compilation-finish-functions))
      (add-to-list 'compilation-finish-functions
                   (lambda (buf result)
                     (setq font-lock-keywords-case-fold-search t)
                     (highlight-regexp pattern 'hi-yellow)
                     (delete-file tempfile)
                     (setq compilation-finish-functions orig)))

      (write-region  (mapconcat 'identity files (char-to-string 0))
                     nil tempfile)
      (grep (format "%s %s | xargs -0 grep -n -i \"%s\" " pattern)))))

;;;; handle symlinks via grep and find:
;; grep command - add "-R" to follow symlinks:
(setq grep-command "grep --color=auto -nH --null -R -e ")

;; https://stackoverflow.com/questions/28915372/change-the-default-find-grep-command-in-emacs
;; change find command to check also for symlinks - it will be used via 'M-x rgrep':
(setq grep-find-template
      "find <D> <X> \\( -type f -o -type l \\) <F> -exec grep <C> -r -nH -e <R> \\{\\} +")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RECENTF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with "recentf" and similar filtering.
;; https://www.emacswiki.org/emacs/RecentFiles
;;

(use-package recentf
  :config

  (defun cfg/recentf-jump-open ()
    "Use `completing-read' to \\[find-file] a recent file
   https://www.masteringemacs.org/article/find-files-faster-recent-files-package "
    (interactive)
    (if (find-file (completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))

  (recentf-mode 1)
  ;; Save 10000 files for recentf mode:
  (setq recentf-max-saved-items 10000)
  (setq recentf-max-menu-items 10000)
  ;; "By default, Recentf saves the list of recent files on exiting Emacs (specifically, `recentf-save-list` is called on `kill-emacs-hook`).
  ;; If Emacs exits abruptly for some reason the recent file list will be lost - therefore you may wish to call `recentf-save-list` periodically, e.g. every 5 minutes:"
  ;;
  ;; https://stackoverflow.com/questions/8023670/change-number-of-files-recentf-in-emacs-stores-using-ido-completion-method
  (run-at-time nil (* 10 60) 'recentf-save-list)
  ;; Currently working on "pure" recentf-mode ("recentf-open-files").
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PRE CONFIGFURATION FOR CUSTOM.EL FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(setq custom-file (expand-file-name "data/custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file) (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PROJECT.EL WITH FD BACKEND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fast and idiomatic fd backend for project.el.
;;

(require 'project)
(require 'vc)
(require 'cl-lib)
(require 'seq)

;;;; User options

(defgroup project-fd nil
  "Improved project.el backend powered by fd."
  :group 'project)

(defcustom project-root-markers
  '(".gitignore" ".dir-locals.el" ".project")
  "Markers that indicate the root of a non-VC project."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-vc-extra-root-markers
  '(".gitignore" ".dir-locals.el" ".project")
  "Markers that reinforce identifying the VC project root."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-vc-ignores
  '(".ccls-cache/" ".git/")
  "Patterns excluded from fd results for VC projects."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-ignores
  '(".ccls-cache/" ".git/")
  "Patterns excluded from fd results in non-VC projects."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-build-dirs
  '("build/" "out/" "dist/" "target/")
  "Common build directories automatically excluded."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-fd-ignore-file ".gitignore"
  "File name used by fd backend to specify ignore rules.
If non-nil and exists in project root, fd will be called with
`--ignore-file` pointing to this file. Default is \".gitignore\"."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "File name"))
  :group 'project-fd)

(defvar project-fd-debug nil
  "If non-nil, output project/fd debug logs.")

(defun project-fd--debug (fmt &rest args)
  (when project-fd-debug
    (apply #'message (concat "[project-fd] " fmt) args)))

;;;; Helpers

(defun project-fd--marker-exists-p (dir marker)
  "Return t if MARKER exists inside DIR (follow symlinks)."
  (file-exists-p (expand-file-name marker (file-truename dir))))

(defun project-fd--dir-has-markers (dir markers)
  "Return non-nil if DIR contains one of MARKERS."
  (seq-some (lambda (m) (project-fd--marker-exists-p dir m))
            markers))

;;;; Project root finder

(defun project-fd--find-root (path)
  "Find project root starting from PATH. Return (TYPE . ROOT)."
  (let* ((path (file-name-as-directory (expand-file-name path)))
         ;; NOTE: vc-root-dir is slow — call *once*
         (vc-root (let ((default-directory path)) (vc-root-dir))))
    (project-fd--debug "Searching root from %s (vc=%s)" path vc-root)

    (catch 'found
      (while (not (equal path "/"))
        (project-fd--debug "Checking dir: %s" path)

        (let ((tru (file-truename path)))

          ;; --- VC project root ---
          (when (and vc-root
                     (equal tru (file-truename vc-root))
                     (project-fd--dir-has-markers path project-vc-extra-root-markers))
            (project-fd--debug "VC project root found: %s" path)
            (throw 'found (cons 'vc path)))

          ;; --- Local project markers ---
          (when (project-fd--dir-has-markers path project-root-markers)
            (project-fd--debug "Local project root found: %s" path)
            (throw 'found (cons 'local path)))

          ;; --- Transient markers (optional implementation) ---
          ;; (when (project-fd--dir-has-markers path project-transient-root-markers)
          ;;   (project-fd--debug "Transient project root found: %s" path)
          ;;   (throw 'found (cons 'transient path))))

          )
        ;; Go up
        (setq path (file-name-directory (directory-file-name path)))))))

(add-hook 'project-find-functions #'project-fd--find-root)

;;;; project-root methods

(cl-defmethod project-root ((project (head vc)))       (cdr project))
(cl-defmethod project-root ((project (head local)))    (cdr project))
(cl-defmethod project-root ((project (head transient))) (cdr project))

;;;; fd command builder — simplified and unified

(defun project-fd--make-fd-command (project root)
  "Return list of arguments for fd search in ROOT."
  (let* ((is-vc (eq (car project) 'vc))
         (ignores (if is-vc project-vc-ignores project-ignores))
         (cmd '("fd" "--type" "f" "--hidden" "--strip-cwd-prefix" ".")))

    ;; Respect ignore file if enabled
    (when (and project-fd-ignore-file
               (project-fd--marker-exists-p root project-fd-ignore-file))
      (setq cmd (append cmd (list "--ignore-file" project-fd-ignore-file))))

    ;; Excludes from VC or non-VC sets
    (setq cmd
          (append cmd
                  (mapcan (lambda (p) (list "--exclude" p)) ignores)))

    ;; Exclude build directories
    (setq cmd
          (append cmd
                  (mapcan (lambda (p) (list "--exclude" p))
                          project-build-dirs)))

    (project-fd--debug "FD(%s) → %S" (car project) cmd)
    cmd))

;;;; fd-based project-files implementation

(defun project-fd-files (project &optional _dirs)
  "Return list of files in PROJECT using fd."
  (let ((root (project-root project)))
    (unless (executable-find "fd")
      (user-error "Program 'fd' not found"))

    (let* ((default-directory root)
           (cmd (project-fd--make-fd-command project root)))
      (project-fd--debug "Running fd in %s" root)
      (apply #'process-lines cmd))))


(cl-defmethod project-files ((project (head vc)) &optional dirs)
  (project-fd-files project dirs))

(cl-defmethod project-files ((project (head transient)) &optional dirs)
  (project-fd-files project dirs))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  (project-fd-files project dirs))

;;;; `project-find-dir' improved command

;;;###autoload
(defun cfg/project-find-dir ()
  "Start Dired in a directory inside the current project.

The current buffer's `default-directory' is available as part of
\"future history\"."
  (interactive)
  (let* ((project (project-current t))
         (root    (project-root project))
         (all-files (project-files project))
         (all-dirs (delete-dups
                    (delq nil
                          (mapcar (lambda (f)
                                    (let ((d (file-name-directory f)))
                                      (when (and d (file-directory-p d))
                                        (expand-file-name d root))))
                                  all-files))))
         (default (or (and default-directory
                           (project--find-default-from default-directory project))
                      root))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (dir (if all-dirs
                  (project--read-file-name
                   project "Dired" all-dirs nil 'file-name-history default)
                root)))
    (dired dir)))

(defalias 'project-find-dir #'cfg/project-find-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VCS/GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with Version Control Systems (core version).
;;

(setq version-control t)		; use version control
;; TODO : update path with <emacs-user-directory>
(setq vc-make-backup-files nil) 	; make backups file even when in version controlled dir
(setq vc-follow-symlinks t )		; don't ask for confirmation when opening symlinked file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with "dired".
;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
;;

(use-package dired
  :config
  ;; "Make dired use the same buffer for viewing directory":
  (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-find-file)

  ;; When you do copy files, emacs prompts for a target dir.
  ;; You can make emacs automatically suggest the target dir on the split pane.
  (setq dired-dwim-target t))

(defcustom list-of-dired-switches
  '("-l" "-la" "-lA" "-lA --group-directories-first")
  "List of ls switches for dired to cycle among.")

(defun cfg/cycle-dired-switches ()
  "Cycle through the list `list-of-dired-switches' of switches for ls"
  (interactive)
  (setq list-of-dired-switches
        (append (cdr list-of-dired-switches)
                (list (car list-of-dired-switches))))
  (dired-sort-other (car list-of-dired-switches)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TRAMP AND SUDO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configuration for TRAMP and sudo.
;;

;; tramp
;; more examples for tramp-parse : https://github.com/abo-abo/swiper/issues/59

(use-package tramp
  :config
  (setq my-tramp-ssh-completions
	'((tramp-parse-sconfig "~/.ssh/ssh_config")
          (tramp-parse-shosts "~/.ssh/known_hosts")))

  (mapc (lambda (method)
          (tramp-set-completion-function method my-tramp-ssh-completions))
	'("fcp" "rsync" "scp" "scpc" "scpx" "sftp" "ssh")))

;; sudo via tramp

(defun cfg/sudo-edit (&optional arg)
  "Edit buffer / file as sudo user"
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name parsed-method
                                                     parsed-user
                                                     parsed-host
                                                     nil
                                                     parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name "sudo"
                                                       "root"
                                                       parsed-host
                                                       parsed-localname
                                                       new-hop)))
           new-fname))))))

(defun cfg/sudired ()
  "Open current directory via sudo and dired."
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with `org-mode'.
;;

;;
;; old ISSUE - force install GNU ELPA version (and ignore built-in pkg):
;; https://github.com/jwiegley/use-package/issues/955
;; ^^ Remember to put it high in your config file so Emacs doesn't load the built-in org-mode first, otherwise you might get weird stuff like (void-function org-assert-version).

;;
;; New options since Emacs 29+++
(use-package org
  :pin gnu ;; gnu must be there, other versions are buggy!
  :ensure t
  :config

  ;; Please create correct "lo-org.el" file inside ~/.emacs.d/data/local/lo-org.el (or other emacs dir)
  ;; Please add below variables inside this file:
  ;; (setq org-agenda-files ...

  (if (file-readable-p (expand-file-name "data/local/lo-org.el" user-emacs-directory))
      (require 'lo-org) ; if true, load additional variables for org-mode
					; if false, then message with "WARNING" will appear during initialization of org-mode:
    (message "WARNING! File data/local/lo-org.el inside your emacs.d is not readable (or not exist)! Please create it and add correct org-mode options!"))

  (setq org-babel-default-header-args
        (assq-delete-all :tangle org-babel-default-header-args))

  (require 'org-compat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IRC and rcirc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Everything what is connected with IRC inside Emacs (via rcirc).
;; EmacsWiki: https://www.emacswiki.org/emacs/rcirc
;;

(defun cfg/rcirc-rename-buffer-as-chan ()
  (let ((buffer (current-buffer)))
    (when (and (rcirc-buffer-process)
               (eq (process-status (rcirc-buffer-process)) 'open))
      (if (rcirc-channel-p rcirc-target)
	  (rename-buffer rcirc-target)))))

;; Dynamically set fill-column at redisplay time:
;; Source: https://www.emacswiki.org/emacs/rcircAutoFillColumn

(defvar rcirc-dynamic-fill-column-margin 3
  "Safety margin used to calculate fill-column depending on window-width")

(defun cfg/rcirc-dynamic-fill-column-window (window &optional margin)
  "Dynamically get window's width and adjust fill-column accordingly"
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'rcirc-mode)
      (setq fill-column
	    (- (window-width window)
	       (or margin rcirc-dynamic-fill-column-margin))))))

(defun cfg/rcirc-dynamic-fill-column (frame)
  "Dynamically tune fill-column for a frame's windows at redisplay time"
  (walk-windows 'cfg/rcirc-dynamic-fill-column-window 'no-minibuf frame))

(use-package rcirc
  :defer t
  :config

  ;; Please create correct "lo-rcirc.el" file inside ~/.emacs.d/data/local/lo-rcirc.el (or other emacs dir)
  ;; Please add below variables inside this file:
  ;; (setq rcirc-server-alist ...
  ;; (setq rcirc-authinfo ...

  (if (file-readable-p (expand-file-name "data/local/lo-rcirc.el" user-emacs-directory))
      (require 'lo-rcirc) ; if true, load additional variables for rcirc
					; if false, then message with "WARNING" will appear during initialization of rcirc:
    (message "WARNING! File data/local/lo-rcirc.el inside your emacs.d is not readable (or not exist)! Please create it and add correct rcirc options!"))

  (add-to-list 'window-size-change-functions 'cfg/rcirc-dynamic-fill-column)
  (setq rcirc-prompt "»» ")
  (setq rcirc-time-format "%H:%M ")
  (setq rcirc-fill-flag nil)
  (add-hook 'rcirc-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'rcirc-mode-hook 'cfg/rcirc-rename-buffer-as-chan)
  (add-hook 'rcirc-mode-hook 'rcirc-omit-mode)
  (evil-set-initial-state 'rcirc-mode 'insert)
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "MODE"))) ;; you can add "AWAY"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAN AND HELP-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun cfg/show-major-mode ()
  "Display the current major mode in the minibuffer."
  (interactive)
  (message "Current major mode: %s" major-mode))

(use-package help-mode
  :config
  )

(use-package man
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FLYSPELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup for flyspell mode.
;;

;; Dependencies for flyspell: *aspell* dictionaries
;; For example - Arch Linux packages: aspell-en aspell-pl
(use-package flyspell
  :init
  :config
  (setq flyspell-default-dictionary "english"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DOC-VIEW AND IMAGE MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(use-package doc-view
  :config
  (setq doc-view-resolution 150)
  (setq doc-view-scale-internally nil)
  )

(use-package image-mode
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HYPERLINKS AND WEB BROWSERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "brave") ;; default browser: firefox or brave

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html
(global-goto-address-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ABBREVS AND TEMPO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Abbrevs should be loaded AFTER any prog- mode (bcz will overwrite existing abbrev table)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
;; http://xahlee.info/emacs/emacs/elisp_abbrev_hook.html
;;

;; This file should be used for private abbrevs:
(setq abbrev-file-name (expand-file-name "data/abbrev_defs" user-emacs-directory))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; If the user has a file of abbrevs, read it (unless -batch):
(when (and (not noninteractive)
	   (file-exists-p abbrev-file-name)
	   (file-readable-p abbrev-file-name))
  (progn
    (require 'at-abbrev_defs) ;; main file with abbrev defs
    (require 'at-long-lines) ;; defs with long lines
    ;; This file should be used for private abbrevs:
    (quietly-read-abbrev-file abbrev-file-name)))

(defun cfg/expand-abbrev ()
  "Try to expand abbrev at point. If no expansion, prompt to select from the current mode's abbrev table."
  (interactive)
  (if (expand-abbrev)
      (message "Abbrev expanded.")
    (let* ((abbrev-table-symbol (intern (concat (symbol-name major-mode) "-abbrev-table")))
	   (abbrev-table (and (boundp abbrev-table-symbol) (symbol-value abbrev-table-symbol))))
      (if abbrev-table
	  (let* ((abbrev (completing-read "Select abbrev: " abbrev-table))
		 (expansion (abbrev-expansion abbrev abbrev-table)))
	    (when expansion
	      ;; Slightly dangerous, but should work - delete from beginning of line to point and insert abbrev:
	      (delete-region (line-beginning-position) (point))
	      (insert expansion)))
	(message "No abbrev found.")))))

(provide 'init_core)
;;; init_core.el ends here
