;;; cfg-op-web-mode.el --- configfuration for front-end development (web) -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with web-mode
;; https://web-mode.org/
;; https://github.com/fxbois/web-mode
;; https://themkat.net/2022/10/04/emacs_web_mode_mixed.html
;; https://github.com/smihica/emmet-mode - unmaintained, but still useful
;;
;;; Code:

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-engines-alist
	'(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  (setq web-mode-enable-css-colorization t
	web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-quoting t
        web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t)
  )

;; load general.el and keybindings:
;; (require 'cfg-gen-op-web-mode) ;; for some reason, sometimes its not working
(load "cfg-gen-op-web-mode.el" nil t) ;; NOMESSAGE = t


;; OPTIONAL: https://github.com/skeeto/skewer-mode - REPL inside the web browser

;; https://github.com/osv/company-web
(use-package company-web
  :ensure t
  :config
  (require 'company-web-html)
  ;;;; Additional company-web- backends should be loaded if required:
  ;; (require 'company-web-jade)
  ;; (require 'company-web-slim)
  ;; :defer t
  ;; :after (company)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; twig-cs-fixer
;; https://github.com/VincentLanglet/Twig-CS-Fixer
;; requires twig-cs-fixer to be in $PATH

(defvar cfg/twig-cs-fixer-executable "twig-cs-fixer"
  "Location of twig-cs-fixer executable.")

(defvar cfg/twig-cs-fixer-options "--no-cache"
  "Extra options passed to twig-cs-fixer CLI.
Set to a string of arguments, e.g. \"--no-cache --verbose\".")

(defun cfg/twig-cs-fixer-fix-buffer ()
  "Run `twig-cs-fixer fix' on the current buffer file.
This saves the buffer, runs the fixer, and reloads the buffer
to reflect any changes made by twig-cs-fixer, without showing 'Mark set'."
  (interactive)
  (if (and buffer-file-name (executable-find cfg/twig-cs-fixer-executable))
      (progn
        (save-buffer)
        (shell-command
         (concat cfg/twig-cs-fixer-executable
                 " fix "
                 cfg/twig-cs-fixer-options " "
                 (shell-quote-argument buffer-file-name)))
        (save-excursion
          (revert-buffer t t t)
          (setq mark-active nil)))
    (error "No file associated with buffer or twig-cs-fixer not found")))

(defun cfg/twig-cs-fixer-lint-buffer ()
  "Run `twig-cs-fixer lint' on the current buffer file.
This saves the buffer, runs the linter, and reloads the buffer
if necessary to reflect any changes, without showing 'Mark set'."
  (interactive)
  (if (and buffer-file-name (executable-find cfg/twig-cs-fixer-executable))
      (progn
        (save-buffer)
        (shell-command
         (concat cfg/twig-cs-fixer-executable
                 " lint "
                 cfg/twig-cs-fixer-options " "
                 (shell-quote-argument buffer-file-name)))
        (save-excursion
          (revert-buffer t t t)
          (setq mark-active nil)))
    (error "No file associated with buffer or twig-cs-fixer not found")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cfg/webmode-fix-buffer ()
  "Dispatch fixer based on template type."
  (interactive)
  (cond
   ((and buffer-file-name (string-suffix-p ".twig" buffer-file-name))
    (cfg/twig-cs-fixer-fix-buffer))
   ;; ((and buffer-file-name (string-suffix-p ".blade.php" buffer-file-name))
   ;;   (cfg/blade-fixer-fix-buffer))
   ;; etc
   (t (error "No fixer defined for this file type"))))

(defun cfg/webmode-lint-buffer ()
  "Dispatch linter based on template type."
  (interactive)
  (cond
   ((and buffer-file-name (string-suffix-p ".twig" buffer-file-name))
    (cfg/twig-cs-fixer-lint-buffer))
   ;; ((and buffer-file-name (string-suffix-p ".blade.php" buffer-file-name))
   ;;   (cfg/blade-fixer-lint-buffer))
   ;; etc
   (t (error "No linter defined for this file type"))))

(provide 'cfg-op-web-mode)
;;; cfg-op-web-mode.el ends here
