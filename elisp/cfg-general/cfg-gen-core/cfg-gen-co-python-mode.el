;;; cfg-gen-co-python-mode.el --- general.el for python-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'python-mode-map
 :major-modes 'python-mode
 :prefix ","
 "j"  '(imenu :which-key "imenu")
 "h"  '(python-check :which-key "python-check")
 "s"  '(run-python :which-key "python-shell")
 "v"  '(:ignore t :which-key "venv")
 "n"  '(:ignore t :which-key "navigate")
 "nb" '(python-nav-beginning-of-block :which-key "beginnning-of-block")
 "nn" '(python-nav-end-of-block :which-key "end-of-block")
 "ne" '(python-nav-beginning-of-statement :which-key "beginnning-of-statement")
 "nd" '(python-nav-end-of-statement :which-key "end-of-statement")
 "nk" '(python-nav-backward-defun :which-key "backward-defun")
 "no" '(python-nav-forward-defun :which-key "forward-defun")
 "nl" '(python-nav-up-list :which-key "up-list")
 "ni" '(python-nav-backward-up-list :which-key "backward-up-list")
 "nc" '(python-nav-backward-sexp-safe :which-key "backward-sexp-safe")
 "nv" '(python-nav-forward-sexp-safe :which-key "forward-sexp-safe")
 "nw" '(python-nav-forward-sexp :which-key "forward-sexp")
 "na" '(python-nav-backward-sexp :which-key "backward-sexp")
 "e"  '(:ignore t :which-key "eval")
 "ee" '(python-shell-send-region :which-key "eval-region")
 "ef" '(python-shell-send-file :which-key "select-and-eval-file")
 "eb" '(python-shell-send-buffer :which-key "eval-buffer")
 "es" '(python-shell-send-string :which-key "eval-given-string")
 "et" '(python-shell-send-statement :which-key "eval-statement")
 "ed" '(python-shell-send-defun :which-key "eval-defun")
 "ep" '(python-shell-package-enable :which-key "package-enable"))

;; xref should be used with "anaconda-mode" (because of improved functionality):

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'python-mode-map
 :major-modes 'python-mode
 "gb" '(xref-go-back :which-key "xref-go-back")
 "gB" '(xref-go-forward :which-key "xref-go-forward"))

;; inferior-python-mode - no prefix

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'inferior-python-mode-map
 :major-modes 'inferior-python-mode
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q" '(kill-this-buffer :which-key "kill-this-buffer"))

(provide 'cfg-gen-co-python-mode)
;;; cfg-gen-co-python-mode.el ends here
