;; general-python-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'python-mode-map
 :major-modes 'python-mode
 :prefix ","
 "j"  '(cfg/helm-jump-in-buffer :which-key "helm-jump-in-buffer")
 "h"  '(python-check :which-key "python-check")
 "s"  '(run-python :which-key "python-shell")
 "v"  '(:ignore t :which-key "venv")
 "va"  '(pythonic-activate :which-key "pythonic-activate")
 "vd"  '(pythonic-deactivate :which-key "pythonic-deactivate")
 "="  '(:ignore t :which-key "format")
 "==" '(yapfify-buffer :which-key "yapfify-buffer")
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
 "a"  '(:ignore t :which-key "anaconda-mode")
 "am" '(anaconda-mode :which-key "anaconda-mode"))

;; general-python-mode - use below only when anaconda mode is enabled:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'anaconda-mode-map
 :major-modes 'anaconda-mode
 :prefix ","
 "ak" '(xref-pop-marker-stack :which-key "xref-pop-marker-stack")
 "a?" '(anaconda-mode-show-doc :which-key "anaconda-mode-show-doc")
 "aa" '(anaconda-mode-find-assignments :which-key "find-assignments")
 "aA" '(anaconda-mode-find-assignments-other-window :which-key "find-assignments-ot-window")
 "ar" '(anaconda-mode-find-references :which-key "find-references")
 "aR" '(anaconda-mode-find-references-other-window :which-key "find-references-ot-window")
 "ad" '(anaconda-mode-find-definitions :which-key "find-definitions")
 "aD" '(anaconda-mode-find-definitions-other-window :which-key "find-definitions-ot-window"))

;; anaconda-mode - no prefix, it will override evil keymaps:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'anaconda-mode-map
 :major-modes 'anaconda-mode
 "gC" '(what-cursor-position :which-key "what-cursor-position")
 "ga" '(anaconda-mode-find-assignments :which-key "find-assignments")
 "gA" '(anaconda-mode-find-assignments-other-window :which-key "find-assignments-ot-window")
 "gr" '(anaconda-mode-find-references :which-key "find-references")
 "gR" '(anaconda-mode-find-references-other-window :which-key "find-references-ot-window")
 "gd" '(anaconda-mode-find-definitions :which-key "find-definitions")
 "gD" '(anaconda-mode-find-definitions-other-window :which-key "find-definitions-ot-window"))
