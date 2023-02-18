;; general-emacs-lisp-mode:

(general-define-key
   :states '(normal visual emacs)
   :keymaps 'emacs-lisp-mode-map
   :major-modes 'emacs-lisp-mode
   :prefix ","
   "c"    '(emacs-lisp-native-compile-and-load :which-key "compile-and-load")
   "e"    '(:ignore t :which-key "eval-elisp")
   "ee"   '(eval-region :which-key "eval-region")
   "eE"   '(eval-buffer :which-key "eval-buffer")
   "="    '(:ignore t :which-key "format")
   "=="   '(format-all-buffer :which-key "format-all-buffer")
   "=b"   '(format-all-buffer :which-key "format-all-buffer")
   "=o"   '(format-all-region :which-key "format-all-region")
   "a"    '(cfg/ffap :which-key "ffap"))
