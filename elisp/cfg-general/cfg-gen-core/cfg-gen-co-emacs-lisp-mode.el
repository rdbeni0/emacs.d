;;; cfg-gen-co-emacs-lisp-mode.el --- general.el for grep/wgrep mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'emacs-lisp-mode-map
 :major-modes 'emacs-lisp-mode
 :prefix ","
 "c"    '(emacs-lisp-native-compile-and-load :which-key "compile-and-load")
 "e"    '(:ignore t :which-key "eval-elisp")
 "ee"   '(eval-region :which-key "eval-region")
 "er"   '(eval-buffer :which-key "eval-buffer"))

(provide 'cfg-gen-co-emacs-lisp-mode)
;;; cfg-gen-co-emacs-lisp-mode.el ends here