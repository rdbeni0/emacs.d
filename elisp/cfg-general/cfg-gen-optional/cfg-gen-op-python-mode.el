;;; cfg-gen-op-python-mode.el --- general.el for python-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'python-mode-map
 :major-modes 'python-mode
 :prefix ","
 "c"  '(sphinx-doc :which-key "sphinx-doc")
 "v"  '(:ignore t :which-key "venv")
 "va" '(pythonic-activate :which-key "pythonic-activate")
 "vd" '(pythonic-deactivate :which-key "pythonic-deactivate")
 "=y" '(yapfify-buffer :which-key "yapfify-buffer"))


(provide 'cfg-gen-op-python-mode)
;;; cfg-gen-op-python-mode.el ends here
