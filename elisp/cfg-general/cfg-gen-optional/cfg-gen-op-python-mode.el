;;; cfg-gen-op-python-mode.el --- general.el for python-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(python-mode-map python-ts-mode-map)
 :major-modes '(python-mode python-ts-mode)
 :prefix ","
 "c"  '(sphinx-doc :which-key "sphinx-doc")
 "i"  '(:ignore t :which-key "pyimport")
 "ir"  '(pyimport-remove-unused :which-key "pyimport-remove-unused")
 "ii"  '(pyimport-insert-missing :which-key "pyimport-insert-missing")
 "v"  '(:ignore t :which-key "venv")
 "=y" '(yapfify-buffer :which-key "yapfify-buffer"))

(provide 'cfg-gen-op-python-mode)
;;; cfg-gen-op-python-mode.el ends here
