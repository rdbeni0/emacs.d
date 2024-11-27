;;; cfg-gen-op-json-mode.el --- general.el for json-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'json-mode-map
 :major-modes 'json-mode
 :prefix ","
 "=i" '(json-mode-beautify :which-key "jm-beautify")
 "p"  '(jsons-print-path :which-key "jsons-print-path")
 "n"  '(json-navigator-navigate-after-point :which-key "navigate-after-point")
 "r"  '(json-navigator-navigate-region :which-key "navigate-region")
 ;; built-in functionality:
 "=p" '(json-pretty-print-buffer :which-key "pretty-p-buffer")
 "=o" '(json-pretty-print-buffer-ordered :which-key "pretty-p-buffer-ordered")
 "=r" '(json-pretty-print-ordered :which-key "pretty-p-ordered"))

(provide 'cfg-gen-op-json-mode)
;;; cfg-gen-op-json-mode.el ends here
