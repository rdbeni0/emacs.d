;;; cfg-gen-op-fzf-regexps.el --- general.el for regexps in Emacs -*- lexical-binding: t -*-

;; space as leader-key + which-key
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "fz"  '(fzf :which-key "fzf")

 "ar"   '(:ignore t :which-key "regexp")
 "arr"  '(regex-tool :which-key "regex-tool")
 "arq"  '(regex-tool-quit :which-key "regex-tool-quit"))

(provide 'cfg-gen-op-fzf-regexps)
;;; cfg-gen-op-fzf-regexps.el ends here
