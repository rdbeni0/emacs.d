;; general-groovy-mode and jenkinsfile-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(groovy-mode-map jenkinsfile-mode-map)
 :major-modes '(groovy-mode jenkinsfile-mode)
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "r" '(groovy-electric-mode :which-key "toggle-electric-mode")
 "="  '(:ignore t :which-key "format")
 "==" '(indent-region :which-key "indent-region")
 "=b" '(indent-region :which-key "indent-region")
 "e"  '(:ignore t :which-key "eval")
 "eg" '(run-groovy :which-key "run-groovy-process")
 "ee" '(groovy-send-region :which-key "send-region-to-process")
 "eE" '(groovy-send-region-and-go :which-key "send-region-t-p-go")
 "ef" '(groovy-load-file :which-key "load-file-to-process"))
