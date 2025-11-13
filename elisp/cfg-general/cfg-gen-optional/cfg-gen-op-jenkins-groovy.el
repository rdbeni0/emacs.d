;;; cfg-gen-op-jenkins-groovy.el --- general.el for groovy and jenkins -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(groovy-mode-map jenkinsfile-mode-map groovy-ts-mode-map jenkinsfile-ts-mode-map)
 :major-modes '(groovy-mode jenkinsfile-mode groovy-ts-mode jenkinsfile-ts-mode)
 :prefix ","
 "r" '(groovy-electric-mode :which-key "g-toggle-electric-mode")
 "="  '(:ignore t :which-key "format")
 "==" '(indent-region :which-key "indent-region")
 "=b" '(indent-region :which-key "indent-region")
 "e"  '(:ignore t :which-key "eval_REPL")
 "eg" '(run-groovy :which-key "run-groovy-process")
 "ee" '(groovy-send-region :which-key "send-region-to-process")
 "eE" '(groovy-send-region-and-go :which-key "send-region-t-p-go")
 "ef" '(groovy-load-file :which-key "load-file-to-process"))

(provide 'cfg-gen-op-jenkins-groovy)
;;; cfg-gen-op-jenkins-groovy.el ends here
