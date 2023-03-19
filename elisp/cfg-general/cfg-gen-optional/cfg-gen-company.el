;; general.el - configuration for company-mode:

;; global - no space + no which-key

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global

 ;; "S-<iso-lefttab>" 'indent-for-tab-command
 "S-<iso-lefttab>" 'cfg/yas-expand-or-company-complete

 "<f5><f5>" '(company-yasnippet :which-key "company-yasnippet")
 "<f5><f7>" '(company-ispell :which-key "company-ispell")
 "<f5><f8>" '(company-files :which-key "company-files"))

;; global - space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 ;; completions

 "<f5><f5>" '(company-yasnippet :which-key "company-yasnippet")
 "<f5><f7>" '(company-ispell :which-key "company-ispell")
 "<f5><f8>" '(company-files :which-key "company-files"))

(provide 'cfg-gen-company)
;;; cfg-gen-company.el ends here
