;; general.el - configuration for company-mode:

;; global - no space + no which-key

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global

 ;; "S-<iso-lefttab>" 'indent-for-tab-command
 "S-<iso-lefttab>" 'cfg/yas-expand-or-company-complete

 "<f5><f5>"  '(company-complete-common :which-key "company-complete")
 "<f5><iso-lefttab>"  '(cfg/yas-expand-or-company-complete :which-key "yas-expand-or-company-complet") ;; or company-indent-or-complete-common
 "<f5><f7>"  '(company-yasnippet :which-key "company-yasnippet")
 "<f5><f8>"  '(company-ispell :which-key "company-ispell")
 "<f5><f9>"  '(company-files :which-key "company-files")
 "<f5><f12>" '(company-diag :which-key "company-diagnose"))

;; global - space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 ;; completions

 "<f5><f5>"  '(company-complete-common :which-key "company-complete")
 "<f5><iso-lefttab>"  '(cfg/yas-expand-or-company-complete :which-key "yas-expand-or-company-complet") ;; or company-indent-or-complete-common
 "<f5><f7>"  '(company-yasnippet :which-key "company-yasnippet")
 "<f5><f8>"  '(company-ispell :which-key "company-ispell")
 "<f5><f9>"  '(company-files :which-key "company-files")
 "<f5><f12>" '(company-diag :which-key "company-diagnose"))

(provide 'cfg-gen-company)
;;; cfg-gen-company.el ends here
