;;; cfg-gen-op-company.el --- general.el - configuration for company-mode -*- lexical-binding: t -*-

;; global - no space + no which-key
(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global

 ;; "S-<iso-lefttab>" 'indent-for-tab-command
 "S-<iso-lefttab>" 'company-indent-or-complete-common
 "C-`" 'yas-insert-snippet

 "<f5>`"  '(yas-insert-snippet :which-key "yas-insert-snippet")
 ;; "<f5><f5>"  '(company-complete-common :which-key "company-abbrev")
 "<f5><iso-lefttab>"  '(company-indent-or-complete-common :which-key "company-complete")
 "<f5><f7>"  '(company-yasnippet :which-key "company-yasnippet")
 "<f5><f8>"  '(company-ispell :which-key "company-ispell")
 "<f5><f9>"  '(company-files :which-key "company-files")
 "<f5>o" '(company-diag :which-key "company-diagnose"))

;; global - space as leader-key + which-key
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 ;; "<f5><f5>"  '(company-complete-common :which-key "company-abbrev")
 "<f5><iso-lefttab>"  '(company-indent-or-complete-common :which-key "company-complete")
 "<f5><f7>"  '(company-yasnippet :which-key "company-yasnippet")
 "<f5><f8>"  '(company-ispell :which-key "company-ispell")
 "<f5><f9>"  '(company-files :which-key "company-files")
 "<f5>o" '(company-diag :which-key "company-diagnose")
 "<f5>`"  '(yas-insert-snippet :which-key "yas-insert-snippet"))

(provide 'cfg-gen-op-company)
;;; cfg-gen-op-company.el ends here
