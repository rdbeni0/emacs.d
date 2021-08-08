;;; cfg-company.el --- configfuration for company-mode -*- lexical-binding: t -*-
;;; Commentary:

;; company-mode
;; https://company-mode.github.io/

;;; Code:

(use-package company
  :defer 2
  :diminish
  :commands
  (company-mode company-indent-or-complete-common)
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))
  )

;; https://github.com/expez/company-quickhelp

(use-package company-quickhelp
  :after company
  :ensure t
  :config
  ;;  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.1)
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  (add-hook 'company-mode-hook #'company-quickhelp-mode)
  (define-key company-active-map (kbd "<f1>") #'company-show-doc-buffer-f1) ; TODO - migrate to general.el
  )

;; https://emacs.stackexchange.com/questions/2762/jump-to-documentation-buffer-with-company-mode

(defun company-show-doc-buffer-f1 ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t))
  )

(provide 'cfg-company)
;;; cfg-company.el ends here
