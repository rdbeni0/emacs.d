;;; cfg-company-yasnippets.el --- configfuration for company-mode and yasnippets -*- lexical-binding: t -*-
;;; Commentary:

;; company-mode
;; https://company-mode.github.io/

;; yasnippet:
;; https://github.com/joaotavora/yasnippet
;; https://www.emacswiki.org/emacs/Yasnippet

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

;;;;;;;;;;;;;;;;;;;;;; yasnippets
;; ;; (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/data/yasnippets")))

(use-package yasnippet
  :ensure t
  )

;; https://github.com/AndreaCrotti/yasnippet-snippets

(setq yasnippets-dynamic-data-dir
      (substring
       ;;
       ;; optional:
       ;; (shell-command-to-string "find ~/.emacs.d/elpa/ -type d -iname snippets")
       ;;
       (shell-command-to-string "ls -d ~/.emacs.d/elpa/yasnippet-snippets-*/snippets")
       0 -1))

;; Adding a custom yasnippet directory:
;; https://stackoverflow.com/questions/46696009/adding-a-custom-yasnippet-directory-to-spacemacs

(use-package yasnippet-snippets
  :ensure t
  :config
  ;;
  ;; not necessary, but optional:
  ;; (setq yas-snippet-dirs (append yas-snippet-dirs (list yasnippets-dynamic-data-dir)))
  ;;
  (yas-reload-all)
  )

(provide 'cfg-company-yasnippets)
;;; cfg-company-yasnippets.el ends here
