;;; cfg-corfu.el --- configfuration for corfu and cape -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with completion corfu and cape.
;; https://github.com/minad/corfu
;; https://github.com/minad/cape

;;; Code:

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'

  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  :config
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 5)

  ;; Aggressive completion, cheap prefix filtering.

  ;; optional - use orderless:
  ;; (when (require 'orderless nil 'noerror)
  ;;   (defun orderless-fast-dispatch (word index total)
  ;;     (and (= index 0) (= total 1) (length< word 4)
  ;; 	   `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  ;;   (orderless-define-completion-style orderless-fast
  ;;     (orderless-style-dispatchers '(orderless-fast-dispatch))
  ;;     (orderless-matching-styles '(orderless-literal orderless-regexp)))

  ;;   (setq-local corfu-auto t
  ;; 		corfu-auto-delay 0
  ;; 		corfu-auto-prefix 2
  ;; 		completion-styles '(orderless-fast)) ;; warning, completion-styles
  ;;   )

  (setq-local corfu-auto t
              corfu-auto-delay 0
              corfu-auto-prefix 2
              )
  )

;; yasnippet:
;; https://github.com/elken/cape-yasnippet

;; Add extensions via cape:
;; https://github.com/minad/cape

;; (use-package cape
;;   :ensure t
;; (...)
;; )

(provide 'cfg-corfu)
;;; cfg-corfu.el ends here
