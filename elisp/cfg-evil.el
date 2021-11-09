;;; cfg-evil.el --- evil mode -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with evil-mode for emacs.

;;; Code:

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit))
  )

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-C-i-jump nil) ;; https://jeffkreeftmeijer.com/emacs-evil-org-tab/
  :config
  (evil-mode 1)
  ;; woarkarounds to add ESC as "quit" button everywhere :
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)
  (evil-define-key 'normal lsp-mode-map (kbd "\\") lsp-command-map)
  (evil-define-key 'visual lsp-mode-map (kbd "\\") lsp-command-map)
  ;; standard undo emacs system
  (evil-set-undo-system 'undo-redo)
  ;; evil search module
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; https://github.com/proofgeneral/pg/issues/174
  ;; https://github.com/syl20bnr/spacemacs/issues/8853
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  ;; visual-line-mode
  ;; https://www.reddit.com/r/spacemacs/comments/f9w7r1/move_to_end_of_line_with_in_visuallinemode/
  (setq evil-respect-visual-line-mode t)
  )

;; useful package:
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

;; https://github.com/Somelauw/evil-org-mode

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

;; evil-markdown-mode
;; https://github.com/Somelauw/evil-markdown
;; this is local package

(use-package evil-markdown
  :after evil
  )

;; TODO
;; https://github.com/emacs-evil/evil-magit

;; (use-package evil-magit
;;   :ensure t
;;   :config
;;   ;;
;;   )


(provide 'cfg-evil)
;;; cfg-evil.el ends here
