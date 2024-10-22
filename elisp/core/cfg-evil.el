;;; cfg-evil.el --- evil mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with evil-mode for emacs.
;;
;;; Code:

(defun cfg/minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-C-i-jump nil)  ;; https://jeffkreeftmeijer.com/emacs-evil-org-tab/
  :config
  (evil-mode 1)
  (setq evil-auto-indent nil)
  ;; woarkarounds to add ESC as "quit" button everywhere :
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'cfg/minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)

  ;; standard undo emacs system
  (evil-set-undo-system 'undo-redo)
  ;; evil search module
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; https://github.com/proofgeneral/pg/issues/174
  ;; https://github.com/syl20bnr/spacemacs/issues/8853
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  ;; visual-line-mode
  ;; https://www.reddit.com/r/spacemacs/comments/f9w7r1/move_to_end_of_line_with_in_visuallinemode/
  (setq evil-respect-visual-line-mode t))

;; implementation of evil commands:

(evil-ex-define-cmd "e[dit]" 'find-file)
(evil-ex-define-cmd "b[uffers]" 'ibuffer)
(evil-ex-define-cmd "E[x]" 'dired-jump)

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(provide 'cfg-evil)
;;; cfg-evil.el ends here
