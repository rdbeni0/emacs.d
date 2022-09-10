;;; cfg-general-which-keys.el --- general.el, which-key and all keys -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for general.el, which-key mode and all keybindings

;;; Code:

;; which-key

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-idle-delay 0.0)
  :config
  (which-key-mode))

;; evil-ex-define-cmd
;; unfortunately it seems that binding : vim commands needs to be done via dedicated function, but not via "general.el":
;; https://stackoverflow.com/questions/12913713/rebinding-b-and-e-in-evil-normal-state-map

(evil-ex-define-cmd "e[dit]" 'helm-find-files)
(evil-ex-define-cmd "b[uffers]" 'ibuffer)
(evil-ex-define-cmd "E[x]" 'dired-jump)

(use-package general
  :demand t
  :ensure t
  :config

  ;; https://github.com/noctuid/general.el/issues/99
  ;; general-override-mode
  ;; :keymaps 'override
  ;; ^override evil keybindings

  (general-override-mode 1)

  ;;;;;;;;;;;;;; Split whole general.el mapping into small pieces:

  ;; Load general.el in GLOBAL scope (for all modes)...
  (cfg/my-load-all-in-directory (expand-file-name "elisp/cfg-general" user-emacs-directory))
  ;; ...and for particular single mode...
  (cfg/my-load-all-in-directory (expand-file-name "elisp/cfg-general/cfg-gen-for-mode" user-emacs-directory))
  ;; ...and for particular feature (only for choosen modes):
  (cfg/my-load-all-in-directory (expand-file-name "elisp/cfg-general/cfg-gen-for-feature" user-emacs-directory)))

(provide 'cfg-general-which-keys)
;;; cfg-general-which-keys.el ends here
