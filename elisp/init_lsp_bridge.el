(load (expand-file-name "elisp/init_core.el" user-emacs-directory))

;; Common packages:
(require 'cfg-op-completion-systems)
(require 'cfg-op-company) ;; company should be FIRST, because after that some variables will be overwritten via lsp-bridge
(require 'cfg-op-yasnippets)
(require 'cfg-op-lsp-bridge)
(require 'cfg-op-list-auto-mode-alist)
(require 'cfg-op-doom-themes-fonts)
(require 'cfg-op-doom-mode-line)
;; (require 'cfg-op-tree-sitter)
(require 'cfg-op-pgtk)
(require 'cfg-op-treemacs-speedbar)
(require 'cfg-op-projectile)
(require 'cfg-op-dumbjump)
(require 'cfg-op-epm)
(require 'cfg-op-evil)
(require 'cfg-op-dired)
(require 'cfg-op-webpaste-htmlize)
(require 'cfg-op-line-numbers)
(require 'cfg-op-email)
(require 'cfg-op-vterm)

;; Langs:
(require 'cfg-op-magit)
(require 'cfg-op-format)
(require 'cfg-op-python)
(require 'cfg-op-python-anaconda)
(require 'cfg-op-fish)
(require 'cfg-op-ssh-config)
(require 'cfg-op-flycheck)
(require 'cfg-op-markdown)
(require 'cfg-op-json)
(require 'cfg-op-groovy-jenkins)
(require 'cfg-op-nix)
(require 'cfg-op-php)
(require 'cfg-op-org)
(require 'cfg-op-web-mode)
