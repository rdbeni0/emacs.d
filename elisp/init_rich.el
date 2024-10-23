(load (expand-file-name "elisp/init_core.el" user-emacs-directory))

;; Common packages:
(require 'cfg-op-completion-systems)
(require 'cfg-op-doom-themes-fonts)
(require 'cfg-op-doom-mode-line)
(require 'cfg-op-treemacs-speedbar)
(require 'cfg-op-projectile)
(require 'cfg-op-dumbjump)
(require 'cfg-op-epm)
(require 'cfg-op-evil)
(require 'cfg-op-dired)
(require 'cfg-op-webpaste-htmlize)

;; Langs:
(require 'cfg-op-magit)
(require 'cfg-op-format)
(require 'cfg-op-python)
(require 'cfg-op-flycheck)
(require 'cfg-op-markdown)
(require 'cfg-op-json)
(require 'cfg-op-groovy-jenkins)
(require 'cfg-op-nix)
(require 'cfg-op-php)
(require 'cfg-op-web-mode)