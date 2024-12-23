;;; cfg-op-list-auto-mode-alist.el --- Full list for auto-mode-alist -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(setq auto-mode-alist
      (append
     ;; File name (within directory) starts with a dot.
    '(("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode)
	 ("\\.nix\\'" . nix-mode)
	 ("\\.php$'" . php-mode)
	 ("~/\\.ssh/config\\'" . ssh-config-mode)
	 ("ssh_config\\'" . ssh-config-mode)
	 ("sshd_config\\'" . ssh-config-mode)
	 ("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
	 ("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'"  . ssh-config-mode)
	 ("/known_hosts\\'"       . ssh-known-hosts-mode)
	 ("/authorized_keys?\\'" . ssh-authorized-keys-mode)
	 ("\\.html?\\'" . web-mode)
	 ("\\.js?\\'" . web-mode)
	 ("\\.css?\\'" . web-mode)
	 ("\\.twig?\\'" . web-mode)
	 ("\\.nix_stable?\\'" . nix-mode)
	 ("\\.nix_unstable?\\'" . nix-mode)
	 ) auto-mode-alist))

(provide 'cfg-op-list-auto-mode-alist)
;;; cfg-op-list-auto-mode-alist.el ends here
