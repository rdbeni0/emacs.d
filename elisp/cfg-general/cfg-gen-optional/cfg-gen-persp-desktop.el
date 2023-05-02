
;; https://stackoverflow.com/questions/58615798/how-to-use-leader-key-as-part-of-package-prefix
(define-key evil-normal-state-map (kbd "SPC b p") 'perspective-map)
(setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory))

(general-define-key
 :prefix "SPC"
 :states '(normal visual emacs)
 :keymaps 'override
 "bp"  '(:ignore t :which-key "persp_desk")
 "bpd"  '(:ignore t :which-key "desktop")
 "bpdc"  '(desktop+-create :which-key "d-session-create")
 "bpdl"  '(desktop+-load :which-key "d-session-load")
 "bpds"  '(desktop-save :which-key "d-session-save")
 "bpD"  '(:ignore t :which-key "desktop")
 "bpDc"  '(desktop+-create :which-key "d-session-create")
 "bpDl"  '(desktop+-load :which-key "d-session-load")
 "bpDs"  '(desktop-save :which-key "d-session-save")
 "wd"  '(:ignore t :which-key "desktop-sessions")
 "wdc"  '(desktop+-create :which-key "d-session-create")
 "wdl"  '(desktop+-load :which-key "d-session-load")
 "wds"  '(desktop-save :which-key "d-session-save")
)

(provide 'cfg-gen-persp-desktop)
;;; cfg-gen-persp-desktop.el ends here
