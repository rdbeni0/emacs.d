;;; cfg-gen-op-yaml-mode.el --- general for yaml-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(yaml-mode-map yaml-ts-mode-map)
 :major-modes '(yaml-mode yaml-ts-mode)
 :prefix ","
 "a" '(yaml-electric-bar-and-angle :which-key "electric-bar-and-angle")
 "b" '(yaml-electric-backspace :which-key "electric-backspace")
 "d" '(yaml-electric-dash-and-dot :which-key "electric-dash-and-dot")
 "i" '(yaml-indent-line :which-key "indent-line")
 "p" '(yaml-fill-paragraph :which-key "fill-paragraph")
 "n" '(yaml-narrow-to-block-literal :which-key "narrow-to-block-literal"))

(provide 'cfg-gen-op-web-mode)
;;; cfg-gen-op-web-mode.el ends here

(provide 'cfg-gen-op-yaml-mode)
;;; cfg-gen-op-yaml-mode.el ends here
