;;; cfg-gen-op-pdf-tools.el --- general.el for webpaste in Emacs -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'pdf-view-mode-map
 :major-modes 'pdf-view-mode
 :prefix ","
 "e" '(pdf-view-goto-page :which-key "goto-page")
 "<home>"'(pdf-view-first-page :which-key "first-page")
 "<end>" '(pdf-view-last-page :which-key "last-page")
 "k" '(pdf-view-shrink :which-key "shrink")
 "l" '(pdf-view-enlarge :which-key "enlarge")
 "0" '(pdf-view-scale-reset :which-key "scale-reset")
 "P" '(pdf-view-fit-page-to-window :which-key "fit-page-to-window")
 "W" '(pdf-view-fit-width-to-window :which-key "fit-width-to-window")
 "H" '(pdf-view-fit-height-to-window :which-key "fit-height-to-window")
 "/" '(isearch-forward :which-key "isearch-forward")
 "s" '(:ignore t :which-key "slice")
 "sr" '(pdf-view-reset-slice :which-key "reset-slice")
 "ss" '(pdf-view-set-slice-using-mouse :which-key "set-slice-using-mouse")
 "sb" '(pdf-view-set-slice-from-bounding-box :which-key "set-slice-from-bounding-box")
 "sl" '(pdf-view-set-slice :which-key "set-slice"))

;; general-pdf-view mode: without prefix
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'pdf-view-mode-map
 :major-modes 'pdf-view-mode
 "C-<mouse-4>"'pdf-view-enlarge
 "C-<mouse-5>"'pdf-view-shrink
 "<right>"'pdf-view-next-page
 "<left>"'pdf-view-previous-page
 "<home>" 'pdf-view-first-page
 "<end>" 'pdf-view-last-page
 "k" 'pdf-view-shrink
 "l" 'pdf-view-enlarge
 "0" 'pdf-view-scale-reset
 "P" 'pdf-view-fit-page-to-window
 "W" 'pdf-view-fit-width-to-window
 "H" 'pdf-view-fit-height-to-window
 "/" 'isearch-forward)

(provide 'cfg-gen-op-pdf-tools)
;;; cfg-gen-op-pdf-tools.el ends here
