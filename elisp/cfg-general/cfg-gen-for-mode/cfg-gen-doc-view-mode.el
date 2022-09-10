;; doc-view mode ; with prefix

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'doc-view-mode-map
 :major-modes 'doc-view-mode
 :prefix ","
 "," '(doc-view-goto-page :which-key "goto-page")
 "<home>"'(doc-view-first-page :which-key "first-page")
 "<end>" '(doc-view-last-page :which-key "last-page")
 "k" '(doc-view-shrink :which-key "shrink")
 "l" '(doc-view-enlarge :which-key "enlarge")
 "0" '(doc-view-scale-reset :which-key "scale-reset")
 "P" '(doc-view-fit-page-to-window :which-key "fit-page-to-window")
 "W" '(doc-view-fit-width-to-window :which-key "fit-width-to-window")
 "H" '(doc-view-fit-height-to-window :which-key "fit-height-to-window")
 "/" '(doc-view-search :which-key "doc-view-search")
 "s" '(:ignore t :which-key "slice")
 "sr" '(doc-view-reset-slice :which-key "reset-slice")
 "ss" '(doc-view-set-slice-using-mouse :which-key "set-slice-using-mouse")
 "sb" '(doc-view-set-slice-from-bounding-box :which-key "set-slice-from-bounding-box")
 "sl" '(doc-view-set-slice :which-key "set-slice"))

;; doc-view mode ; without prefix

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'doc-view-mode-map
 :major-modes 'doc-view-mode
 "C-<mouse-4>" 'doc-view-enlarge
 "C-<mouse-5>"'doc-view-shrink
 "<right>" 'doc-view-next-page
 "<left>" 'doc-view-previous-page
 "<home>" 'doc-view-first-page
 "<end>" 'doc-view-last-page
 "k" 'doc-view-shrink
 "l" 'doc-view-enlarge
 "/" 'doc-view-search
 "W" 'doc-view-fit-width-to-window
 "H" 'doc-view-fit-height-to-window
 "P" 'doc-view-fit-page-to-window
 "0" 'doc-view-scale-reset)
