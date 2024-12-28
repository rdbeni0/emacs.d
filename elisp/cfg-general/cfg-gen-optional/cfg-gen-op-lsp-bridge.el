;;; cfg-gen-op-lsp-bridge.el --- general.el for lsp-bridge -*- lexical-binding: t -*-

;; https://github.com/manateelazycat/lsp-bridge?tab=readme-ov-file#keymap
(general-define-key
 :prefix "\\"
 :states '(normal visual emacs)
 :keymaps 'override
 "9" '(lsp-bridge-indent-left :which-key "indent-left")
 "0" '(lsp-bridge-indent-right :which-key "indent-right")
 "=" '(lsp-bridge-code-format :which-key "format")
 ";" '(lsp-bridge-rename :which-key "rename")
 "]" '(lsp-bridge-find-type-def :which-key "jump-typeDefinition")
 "[" '(lsp-bridge-find-impl :which-key "jump-implementation")
 "p" '(lsp-bridge-find-def :which-key "def-find")
 "/" '(lsp-bridge-find-def-return :which-key "def-return")
 "l" '(lsp-bridge-find-references :which-key "find-references-rg")
 "U" '(lsp-bridge-show-documentation :which-key "show-buffer-doc")
 "u" '(lsp-bridge-popup-documentation :which-key "popup-doc")
 "<up>" '(lsp-bridge-popup-documentation-scroll-up :which-key "popup-doc-up")
 "<next>" '(lsp-bridge-popup-documentation-scroll-up :which-key "popup-doc-up")
 "<prior>" '(lsp-bridge-popup-documentation-scroll-down :which-key "popup-doc-down")
 "<down>" '(lsp-bridge-popup-documentation-scroll-down :which-key "popup-doc-down")
 "'" '(lsp-bridge-code-action :which-key "code-actions")
 "f" '(lsp-bridge-diagnostic-list :which-key "diagnostic")
 "," '(lsp-bridge-diagnostic-copy :which-key "diagnostic-copy")
 "i" '(lsp-bridge-toggle-sdcv-helper :which-key "toggle-dict-comletion")

 "e" '(:ignore t :which-key "lsp-bridge-peek")
 "ee" '(lsp-bridge-peek :which-key "peek")
 ;; lsp-bridge-peek-abort: Close peek window (default binding to C-g)
 "eq" '(lsp-bridge-peek-abort :which-key "quit")
 ;; lsp-bridge-peek-list-prev-line: Select the previous definition or reference (default binding to M-S-p)
 "e <up>" '(lsp-bridge-peek-list-prev-line :which-key "prev-line")
 "ek" '(lsp-bridge-peek-list-prev-line :which-key "prev-line")
 ;; lsp-bridge-peek-list-next-line: Select the next definition or reference (default binding to M-S-n)
 "e <down>" '(lsp-bridge-peek-list-next-line :which-key "next-line")
 "ej" '(lsp-bridge-peek-list-next-line :which-key "next-line")
 ;; lsp-bridge-peek-file-content-next-line: Scroll down one line in the peek window file content (default binding to M-n)
 "e <right>" '(lsp-bridge-peek-file-content-next-line :which-key "scroll-down")
 "el" '(lsp-bridge-peek-file-content-next-line :which-key "scroll-down")
 ;; lsp-bridge-peek-file-content-prev-line: Scroll up one line in the peek window file content (default binding to M-p)
 "e <left>" '(lsp-bridge-peek-file-content-prev-line :which-key "scroll-up")
 "eh" '(lsp-bridge-peek-file-content-prev-line :which-key "scroll-up")
 ;; lsp-bridge-peek-jump: Jump to the location of the definition or reference (default binding to M-l j)
 "ed" '(lsp-bridge-peek-jump :which-key "jump-def")
 "e <RET>" '(lsp-bridge-peek-jump :which-key "jump-def") ;; <enter>
 ;; lsp-bridge-peek-jump-back: Jump back to the original position (default bound to M-l b)
 "ea" '(lsp-bridge-peek-jump-back :which-key "jump-def-back")
 "e <backspace>" '(lsp-bridge-peek-jump-back :which-key "jump-def-back") ;; <backspace>
 ;; lsp-bridge-peek-through: View one symbol in the peek window
 "ev" '(lsp-bridge-peek-through :which-key "peek-through")

 ;; lsp-bridge-peek-tree-previous-branch: Select the previous branch at the same level in the browsing history (default binding to <up>)
 ;; lsp-bridge-peek-tree-next-branch: Select the next branch at the same level in the browsing history (default binding to <down>)
 ;; lsp-bridge-peek-tree-previous-node: Select the previous higher-level node in the browsing history (default binding to <left>)
 ;; lsp-bridge-peek-tree-next-node: Select the next lower-level node in the browsing history (default binding to <right>)
 "e1" '(lsp-bridge-peek-tree-previous-branch :which-key "prev-branch")
 "e4" '(lsp-bridge-peek-tree-next-branch :which-key "next-branch")
 "e2" '(lsp-bridge-peek-tree-previous-node :which-key "prev-node")
 "e3" '(lsp-bridge-peek-tree-next-node :which-key "next-node")

 "\\" '(:ignore t :which-key "server")
 "\\r" '(lsp-bridge-restart-process :which-key "reconnect")
 "\\p" '(lsp-bridge-profile-dump :which-key "profile-dump")
 "\\q" '(lsp-bridge-kill-process :which-key "kill-process")
 "\\Q" '(lsp-bridge-stop-process :which-key "kill-process") ;; "stop" is alias for lsp-bridge-kill-process
 "\\m" '(lsp-bridge-mode :which-key "lsp-bridge-mode"))

;; diagnostic, ref-mode
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'lsp-bridge-ref-mode-map
 :major-modes 'lsp-bridge-ref-mode
 :prefix ","
 "A" '(lsp-bridge-ref-quit :which-key "edit-discard-C-c_C-q")
 "a" '(lsp-bridge-ref-apply-changed :which-key "edit-apply-C-c_C-c")
 "D" '(lsp-bridge-ref-remove-line-from-results :which-key "remove-result")
 "F" '(lsp-bridge-ref-filter-mismatch-results :which-key "filter-mismatch")
 "X" '(lsp-bridge-ref-filter-mismatch-files :which-key "mismatch-files")
 "e" '(lsp-bridge-ref-switch-to-edit-mode :which-key "toggle-edit-mode")
 "E" '(lsp-bridge-ref-switch-to-view-mode :which-key "view-mode")
 "f" '(lsp-bridge-ref-filter-match-results :which-key "filter")
 "," '(lsp-bridge-diagnostic-copy :which-key "diagnostic-copy")

 "<left>" '(lsp-bridge-ref-jump-next-file :which-key "jump-next-file")
 "h" '(lsp-bridge-ref-jump-next-file :which-key "jump-next-file")
 "<down>" '(lsp-bridge-ref-jump-next-keyword :which-key "jump-next")
 "j" '(lsp-bridge-ref-jump-next-keyword :which-key "jump-next")
 "]" '(lsp-bridge-ref-jump-next-keyword :which-key "jump-next")
 "<up>" '(lsp-bridge-ref-jump-prev-keyword :which-key "jump-prev")
 "[" '(lsp-bridge-ref-jump-prev-keyword :which-key "jump-prev")
 "k" '(lsp-bridge-ref-jump-prev-keyword :which-key "jump-prev")
 "<right>" '(lsp-bridge-ref-jump-prev-file :which-key "jump-prev-file")
 "l" '(lsp-bridge-ref-jump-prev-file :which-key "jump-prev-file")

 "r" '(lsp-bridge-ref-replace-all-matches :which-key "replace")
 "u" '(lsp-bridge-ref-unfilter :which-key "unfilter")
 "x" '(lsp-bridge-ref-filter-match-files :which-key "match-files")
 "q" '(lsp-bridge-ref-quit :which-key "quit")
 "Q" '(kill-buffer-and-window :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'lsp-bridge-ref-mode-map
 :major-modes 'lsp-bridge-ref-mode
 "A" 'lsp-bridge-ref-quit
 "D" 'lsp-bridge-ref-remove-line-from-results
 "F" 'lsp-bridge-ref-filter-mismatch-results
 "Q" 'kill-buffer-and-window
 "X" 'lsp-bridge-ref-filter-mismatch-files
 "a" 'lsp-bridge-ref-apply-changed
 "e" 'lsp-bridge-ref-switch-to-edit-mode
 "f" 'lsp-bridge-ref-filter-match-results
 "h" 'lsp-bridge-ref-jump-next-file
 "j" 'lsp-bridge-ref-jump-next-keyword
 "k" 'lsp-bridge-ref-jump-prev-keyword
 "l" 'lsp-bridge-ref-jump-prev-file
 "q" 'lsp-bridge-ref-quit
 "r" 'lsp-bridge-ref-replace-all-matches
 "u" 'lsp-bridge-ref-unfilter
 "x" 'lsp-bridge-ref-filter-match-files
 )

(provide 'cfg-gen-op-lsp-bridge)
;;; cfg-gen-op-lsp-bridge.el ends here
