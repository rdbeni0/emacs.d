;;; cfg-completion-systems-native.el --- configfuration for completion styles and engines -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with completion styles in native Emacs.
;; Alternative version of "completion-systems".
;; Requires minimum Emacs 26.3++
;; As of version 28.1++ it changes the behavior

;;; Code:

;; http://xahlee.info/emacs/emacs/emacs_icomplete_mode.html
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
;; https://karthinks.com/software/more-batteries-included-with-emacs/


(if (version< emacs-version "28.1")
    (progn
      
        ;; ido:
        ;; make buffer switch command do suggestions, also for find-file command
        (require 'ido)
        (ido-mode 1)
	(setq ido-everywhere t)
        ;; show choices vertically
        (setf (nth 2 ido-decorations) "\n")
        ;; show any name that has the chars you typed
        (setq ido-enable-flex-matching t)
        ;; use current pane for newly opened file
        (setq ido-default-file-method 'selected-window)
        ;; use current pane for newly switched buffer
        (setq ido-default-buffer-method 'selected-window)

	;; icomplete
        ;; minibuffer enhanced completion icomplete
        (require 'icomplete)
        (icomplete-mode 1)
        ;; show choices vertically
        (setq icomplete-separator "\n")
        (setq icomplete-hide-common-prefix nil)
        (setq icomplete-in-buffer t)
        (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
        (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)

      )
  (fido-vertical-mode 1))

(provide 'cfg-completion-systems-native)
;;; cfg-completion-systems-native.el ends here
