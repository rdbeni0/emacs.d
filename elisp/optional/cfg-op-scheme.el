;;; cfg-op-scheme.el --- configuration for scheme -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package geiser
  :ensure t
  :config
  (setq geiser-smart-tab-mode t)
  (require 'cfg-gen-op-scheme))

(use-package geiser-chicken
  :ensure t
  :after geiser)

;; Only keep the chicken implementation
(setq geiser-default-implementation 'chicken)
(setq geiser-active-implementations '(chicken))

;; Force the buffer-local Geiser implementation to chicken so scheme-chicken can run
(add-hook 'scheme-mode-hook
          (lambda ()
            (setq-local geiser-impl--implementation 'chicken)))

;; Flycheck configuration is applied only if Flycheck is actually loaded.
;; This keeps the config harmless when Flycheck is disabled or not installed.
(with-eval-after-load 'flycheck
  ;; Remove the problematic racket checker
  (setq flycheck-checkers (delq 'racket flycheck-checkers))
  ;; Enable Flycheck in scheme buffers
  (add-hook 'scheme-mode-hook #'flycheck-mode))

(defun cfg/geiser-quit-repl ()
  "Quit the Geiser REPL, hide its window, and leave the buffer in background.

When called from a REPL buffer, exit that REPL.  Otherwise assume we
are in a source buffer and find the associated REPL by buffer name.
The REPL buffer is not killed; its window is deleted and the buffer
is buried."
  (interactive)
  (let* ((from-repl (derived-mode-p 'geiser-repl-mode))
         (repl-buf (if from-repl
                       (current-buffer)
                     (or (ignore-errors (geiser-repl--repl-buffer))
                         ;; Fallback: find by name, handling Emacs <N> suffixes
                         (cl-find-if (lambda (b)
                                       (string-match-p "\\*Geiser .+ REPL\\*\\(<[0-9]+>\\)?$"
                                                       (buffer-name b)))
                                     (buffer-list)))))
         (win (and repl-buf (buffer-live-p repl-buf)
                   (get-buffer-window repl-buf))))
    (when (and repl-buf (buffer-live-p repl-buf))
      (with-current-buffer repl-buf
        (ignore-errors
          (if from-repl
              (geiser-repl-exit)
            (geiser-exit-repl))))

      ;; We do NOT call kill-buffer to avoid the "running process" prompt.
      (bury-buffer repl-buf))

    (when (and win (window-live-p win))
      (ignore-errors (delete-window win)))))

(provide 'cfg-op-scheme)
;;; cfg-op-scheme.el ends here
