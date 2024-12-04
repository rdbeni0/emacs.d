;;; cfg-common-options.el --- configure common options -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Example:
;; https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/init.el
;;
;;; Code:

;; don't ask about .dir-locals.el variables:
(setq-default enable-local-variables :all)

;; https://www.reddit.com/r/emacs/comments/2mu7yi/disable_electric_indent_mode/
(electric-indent-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STARTUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print a default message in the empty scratch buffer opened at startup
;; (setq initial-scratch-message "Welcome message!")

(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq inhibit-startup-message t)
;; (setq initial-major-mode 'emacs-lisp-mode)
;;;; optional - use "M-x eshell" instead of *scratch*:
;; (add-hook 'emacs-startup-hook (eshell))

;; turn off scratch buffer and startup screen:
;; (kill-buffer "*scratch*")

;; Probably not required anymore - only used on xorg/xserver:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Resources.html
;; https://emacs.stackexchange.com/questions/13291/emacs-cursor-color-is-different-in-daemon-and-non-daemon-modes
;; (setq inhibit-x-resources t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MINIMAL UI AND BASIC APPEARANCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silent bell when you make a mistake:
(setq ring-bell-function 'ignore )

;; Fix archaic defaults:
;; sentence SHOULD end with only a point.
(setq sentence-end-double-space nil)

;; toggle wrapping text at the 80th character
(setq default-fill-column 80)
(set-cursor-color "#ffffff")

(scroll-bar-mode)
;; (scroll-bar-mode -1)
;; (horizontal-scroll-bar-mode)
(tool-bar-mode   -1)
;; (tooltip-mode    -1)
;; (menu-bar-mode   -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 '(auth-source-save-behavior nil)
 '(warning-suppress-types '((frameset))))

;; Optional Misc. UI tweaks
;; (blink-cursor-mode -1)                                ; Steady cursor
;; (pixel-scroll-precision-mode)                         ; Smooth scrolling

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Switching-Buffers.html
;; OPTIONAL - Make switching buffers more consistent:
;; (setopt switch-to-buffer-obey-display-actions t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MOUSE INTEGRATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;;OPTIONAL - Enable horizontal scrolling
;;To Be Checked:
;; (setopt mouse-wheel-tilt-scroll t)
;; (setopt mouse-wheel-flip-direction t)

(provide 'cfg-common-options)
;;; cfg-common-options.el ends here
