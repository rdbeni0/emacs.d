;; general-image-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'image-mode-map
 :major-modes 'image-mode
 "q" 'kill-this-buffer
 "<left>" 'image-previous-file
 "<right>" 'image-next-file
 "L" 'image-rotate
 "R" 'image-rotate
 "." 'image-previous-frame)

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'image-mode-map
 :major-modes 'image-mode
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "d" '(image-dired :which-key "image-dired")
 "c" '(image-toggle-display :which-key "toggle-display")
 "<left>" '(image-previous-file :which-key "prev-file")
 "<right>" '(image-next-file  :which-key "next-file")
 "L" '(image-rotate :which-key "rotate")
 "R" '(image-rotate  :which-key "rotate")
 "H" '(image-transform-fit-to-height :which-key "fit-to-height")
 "W" '(image-transform-fit-to-width :which-key "fit-to-width")
 "O" '(image-transform-original :which-key "original-size")
 "RET" '(image-toggle-animation :which-key "animation-toggle")
 "." '(image-previous-frame :which-key "animation-prev-frame")
 ";" '(image-next-frame  :which-key "animation-next-frame")

 ;; animations, gifs...

 "a"   '(:ignore t :which-key "animations")
 "aa" '(image-toggle-animation :which-key "animation-RET-toggle")
 "a." '(image-previous-frame :which-key "animation-prev-frame")
 "a;" '(image-next-frame  :which-key "animation-next-frame")
 "a0" '(image-reset-speed :which-key "animation-reset-speed")
 "ar" '(image-reverse-speed  :which-key "animation-reverse-speed")
 "aF" '(image-goto-frame :which-key "animation-goto-frame")
 "a{" '(image-decrease-speed  :which-key "animation-decrease-speed")
 "a}" '(image-increase-speed :which-key "animation-increase-speed"))
