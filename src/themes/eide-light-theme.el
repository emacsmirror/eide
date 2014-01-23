(deftheme eide-light
  "Emacs-IDE light color theme")

(custom-theme-set-faces
 'eide-light
 '(default ((t (:background "old lace" :foreground "black" :height 140))))
 '(region ((t (:background "bisque")))) ; :foreground "black"))))
 '(font-lock-builtin-face ((t (:background "yellow" :foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "light slate blue"))))
 '(font-lock-constant-face ((t (:background "misty rose" :foreground "deep pink"))))
 '(font-lock-function-name-face ((t (:foreground "red" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "brown" :weight bold))))
 '(font-lock-string-face ((t (:background "white" :foreground "black"))))
 '(font-lock-type-face ((t (:foreground "sea green"))))
 '(font-lock-variable-name-face ((t (:foreground "orange red"))))
 '(fringe ((t (:background "old lace"))))
 '(mode-line ((t (:background "wheat")))))

(provide-theme 'eide-light)
