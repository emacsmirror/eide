(deftheme eide-display
  "Emacs-IDE override of Emacs default settings for display")

(custom-theme-set-variables
 'eide-display
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode (quote right)
 '(show-trailing-whitespace t)))

(provide-theme 'eide-display)
