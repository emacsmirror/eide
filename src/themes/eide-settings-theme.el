(deftheme eide-settings
  "Emacs-IDE override of Emacs default settings")

(custom-theme-set-variables
 'eide-settings
 '(inhibit-startup-message t)
 '(make-backup-files nil)
 '(large-file-warning-threshold nil)
 '(revert-without-query (quote (".*")))
 '(tags-revert-without-query t)
 '(scroll-conservatively 1)
 '(scroll-preserve-screen-position t)
 '(mouse-wheel-progressive-speed nil))

(provide-theme 'eide-settings)
