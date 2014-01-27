(deftheme eide-coding
  "Emacs-IDE override of Emacs default settings for coding")

(custom-theme-set-variables
 'eide-coding
 '(indent-tabs-mode nil)
 '(tab-width 4)
 '(c-basic-offset 2)
 '(c-offsets-alist (quote ((substatement-open . 0) (case-label . +))))
 '(sh-basic-offset 2)
 '(perl-indent-level 2))

(provide-theme 'eide-coding)
