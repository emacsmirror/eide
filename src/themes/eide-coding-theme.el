;;; eide-coding-theme.el --- Emacs-IDE: Coding theme

;; Copyright (C) 2014 Cédric Marie

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

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

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-coding)

;;; eide-coding-theme.el ends here
