;;; eide-browsing-theme.el --- Emacs-IDE: Browsing theme

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

(deftheme eide-browsing
  "Emacs-IDE override of Emacs default settings for browsing")

(custom-theme-set-variables
 'eide-browsing
 '(tags-case-fold-search nil))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-browsing)

;;; eide-browsing-theme.el ends here
