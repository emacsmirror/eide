;;; eide-display-theme.el --- Emacs-IDE: Display theme

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

(deftheme eide-display
  "Emacs-IDE override of Emacs default settings for display")

(custom-theme-set-variables
 'eide-display
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode (quote right))
 '(show-trailing-whitespace t)
 '(show-paren-mode t)
 '(line-number-mode t)
 '(column-number-mode t)
 '(which-function-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(gdb-many-windows t))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-display)

;;; eide-display-theme.el ends here
