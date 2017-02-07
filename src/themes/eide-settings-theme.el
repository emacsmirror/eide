;;; eide-settings-theme.el --- Emacs-IDE: Settings theme

;; Copyright (C) 2014-2017 Cédric Marie

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
 '(compilation-scroll-output t)
 '(mouse-wheel-progressive-speed nil))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-settings)

;;; eide-settings-theme.el ends here
