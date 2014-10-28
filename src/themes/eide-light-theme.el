;;; eide-light-theme.el --- Emacs-IDE: Light color theme

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

(deftheme eide-light
  "Emacs-IDE light color theme")

(custom-theme-set-faces
 'eide-light
 '(default ((t (:background "old lace" :foreground "black"))))
 '(region ((t (:background "bisque"))))
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

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-light)

;;; eide-light-theme.el ends here
