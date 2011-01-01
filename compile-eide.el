;;; compile-eide.el --- Compilation of Emacs-IDE

;; Copyright (C) 2008-2011 Cédric Marie

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

;; Configuration
(mouse-wheel-mode 1)
(set-scroll-bar-mode 'right)

;; Load path
(add-to-list 'load-path (concat default-directory "src"))

(byte-compile-file "src/eide-compare.el")
(byte-compile-file "src/eide-config.el")
(byte-compile-file "src/eide-edit.el")
(byte-compile-file "src/eide-help.el")
(byte-compile-file "src/eide-keys.el")
(byte-compile-file "src/eide-menu.el")
(byte-compile-file "src/eide-popup.el")
(byte-compile-file "src/eide-project.el")
(byte-compile-file "src/eide-search.el")
(byte-compile-file "src/eide-svn.el")
(byte-compile-file "src/eide-windows.el")
(byte-compile-file "src/eide.el")

(kill-emacs)

;;; compile-eide.el ends here
