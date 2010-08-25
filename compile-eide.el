;;; compile-eide.el --- Compilation of Emacs-IDE

;; Copyright (C) 2005-2009 Cédric Marie

;; This program is free software ; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation ; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY ; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program ; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code:

;; Configuration
(mouse-wheel-mode 1)
(set-scroll-bar-mode 'right)

;; Load path
(add-to-list 'load-path default-directory)

;; Delete all .elc files
(shell-command (concat "cd " default-directory " ; rm -f *.elc"))

(byte-compile-file "eide-compare.el")
(byte-compile-file "eide-config.el")
(byte-compile-file "eide-edit.el")
(byte-compile-file "eide-help.el")
(byte-compile-file "eide-keys.el")
(byte-compile-file "eide-menu.el")
(byte-compile-file "eide-popup.el")
(byte-compile-file "eide-project.el")
(byte-compile-file "eide-search.el")
(byte-compile-file "eide-toolbar.el")
(byte-compile-file "eide-windows.el")
(byte-compile-file "eide.el")

(if (file-exists-p (concat default-directory ".do_not_exit_after_compilation"))
  (progn
    ;; Full window for compilation log
    (select-window (next-window))
    (delete-other-windows)
    (shell-command (concat "rm -f " default-directory ".do_not_exit_after_compilation")))
  (kill-emacs))

;;; compile-eide.el ends here
