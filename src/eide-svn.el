;;; eide-svn.el --- Emacs-IDE, svn

;; Copyright (C) 2005-2010 Cédric Marie

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

(provide 'eide-svn)

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Check if current buffer is modified compared to svn repository.
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-svn-is-current-buffer-modified-p ()
  (if eide-config-show-svn-status-flag
    (not (string-equal (shell-command-to-string (concat "svn st -q " buffer-file-name)) ""))
    nil))

;; ----------------------------------------------------------------------------
;; Update buffers svn status (modified or not).
;;
;; input  : p-files-list : list of files to update (overrides
;;              eide-menu-files-list)
;;          eide-menu-files-list : list of opened files.
;; ----------------------------------------------------------------------------
(defun eide-svn-update-files-status (&optional p-files-list)
  (if eide-config-show-svn-status-flag
    (save-excursion
      (let ((l-files-list nil))
        (if p-files-list
          (setq l-files-list p-files-list)
          (setq l-files-list eide-menu-files-list))
        (dolist (l-buffer-name l-files-list)
          (set-buffer l-buffer-name)
          (make-local-variable 'eide-menu-local-svn-modified-status-flag)
          (setq eide-menu-local-svn-modified-status-flag (eide-svn-is-current-buffer-modified-p)))))))

;; ----------------------------------------------------------------------------
;; Execute "svn diff" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-svn-diff ()
  (if (and eide-config-show-svn-status-flag eide-menu-local-svn-modified-status-flag)
    (shell-command (concat "svn diff " buffer-file-name))))

;; ----------------------------------------------------------------------------
;; Execute "svn revert" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-svn-revert ()
  (if (and eide-config-show-svn-status-flag eide-menu-local-svn-modified-status-flag)
    (progn
      (shell-command (concat "svn revert " buffer-file-name))
      (revert-buffer))))

;;; eide-svn.el ends here
