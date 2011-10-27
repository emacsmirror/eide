;;; eide-git.el --- Emacs-IDE, git

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

(provide 'eide-git)

(defvar eide-git-diff-full-command nil)

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Check if current buffer is modified compared to git repository.
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-git-is-current-buffer-modified-p ()
  (if eide-config-show-git-status-flag
    (if (file-exists-p buffer-file-name)
      (not (string-equal (shell-command-to-string (concat "git status " buffer-file-name " | grep modified")) ""))
      nil)
    nil))

;; ----------------------------------------------------------------------------
;; Update buffers git status (modified or not).
;;
;; input  : p-files-list : list of files to update (overrides
;;              eide-menu-files-list).
;;          eide-menu-files-list : list of opened files.
;; ----------------------------------------------------------------------------
(defun eide-git-update-files-status (&optional p-files-list)
  (if eide-config-show-git-status-flag
    (save-excursion
      (let ((l-files-list nil))
        (if p-files-list
          (setq l-files-list p-files-list)
          (setq l-files-list eide-menu-files-list))
        (dolist (l-buffer-name l-files-list)
          (set-buffer l-buffer-name)
          (make-local-variable 'eide-menu-local-git-modified-status-flag)
          (setq eide-menu-local-git-modified-status-flag (eide-git-is-current-buffer-modified-p)))))))

;; ----------------------------------------------------------------------------
;; Set git diff command.
;;
;; input  : p-cmd : diff program.
;; output : eide-git-diff-full-command : git diff command.
;; ----------------------------------------------------------------------------
(defun eide-git-set-diff-command (p-cmd)
  (if (string-equal p-cmd "")
    (setq eide-git-diff-full-command "git diff ")
    (setq eide-git-diff-full-command (concat "git difftool -y --extcmd=" p-cmd " "))))

;; ----------------------------------------------------------------------------
;; Execute "git diff" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-git-diff ()
  (if (and eide-config-show-git-status-flag eide-menu-local-git-modified-status-flag)
    (shell-command (concat eide-git-diff-full-command buffer-file-name))))

;; ----------------------------------------------------------------------------
;; Execute "git diff" on a directory.
;;
;; input  : p-directory-name : directory name.
;;          p-files-list-string : string containing files list.
;; ----------------------------------------------------------------------------
(defun eide-git-diff-files-in-directory (p-directory-name p-files-list-string)
  (if eide-config-show-git-status-flag
    (let ((l-full-directory-name nil))
      (if (string-match "^/" p-directory-name)
        (setq l-full-directory-name p-directory-name)
        (setq l-full-directory-name (concat eide-root-directory p-directory-name)))
      (shell-command (concat "cd " l-full-directory-name " && " eide-git-diff-full-command p-files-list-string)))))

;; ----------------------------------------------------------------------------
;; Execute "git checkout" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-git-checkout ()
  (if (and eide-config-show-git-status-flag eide-menu-local-git-modified-status-flag)
    (progn
      (shell-command (concat "git checkout " buffer-file-name))
      (revert-buffer))))

;;; eide-git.el ends here
