;;; eide-vc.el --- Emacs-IDE, version control (svn and git)

;; Copyright (C) 2008-2012 Cédric Marie

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

(provide 'eide-vc)

(require 'vc)

(defvar eide-vc-svn-diff-full-command nil)
(defvar eide-vc-git-diff-full-command nil)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Call vc-diff on current buffer with specific backend.
;;
;; input  : p-backend : vc backend.
;; ----------------------------------------------------------------------------
(defun eide-i-vc-diff (p-backend)
  (let ((l-vc-backend (vc-backend buffer-file-name)))
    ;; Temporary switch to specific backend (in case the file is under several version control systems)
    (vc-switch-backend buffer-file-name p-backend)
    (save-excursion
      (vc-diff nil))
    ;; Switch back to previous backend
    (vc-switch-backend buffer-file-name l-vc-backend)))

;; ----------------------------------------------------------------------------
;; Call vc-annotate on current buffer with specific backend.
;;
;; input  : p-backend : vc backend.
;; ----------------------------------------------------------------------------
(defun eide-i-vc-blame (p-backend)
  (let ((l-vc-backend (vc-backend buffer-file-name)))
    ;; Temporary switch to specific backend (in case the file is under several version control systems)
    (vc-switch-backend buffer-file-name p-backend)
    (save-excursion
      (vc-annotate buffer-file-name (vc-working-revision buffer-file-name)))
    ;; Switch back to previous backend
    (vc-switch-backend buffer-file-name l-vc-backend)))

;; ----------------------------------------------------------------------------
;; Call vc-revert-file on current buffer with specific backend.
;;
;; input  : p-backend : vc backend.
;; ----------------------------------------------------------------------------
(defun eide-i-vc-revert (p-backend)
  (let ((l-vc-backend (vc-backend buffer-file-name)))
    ;; Temporary switch to specific backend (in case the file is under several version control systems)
    (vc-switch-backend buffer-file-name p-backend)
    (save-excursion
      (vc-revert-file buffer-file-name))
    ;; Switch back to previous backend
    (vc-switch-backend buffer-file-name l-vc-backend)))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Update current buffer status (modified or not compared to vc repositories).
;; ----------------------------------------------------------------------------
(defun eide-vc-update-current-buffer-status ()
  (if eide-config-show-svn-status-flag
    (progn
      (make-local-variable 'eide-menu-local-svn-modified-status-flag)
      (setq eide-menu-local-svn-modified-status-flag nil)))
  (if eide-config-show-git-status-flag
    (progn
      (make-local-variable 'eide-menu-local-git-modified-status-flag)
      (setq eide-menu-local-git-modified-status-flag nil)))
  (if (file-exists-p buffer-file-name)
    (let ((l-vc-backend (vc-backend buffer-file-name)))
      (if (and eide-config-show-svn-status-flag (vc-svn-registered buffer-file-name))
        (progn
          ;; Temporary switch to SVN backend (in case the file is under several version control systems)
          (vc-switch-backend buffer-file-name 'SVN)
          ;; NB: vc-state doesn't use selected backend, vc-workfile-unchanged-p does!
          (setq eide-menu-local-svn-modified-status-flag (not (vc-workfile-unchanged-p buffer-file-name)))))
      (if (and eide-config-show-git-status-flag (vc-git-registered buffer-file-name))
        (progn
          ;; Temporary switch to Git backend (in case the file is under several version control systems)
          (vc-switch-backend buffer-file-name 'Git)
          ;; NB: vc-state doesn't use selected backend, vc-workfile-unchanged-p does!
          (setq eide-menu-local-git-modified-status-flag (not (vc-workfile-unchanged-p buffer-file-name)))))
      ;; Switch back to previous backend
      (vc-switch-backend buffer-file-name l-vc-backend))))

;; ----------------------------------------------------------------------------
;; Update buffers vc status (modified or not).
;;
;; input  : p-files-list : list of files to update (overrides
;;              eide-menu-files-list).
;;          eide-menu-files-list : list of open files.
;; ----------------------------------------------------------------------------
(defun eide-vc-update-files-status (&optional p-files-list)
  (if eide-config-show-svn-status-flag
    (save-excursion
      (let ((l-files-list nil))
        (if p-files-list
          (setq l-files-list p-files-list)
          (setq l-files-list eide-menu-files-list))
        (dolist (l-buffer-name l-files-list)
          (set-buffer l-buffer-name)
          (eide-vc-update-current-buffer-status))))))

;; ----------------------------------------------------------------------------
;; Set svn/git diff commands.
;;
;; input  : p-cmd : diff program.
;; output : eide-vc-svn-diff-full-command : svn diff command.
;;          eide-vc-git-diff-full-command : git diff command.
;; ----------------------------------------------------------------------------
(defun eide-vc-set-diff-command (p-cmd)
  (if (string-equal p-cmd "")
    (progn
      (setq eide-vc-svn-diff-full-command nil)
      (setq eide-vc-git-diff-full-command nil))
    (progn
      (setq eide-vc-svn-diff-full-command (concat "svn diff --diff-cmd=" p-cmd " "))
      (setq eide-vc-git-diff-full-command (concat "git difftool -y --extcmd=" p-cmd " ")))))

;; ----------------------------------------------------------------------------
;; Execute "svn diff" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-svn-diff ()
  (if (and eide-config-show-svn-status-flag eide-menu-local-svn-modified-status-flag)
    (if eide-vc-svn-diff-full-command
      (shell-command (concat eide-vc-svn-diff-full-command buffer-file-name))
      (eide-i-vc-diff 'SVN))))

;; ----------------------------------------------------------------------------
;; Execute "svn diff" on a directory.
;;
;; input  : p-directory-name : directory name.
;;          p-files-list-string : string containing files list.
;; ----------------------------------------------------------------------------
(defun eide-svn-diff-files-in-directory (p-directory-name p-files-list-string)
  (if eide-config-show-svn-status-flag
    (let ((l-full-directory-name nil))
      (if (string-match "^/" p-directory-name)
        (setq l-full-directory-name p-directory-name)
        (setq l-full-directory-name (concat eide-root-directory p-directory-name)))
      (if eide-vc-svn-diff-full-command
        (shell-command (concat "cd " l-full-directory-name " && " eide-vc-svn-diff-full-command p-files-list-string))
        (shell-command (concat "cd " l-full-directory-name " && svn diff " p-files-list-string))))))

;; ----------------------------------------------------------------------------
;; Execute "svn blame" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-svn-blame ()
  (if eide-config-show-svn-status-flag
    (eide-i-vc-blame 'SVN)))

;; ----------------------------------------------------------------------------
;; Execute "svn revert" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-svn-revert ()
  (if (and eide-config-show-svn-status-flag eide-menu-local-svn-modified-status-flag)
    (eide-i-vc-revert 'SVN)))

;; ----------------------------------------------------------------------------
;; Execute "git diff" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-git-diff ()
  (if (and eide-config-show-git-status-flag eide-menu-local-git-modified-status-flag)
    (if eide-vc-git-diff-full-command
      (shell-command (concat eide-vc-git-diff-full-command buffer-file-name))
      (eide-i-vc-diff 'Git))))

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
      (if eide-vc-git-diff-full-command
        (shell-command (concat "cd " l-full-directory-name " && " eide-vc-git-diff-full-command p-files-list-string))
        (shell-command (concat "cd " l-full-directory-name " && git diff " p-files-list-string))))))

;; ----------------------------------------------------------------------------
;; Execute "git blame" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-git-blame ()
  (if eide-config-show-git-status-flag
    (eide-i-vc-blame 'Git)))

;; ----------------------------------------------------------------------------
;; Execute "git checkout" on current buffer.
;; ----------------------------------------------------------------------------
(defun eide-git-checkout ()
  (if (and eide-config-show-git-status-flag eide-menu-local-git-modified-status-flag)
    (eide-i-vc-revert 'Git)))

;;; eide-vc.el ends here
