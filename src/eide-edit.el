;;; eide-edit.el --- Emacs-IDE: Clean and edit files (REF/NEW)

;; Copyright (C) 2008-2015 Cédric Marie

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

(provide 'eide-edit)

(require 'eide-popup)

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-edit-get-buffer-status ()
  "Get current buffer status (\"nofile\", \"ref\", \"new\" or \"\")."
  (if (not (file-exists-p buffer-file-name))
      "nofile"
    (if (file-exists-p (concat buffer-file-name ".ref"))
        "new"
      (if (file-exists-p (concat buffer-file-name ".new"))
          "ref"
        ""))))

(defun eide-edit-update-files-status (&optional p-files-list)
  "Update buffers edit status (\"nofile\", \"ref\", \"new\" or \"\").
Argument:
- p-files-list (optional): list of files to update (overrides
  eide-menu-files-list)."
  (save-current-buffer
    (let ((l-files-list nil))
      (if p-files-list
          (setq l-files-list p-files-list)
        (setq l-files-list eide-menu-files-list))
      (dolist (l-buffer-name l-files-list)
        (set-buffer l-buffer-name)
        (make-local-variable 'eide-menu-local-edit-status)
        (setq eide-menu-local-edit-status (eide-edit-get-buffer-status))))))

(defun eide-edit-set-rw ()
  "Set write permission for current file."
  (when buffer-read-only
    ;; chmod +w (add -w-------, i.e. w for user)
    (set-file-modes buffer-file-name (logior (file-modes buffer-file-name) 128))
    (revert-buffer)))

(defun eide-edit-set-r ()
  "Unset write permission for current file."
  (unless buffer-read-only
    ;; chmod -w (remove -w-------, i.e. w for user)
    (set-file-modes buffer-file-name (logxor (file-modes buffer-file-name) 128))
    (revert-buffer)))

(defun eide-edit-make-ref-file ()
  "Create \".ref\" version of current file, use \".new\", and set write
permission for it."
  (when (string-equal eide-menu-local-edit-status "")
    (let ((l-ref-file (concat buffer-file-name ".ref")))
      (rename-file buffer-file-name l-ref-file)
      (copy-file l-ref-file buffer-file-name))
    ;; chmod +w (add -w-------, i.e. w for user)
    (set-file-modes buffer-file-name (logior (file-modes buffer-file-name) 128))
    (revert-buffer)))

(defun eide-edit-use-ref-file ()
  "Use \".ref\" version of current file."
  (when (string-equal eide-menu-local-edit-status "new")
    (rename-file buffer-file-name (concat buffer-file-name ".new"))
    (rename-file (concat buffer-file-name ".ref") buffer-file-name)
    ;; Update the modification time of the file (for it to be recompiled)
    (set-file-times buffer-file-name)
    (revert-buffer)))

(defun eide-edit-use-new-file ()
  "Use \".new\" version of current file."
  (when (string-equal eide-menu-local-edit-status "ref")
    (rename-file buffer-file-name (concat buffer-file-name ".ref"))
    (rename-file (concat buffer-file-name ".new") buffer-file-name)
    ;; Update the modification time of the file (for it to be recompiled)
    (set-file-times buffer-file-name)
    (revert-buffer)))

(defun eide-edit-discard-new-file ()
  "Discard \".new\" version of current file."
  (when (string-equal eide-menu-local-edit-status "ref")
    (delete-file (concat buffer-file-name ".new") nil)))

(defun eide-edit-restore-ref-file ()
  "Restore \".ref\" version of current file."
  (when (string-equal eide-menu-local-edit-status "new")
    (delete-file buffer-file-name)
    (rename-file (concat buffer-file-name ".ref") buffer-file-name)
    ;; Update the modification time of the file (for it to be recompiled)
    (set-file-times buffer-file-name)
    (revert-buffer)))

(defun eide-edit-discard-ref-file ()
  "Discard \".ref\" version of current file."
  (when (string-equal eide-menu-local-edit-status "new")
    (delete-file (concat buffer-file-name ".ref"))))

(defun eide-edit-untabify-and-indent ()
  "Untabify and indent the content of current file."
  (unless buffer-read-only
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max) nil)
    (save-buffer)))

(defun eide-edit-delete-trailing-spaces ()
  "Delete all trailing spaces in current file."
  (unless buffer-read-only
    (delete-trailing-whitespace)
    (save-buffer)))

(defun eide-edit-action-on-file (p-function p-buffer-name &optional p-confirmation-message)
  "Do an action on a file.
Arguments:
- p-function: function to call (once the buffer is current).
- p-buffer-name: buffer name.
- p-confirmation-message (optional): string for confirmation message, nil if
  confirmation is not required."
  (when (or (not p-confirmation-message)
            (y-or-n-p (concat "Do you really want to " p-confirmation-message "?")))
    (eide-menu-buffer-update-start p-buffer-name)
    (with-current-buffer p-buffer-name
      (funcall p-function))
    (eide-menu-buffer-update-stop p-buffer-name)))

(defun eide-edit-action-on-directory (p-function p-directory-name &optional p-confirmation-message)
  "Do an action on all open files in a directory.
Arguments:
- p-function: function to call (once the buffer is current).
- p-directory-name: directory name.
- p-confirmation-message (optional): string for confirmation message, nil if
  confirmation is not required."
  (when (or (not p-confirmation-message)
            (y-or-n-p (concat "Do you really want to " p-confirmation-message "?")))
    (eide-menu-directory-update-start p-directory-name)
    (dolist (l-buffer-name eide-menu-files-list)
      (when (eide-menu-is-file-in-directory-p l-buffer-name p-directory-name)
        (with-current-buffer l-buffer-name
          (when (file-exists-p buffer-file-name)
            (funcall p-function)))))
    (eide-menu-directory-update-stop p-directory-name)))

;;; eide-edit.el ends here
