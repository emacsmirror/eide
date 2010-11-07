;;; eide-edit.el --- Emacs-IDE, edit

;; Copyright (C) 2008-2010 Cédric Marie

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

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Get current buffer status (REF, NEW or not edited).
;;
;; return : buffer status ("nofile", "ref", "new" or "").
;; ----------------------------------------------------------------------------
(defun eide-edit-get-buffer-status ()
  (if (not (file-exists-p buffer-file-name))
    "nofile"
    (if (file-exists-p (concat buffer-file-name ".ref"))
      "new"
      (if (file-exists-p (concat buffer-file-name ".new"))
        "ref"
        ""))))

;; ----------------------------------------------------------------------------
;; Update buffers edit status (REF, NEW or not edited).
;;
;; input  : p-files-list : list of files to update (overrides
;;              eide-menu-files-list)
;;          eide-menu-files-list : list of opened files.
;; ----------------------------------------------------------------------------
(defun eide-edit-update-files-status (&optional p-files-list)
  (save-excursion
    (let ((l-files-list nil))
      (if p-files-list
        (setq l-files-list p-files-list)
        (setq l-files-list eide-menu-files-list))
      (dolist (l-buffer-name l-files-list)
        (set-buffer l-buffer-name)
        (make-local-variable 'eide-menu-local-edit-status)
        (setq eide-menu-local-edit-status (eide-edit-get-buffer-status))))))

;; ----------------------------------------------------------------------------
;; Set write permission for current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-set-rw ()
  (if buffer-read-only
    (progn
      (shell-command (concat "chmod +w \"" buffer-file-name "\""))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Unset write permission for current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-set-r ()
  (if (not buffer-read-only)
    (progn
      (shell-command (concat "chmod -w \"" buffer-file-name "\""))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Create ".ref" version of current file, and use ".new".
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-make-ref-file ()
  (if (string-equal eide-menu-local-edit-status "")
    (progn
      (shell-command (concat "mv \"" buffer-file-name "\" \"" buffer-file-name ".ref\" ; cp \"" buffer-file-name ".ref\" \"" buffer-file-name "\" ; chmod +w \"" buffer-file-name "\""))
      (revert-buffer))))

;;(setq nnn (file-modes buffer-file-name))
;;(setq mmm (logior (file-modes buffer-file-name) 128))) ; = "chmod +w"
;; file-name-sans-extension
;; TODO: utiliser les commandes lisp équivalentes
;; (shell-command (concat "mv " buffer-file-name " " buffer-file-name ".ref ; cp " buffer-file-name ".ref " buffer-file-name " ; chmod +w " buffer-file-name))
;; (rename-file buffer-file-name (concat buffer-file-name ".ref"))
;; (copy-file (concat buffer-file-name ".ref") buffer-file-name)
;; (set-file-modes (logior (file-modes buffer-file-name) 128)) ; = "chmod +w"

;; ----------------------------------------------------------------------------
;; Use ".ref" version of current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-use-ref-file ()
  (if (string-equal eide-menu-local-edit-status "new")
    (progn
      (shell-command (concat "mv \"" buffer-file-name "\" \"" buffer-file-name ".new\""))
      (shell-command (concat "mv \"" buffer-file-name ".ref\" \"" buffer-file-name "\""))
      (if eide-option-touch-files-when-using-flag
        (shell-command (concat "touch \"" buffer-file-name "\"")))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Use ".new" version of current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-use-new-file ()
  (if (string-equal eide-menu-local-edit-status "ref")
    (progn
      (shell-command (concat "mv \"" buffer-file-name "\" \"" buffer-file-name ".ref\""))
      (shell-command (concat "mv \"" buffer-file-name ".new\" \"" buffer-file-name "\""))
      (if eide-option-touch-files-when-using-flag
        (shell-command (concat "touch \"" buffer-file-name "\"")))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Discard ".new" version of current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-discard-new-file ()
  (if (string-equal eide-menu-local-edit-status "ref")
    (progn
      (shell-command (concat "rm -f \"" buffer-file-name ".new\""))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Restore ".ref" version of current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-restore-ref-file ()
  (if (string-equal eide-menu-local-edit-status "new")
    (progn
      (shell-command (concat "rm -f \"" buffer-file-name "\" ; mv \"" buffer-file-name ".ref\" \"" buffer-file-name "\""))
      (if eide-option-touch-files-when-using-flag
        (shell-command (concat "touch \"" buffer-file-name "\"")))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Discard ".ref" version of current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-discard-ref-file ()
  (if (string-equal eide-menu-local-edit-status "new")
    (progn
      (shell-command (concat "rm -f \"" buffer-file-name ".ref\""))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Untabify and indent the content of current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-untabify-and-indent ()
  (if (not buffer-read-only)
    (progn
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max) nil)
      (ad-deactivate 'save-buffer)
      (save-buffer)
      (ad-activate 'save-buffer))))

;; ----------------------------------------------------------------------------
;; Convert current file end of line from DOS to UNIX.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-dos-to-unix ()
  (if (not buffer-read-only)
    (progn
      (shell-command (concat "dos2unix \"" buffer-file-name "\""))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Convert current file end of line from UNIX to DOS.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-unix-to-dos ()
  (if (not buffer-read-only)
    (progn
      (shell-command (concat "unix2dos \"" buffer-file-name "\""))
      (revert-buffer))))

;; ----------------------------------------------------------------------------
;; Delete all trailing spaces in current file.
;; Called by eide-edit-action-on-file or eide-edit-action-on-directory.
;; ----------------------------------------------------------------------------
(defun eide-edit-delete-trailing-spaces ()
  (if (not buffer-read-only)
    (progn
      (delete-trailing-whitespace)
      (ad-deactivate 'save-buffer)
      (save-buffer)
      (ad-activate 'save-buffer))))

;; ----------------------------------------------------------------------------
;; Do an action on a file.
;;
;; input  : p-function : function to call when buffer is current.
;;          p-buffer-name : buffer name.
;;          p-confirmation-message : string for confirmation message, nil if
;;              confirmation is not required.
;; ----------------------------------------------------------------------------
(defun eide-edit-action-on-file (p-function p-buffer-name &optional p-confirmation-message)
  (if (or (not p-confirmation-message)
          (eide-popup-question-yes-or-no-p (concat "Do you really want to " p-confirmation-message " ?")))
    (progn
      (eide-menu-buffer-update-start p-buffer-name)
      (save-excursion
        (set-buffer p-buffer-name)
        (funcall p-function))
      (eide-menu-buffer-update-stop p-buffer-name))))

;; ----------------------------------------------------------------------------
;; Do an action on all opened files in a directory.
;;
;; input  : p-function : function to call when buffer is current.
;;          p-directory-name : directory name.
;;          p-confirmation-message : string for confirmation message, nil if
;;              confirmation is not required.
;;          eide-menu-files-list : list of opened files.
;; ----------------------------------------------------------------------------
(defun eide-edit-action-on-directory (p-function p-directory-name &optional p-confirmation-message)
  (if (or (not p-confirmation-message)
          (eide-popup-question-yes-or-no-p (concat "Do you really want to " p-confirmation-message " ?")))
    (progn
      (eide-menu-directory-update-start p-directory-name)
      (dolist (l-buffer-name eide-menu-files-list)
        (if (eide-menu-is-file-in-directory-p l-buffer-name p-directory-name)
          (save-excursion
            (set-buffer l-buffer-name)
            (if (file-exists-p buffer-file-name)
              (funcall p-function)))))
      (eide-menu-directory-update-stop p-directory-name))))

;;; eide-edit.el ends here
