;;; eide-edit.el --- Emacs-IDE, edit

;; Copyright (C) 2005-2008 Cédric Marie

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

(provide 'eide-edit)


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Get current buffer status
;;
;; return : buffer status ("ref", "new" or "")
;; ----------------------------------------------------------------------------
(defun eide-edit-get-buffer-status ()
;  (if (equal (shell-command (concat "[[ -e " buffer-file-name ".ref ]]")) 0)
  (if (file-exists-p (concat buffer-file-name ".ref"))
    (setq my-edit-ref-exist t)
    (setq my-edit-ref-exist nil)
  )
;  (if (equal (shell-command (concat "[[ -e " buffer-file-name ".new ]]")) 0)
  (if (file-exists-p (concat buffer-file-name ".new"))
    (setq my-edit-new-exist t)
    (setq my-edit-new-exist nil)
  )
  (if my-edit-ref-exist
    (if my-edit-new-exist
      nil ; ce cas ne doit pas arriver
      "new")
    (if my-edit-new-exist
      "ref"
      "")))

;; not called !
;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-edit-dos-to-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match ""))
  (message "Buffer has been converted to Unix format (end of line)."))

; not called !
;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-edit-unix-to-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n"))
  (message "Buffer has been converted to Dos format (end of line)."))

;; ----------------------------------------------------------------------------
;; Rename selected file (prompt for new name)
;; ----------------------------------------------------------------------------
(defun eide-edit-rename (this-buffer)
  (interactive)
  (setq this-new-buffer (read-string (concat "New name for " this-buffer " : ")))
  (if (string-equal this-new-buffer "")
    (message "Rename command cancelled")
    (progn
      (save-excursion
        (set-buffer this-buffer)
        (shell-command (concat "mv " this-buffer " " this-new-buffer)) ; TODO : file name !
        (find-file this-new-buffer)
        (kill-buffer this-buffer))
      (if (string-equal eide-current-buffer my-buffer)
        (setq eide-current-buffer this-new-buffer)))))
;; TODO : update menu

;; ----------------------------------------------------------------------------
;; Copy selected file (prompt for new name)
;; ----------------------------------------------------------------------------
(defun eide-edit-copy (this-buffer)
  (interactive)
  (setq this-new-buffer (read-string (concat "Name for copy of " this-buffer " : ")))
  (if (string-equal this-new-buffer "")
    (message "Copy command cancelled")
    (progn
      (save-excursion
        (set-buffer this-buffer)
        (shell-command (concat "cp " this-buffer " " this-new-buffer)) ; TODO : file name !
        (find-file this-new-buffer)))))
;; TODO : update menu

;; ----------------------------------------------------------------------------
;; Delete selected file
;; ----------------------------------------------------------------------------
(defun eide-edit-delete (this-buffer)
  (interactive)
  (if (eide-popup-question-yes-or-no-p (concat "Do you really want to delete " this-buffer " ?"))
    (progn
      (save-excursion
        (set-buffer this-buffer)
        (shell-command (concat "rm -f " buffer-file-name)))
      (eide-menu-file-close this-buffer))))

;; ----------------------------------------------------------------------------
;; View clean version of selected file
;; ----------------------------------------------------------------------------
(defun eide-edit-view-clean-buffer (this-buffer)
  (interactive)
;  (save-excursion
;    (set-buffer my-buffer)
;    (eide-display-view-clean-buffer))

  ;; TODO : à mettre en commun avec open-file
  (eide-windows-select-window-file nil)
  (switch-to-buffer this-buffer)
  (eide-display-view-clean-buffer))

;; ----------------------------------------------------------------------------
;; Set write permission for current buffer
;; ----------------------------------------------------------------------------
(defun eide-edit-set-rw (this-buffer)
  (interactive)
  (eide-menu-buffer-update-start this-buffer)
  (save-excursion
    (set-buffer this-buffer)
    (shell-command (concat "chmod +w " buffer-file-name))
    (revert-buffer))
  (message "File is now 'Read/Write'")
  (eide-menu-buffer-update-stop this-buffer))

;; ----------------------------------------------------------------------------
;; Unset write permission for current buffer
;; ----------------------------------------------------------------------------
(defun eide-edit-set-r (this-buffer)
  (interactive)
  (eide-menu-buffer-update-start this-buffer)
  (save-excursion
    (set-buffer this-buffer)
    (shell-command (concat "chmod -w " buffer-file-name))
    (revert-buffer))
  (message "File is now 'Read only'")
  (eide-menu-buffer-update-stop this-buffer))

;; ----------------------------------------------------------------------------
;; Create and use ".ref" version of selected file
;; ----------------------------------------------------------------------------
(defun eide-edit-make-ref-file (this-buffer)
  (interactive)
  (eide-menu-buffer-update-start this-buffer)
  (save-excursion
    (set-buffer this-buffer)
    (shell-command (concat "mv " buffer-file-name " " buffer-file-name ".ref ; cp " buffer-file-name ".ref " buffer-file-name " ; chmod +w " buffer-file-name))
    (revert-buffer))
  (message "File is now 'Read/Write' (original file was saved as .ref)")
  (eide-menu-buffer-update-stop this-buffer))

;  (setq nnn (file-modes buffer-file-name))
;  (setq mmm (logior (file-modes buffer-file-name) 128))) ; = "chmod +w"
; file-name-sans-extension
    ;; TODO : utiliser les commandes lisp équivalentes
;    (shell-command (concat "mv " buffer-file-name " " buffer-file-name ".ref ; cp " buffer-file-name ".ref " buffer-file-name " ; chmod +w " buffer-file-name))
;;  (rename-file buffer-file-name (concat buffer-file-name ".ref"))
;  (copy-file (concat buffer-file-name ".ref") buffer-file-name)
;  (set-file-modes (logior (file-modes buffer-file-name) 128)) ; = "chmod +w"

;; ----------------------------------------------------------------------------
;; Use ".ref" version of selected file
;; ----------------------------------------------------------------------------
(defun eide-edit-use-ref-file (this-buffer)
  (interactive)
  (eide-menu-buffer-update-start this-buffer)
  (save-excursion
    (set-buffer this-buffer)
    (shell-command (concat "mv " buffer-file-name " " buffer-file-name ".new"))
    (shell-command (concat "mv " buffer-file-name ".ref " buffer-file-name))
    (if eide-option-touch-files-when-using-flag
      (shell-command (concat "touch " buffer-file-name)))
    (revert-buffer))
  (message "Use original buffer (modified buffer = .new)")
  (eide-menu-buffer-update-stop this-buffer))

;; ----------------------------------------------------------------------------
;; Use ".new" version of selected file
;; ----------------------------------------------------------------------------
(defun eide-edit-use-new-file (this-buffer)
  (interactive)
  (eide-menu-buffer-update-start this-buffer)
  (save-excursion
    (set-buffer this-buffer)
    (shell-command (concat "mv " buffer-file-name " " buffer-file-name ".ref"))
    (shell-command (concat "mv " buffer-file-name ".new " buffer-file-name))
    (if eide-option-touch-files-when-using-flag
      (shell-command (concat "touch " buffer-file-name)))
    (revert-buffer))
  (message "Use modified buffer (original buffer = .ref)")
  (eide-menu-buffer-update-stop this-buffer))

;; ----------------------------------------------------------------------------
;; Restore ".ref" version of selected file, when ".ref" is current version
;; (delete ".new" version) and update "menu" buffer
;; ----------------------------------------------------------------------------
(defun eide-edit-restore-ref-file-using-ref (this-buffer)
  (interactive)
  (if (eide-popup-question-yes-or-no-p "Do you really want to restore REF file (modifications in NEW file will be lost) ?")
    (progn
      (eide-menu-buffer-update-start this-buffer)
      (save-excursion
        (set-buffer this-buffer)
        (shell-command (concat "rm -f " buffer-file-name ".new"))
        (revert-buffer))
      (message "Original buffer (.ref) has been restored (modifications are lost)")
      (eide-menu-buffer-update-stop this-buffer))))

;; ----------------------------------------------------------------------------
;; Restore ".ref" version of selected file, when ".new" is current version
;; (switch to ".ref" version and delete ".new" version) and update "menu"
;; buffer
;; ----------------------------------------------------------------------------
(defun eide-edit-restore-ref-file-using-new (this-buffer)
  (interactive)
  (if (eide-popup-question-yes-or-no-p "Do you really want to restore REF file (modifications in NEW file will be lost) ?")
    (progn
      (eide-menu-buffer-update-start this-buffer)
      (save-excursion
        (set-buffer this-buffer)
        (shell-command (concat "rm -f " buffer-file-name " ; mv " buffer-file-name ".ref " buffer-file-name))
        (if eide-option-touch-files-when-using-flag
          (shell-command (concat "touch " buffer-file-name)))
        (revert-buffer))
      (eide-menu-buffer-update-stop this-buffer))))

;; ----------------------------------------------------------------------------
;; Discard ".ref" version of selected file, when ".new" is current version
;; (delete ".ref" version) and update "menu" buffer
;; ----------------------------------------------------------------------------
(defun eide-edit-discard-ref-file-using-new (this-buffer)
  (interactive)
  (if (eide-popup-question-yes-or-no-p "Do you really want to discard REF file (NEW file will become future reference) ?")
    (progn
      (eide-menu-buffer-update-start this-buffer)
      (save-excursion
        (set-buffer this-buffer)
        (shell-command (concat "rm -f " buffer-file-name ".ref"))
        (revert-buffer))
      (eide-menu-buffer-update-stop this-buffer))))

; not called !
;; ----------------------------------------------------------------------------
;; Use ".ref" version of all files
;; ----------------------------------------------------------------------------
(defun eide-edit-use-ref-file-all ()
  (interactive)
  (save-excursion
    (if eide-menu-source-file-list
      (dolist (this-buffer eide-menu-source-file-list)
        (set-buffer this-buffer-name)
        (if (string-equal (eide-edit-get-buffer-status) "new")
          (eide-popup-edit-use-ref-file) )))
    (if eide-menu-header-file-list
      (dolist (this-buffer eide-menu-header-file-list)
        (set-buffer this-buffer-name)
        (if (string-equal (eide-edit-get-buffer-status) "new")
          (eide-popup-edit-use-ref-file) )))
    (if eide-menu-other-file-list
      (dolist (this-buffer eide-menu-other-file-list)
        (set-buffer this-buffer-name)
        (if (string-equal (eide-edit-get-buffer-status) "new")
          (eide-popup-edit-use-ref-file))))))

; not called !
;; ----------------------------------------------------------------------------
;; Use ".new" version of all files
;; ----------------------------------------------------------------------------
(defun eide-edit-use-new-file-all ()
  (interactive)
  (save-excursion
    (if eide-menu-source-file-list
      (dolist (this-buffer eide-menu-source-file-list)
        (set-buffer this-buffer-name)
        (if (string-equal (eide-edit-get-buffer-status) "ref")
          (eide-popup-edit-use-new-file) )))
    (if eide-menu-header-file-list
      (dolist (this-buffer eide-menu-header-file-list)
        (set-buffer this-buffer-name)
        (if (string-equal (eide-edit-get-buffer-status) "ref")
          (eide-popup-edit-use-new-file) )))
    (if eide-menu-other-file-list
      (dolist (this-buffer eide-menu-other-file-list)
        (set-buffer this-buffer-name)
        (if (string-equal (eide-edit-get-buffer-status) "ref")
          (eide-popup-edit-use-new-file))))))

;; ----------------------------------------------------------------------------
;; Clean the code of a buffer
;; - replace tabs with spaces
;; - indent the whole buffer
;; ----------------------------------------------------------------------------
(defun eide-edit-pretty-buffer (this-buffer)
  (interactive)
  (if (eide-popup-question-yes-or-no-p "Do you really want to clean this buffer (undo will not be possible) ?")
    (progn
      (save-excursion
        (set-buffer my-buffer)
        (untabify (point-min) (point-max))
        (indent-region (point-min) (point-max) nil)
        (message "Buffer has been cleaned.")))))

;;; eide-edit.el ends here
