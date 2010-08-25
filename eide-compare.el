;;; eide-compare.el --- Emacs-IDE, compare

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

(provide 'eide-compare)

(defvar eide-compare-other-project-directory nil)
(defvar eide-compare-other-projects-list nil)


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Start ediff mode
;; ----------------------------------------------------------------------------
(defun eide-compare-internal-ediff-mode-start ()
  ;(ad-deactivate 'switch-to-buffer)
  (eide-key-bindings-configure-for-ediff))

;; ----------------------------------------------------------------------------
;; Stop ediff mode
;; ----------------------------------------------------------------------------
(defun eide-compare-internal-ediff-mode-stop ()
  ;(ad-activate 'switch-to-buffer)
  (eide-key-bindings-configure-for-editor))

;; ----------------------------------------------------------------------------
;; Hook for exiting ediff : Close temporary buffer, and restore display
;;
;; input  : eide-compare-other-buffer-name : name of temporary buffer to be
;;                                           closed
;; ----------------------------------------------------------------------------
(defun eide-compare-internal-ediff-quit-hook ()
  ;; Call default hook
  (ediff-cleanup-mess)
  ;; Delete other windows, otherwise current line is not restored in
  ;; eide-compare-buffer-name, unless it is the same as eide-current-buffer
  ;; (and I don't know why !!)
  (delete-other-windows)
  ;; Restore cursor position in the buffer that has been compared
  (set-buffer eide-compare-buffer-name)
  (goto-line eide-compare-current-line)
  ;; Back to current buffer
  (switch-to-buffer eide-current-buffer)
  ;; Build windows layout
  (eide-windows-layout-build)
  ;; Restore default hook
  (setq ediff-quit-hook 'ediff-cleanup-mess)
  (eide-compare-internal-ediff-mode-stop)
  (kill-buffer eide-compare-other-buffer-name))

;; ----------------------------------------------------------------------------
;; Compare a buffer and a file
;;
;; input  : other-buffer-name-prefix   : prefix to add before file buffer name
;;          buffer-in-left-window-flag : t = buffer|file, nil = file|buffer
;;          force-major-mode-flag      : t = force syntax highlighting for file
;;                                       (necessary when file name ends in .ref
;;                                       or .new)
;; ----------------------------------------------------------------------------
(defun eide-compare-internal-ediff-2-files (other-buffer-name-prefix buffer-in-left-window-flag force-major-mode-flag)
  (eide-compare-internal-ediff-mode-start)
  (setq ediff-quit-hook 'eide-compare-internal-ediff-quit-hook)
  ;; Hide menu
  (eide-windows-layout-unbuild)
  ;; Save current line of buffer to compare
  (set-buffer eide-compare-buffer-name)
  (setq eide-compare-current-line (count-lines (point-min) (point)))
  (if (= (current-column) 0)
    (setq eide-compare-current-line  (1+ eide-compare-current-line)))

  (if force-major-mode-flag
    (setq default-major-mode major-mode))
  (eide-menu-find-file-without-hook eide-compare-other-buffer-filename)
;;  (setq eide-compare-other-buffer-name (buffer-name))
  (setq eide-compare-other-buffer-name (concat other-buffer-name-prefix eide-compare-buffer-name))
  (rename-buffer eide-compare-other-buffer-name)
; fichier non enregistré dans .emacs.desktop
; mais : du coup, on ne peut pas le modifier (c'est un buffer sans fichier associé !)
;  (set-buffer (generate-new-buffer eide-compare-other-buffer-name))
;  (insert-file-contents eide-compare-other-buffer-filename)
  (if force-major-mode-flag
    (progn
      ;; Set major mode, in case file name ends in .ref or .new
      (set-buffer-major-mode (current-buffer)) ;eide-compare-other-buffer-name)
      ;; Turn hide/show mode off, because if emacs is closed before this
      ;; temporary buffer is closed, it will be loaded next time, with an error
      ;; because default major mode is Fundamental
      ;; TODO : réécrire le commentaire ci-dessus ! :-)
      (if hs-minor-mode
          (hs-minor-mode))))

;  (ediff-files my-ref-file eide-compare-buffer-name)
  (if buffer-in-left-window-flag
    (ediff-buffers eide-compare-buffer-name eide-compare-other-buffer-name)
    (ediff-buffers eide-compare-other-buffer-name eide-compare-buffer-name))

  (if force-major-mode-flag
    (setq default-major-mode 'fundamental-mode)))

;; TODO : action "update project list"

;; ----------------------------------------------------------------------------
;; Quit ediff session
;; ----------------------------------------------------------------------------
(defun eide-compare-internal-select-control-window ()
  (let ((my-control-window nil))
    (save-excursion
      (set-buffer "*Ediff Control Panel*")
      (setq my-control-window ediff-control-window))
    (select-window my-control-window)))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Build the list of other projects
;;
;; output :  eide-compare-other-projects-list : list of other projects
;; ----------------------------------------------------------------------------
(defun eide-compare-build-other-projects-list ()
  (setq eide-compare-other-projects-list nil)
  ;; file-name-as-directory adds "/"
  (dolist (this-dir (mapcar 'file-name-as-directory (directory-files (concat eide-project-directory "..") t)))
    (let ((my-project-file (concat this-dir eide-project-file)))
      (if (file-exists-p my-project-file)
        ;; A project has been defined in this directory
        (progn
          (if eide-windows-is-layout-visible-flag
            (eide-windows-select-window-file t))
          ;; Load ".emacs-ide.project" for this project
          (eide-menu-find-file-without-hook my-project-file)
          ;; Retrieve project name
          (let ((proj-name (eide-custom-get-project-value-in-current-buffer "project_name")))
            (if (string-equal proj-name eide-project-name)
              ;; This is current project : no need to add it to the list
              ;; ".emacs-ide.project" should not be closed : switch to current
              ;; buffer instead (without defadvice, because current buffer
              ;; might be *scratch*)
              (eide-menu-switch-to-buffer-without-advice eide-current-buffer)
              (progn
                ;; Close ".emacs-ide.project" for this project
                (kill-this-buffer)
                ;; Add this project to the list
                (setq eide-compare-other-projects-list (append (list (cons proj-name this-dir)) eide-compare-other-projects-list))))))))))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-compare-select-another-project (this-project-name this-project-directory)
  (interactive)
  (setq eide-compare-other-project-name this-project-name)
  (setq eide-compare-other-project-directory this-project-directory)
  (message (concat "Now you can compare files with project \"" this-project-name "\"")))

;; ----------------------------------------------------------------------------
;; Compare selected file (".new" version) with ".ref" version
;; ----------------------------------------------------------------------------
(defun eide-compare-with-ref-file (this-buffer)
  (interactive)
  (setq eide-compare-buffer-name this-buffer)
  (setq eide-compare-buffer-filename (buffer-file-name (get-buffer eide-compare-buffer-name)))
  (setq eide-compare-other-buffer-filename (concat eide-compare-buffer-filename ".ref"))
  (eide-compare-internal-ediff-2-files "* (REF) " nil t))

;; ----------------------------------------------------------------------------
;; Compare selected file (".ref" version) with ".new" version
;; ----------------------------------------------------------------------------
(defun eide-compare-with-new-file (this-buffer)
  (interactive)
  (setq eide-compare-buffer-name this-buffer)
  (setq eide-compare-buffer-filename (buffer-file-name (get-buffer eide-compare-buffer-name)))
  (setq eide-compare-other-buffer-filename (concat eide-compare-buffer-filename ".new"))
  (eide-compare-internal-ediff-2-files "* (NEW) " t t))

;; ----------------------------------------------------------------------------
;; Compare selected file with version in another project
;; ----------------------------------------------------------------------------
(defun eide-compare-with-other-project (this-buffer)
  (interactive)
  (setq eide-compare-buffer-name this-buffer)
  (setq eide-compare-buffer-filename (buffer-file-name (get-buffer eide-compare-buffer-name)))
  (setq eide-compare-other-buffer-filename (concat eide-compare-other-project-directory (substring eide-compare-buffer-filename (length eide-project-directory))))
  (eide-compare-internal-ediff-2-files (concat "* (" eide-compare-other-project-name ") ") nil nil))

;; ----------------------------------------------------------------------------
;; Quit ediff session
;; ----------------------------------------------------------------------------
(defun eide-compare-quit ()
  (interactive)
  (eide-compare-internal-select-control-window)
  (call-interactively 'ediff-quit))

;; ----------------------------------------------------------------------------
;; Update diffs
;; ----------------------------------------------------------------------------
(defun eide-compare-update ()
  (interactive)
  (eide-compare-internal-select-control-window)
  (ediff-update-diffs))

;; ----------------------------------------------------------------------------
;; Go to previous diff
;; ----------------------------------------------------------------------------
(defun eide-compare-go-to-previous-diff ()
  (interactive)
  (eide-compare-internal-select-control-window)
  (ediff-previous-difference))

;; ----------------------------------------------------------------------------
;; Go to next diff
;; ----------------------------------------------------------------------------
(defun eide-compare-go-to-next-diff ()
  (interactive)
  (eide-compare-internal-select-control-window)
  (ediff-next-difference))

;; ----------------------------------------------------------------------------
;; Copy A to B
;; ----------------------------------------------------------------------------
(defun eide-compare-copy-a-to-b ()
  (interactive)
  (eide-compare-internal-select-control-window)
  (call-interactively 'ediff-copy-A-to-B))

;; ----------------------------------------------------------------------------
;; Copy B to A
;; ----------------------------------------------------------------------------
(defun eide-compare-copy-b-to-a ()
  (interactive)
  (eide-compare-internal-select-control-window)
  (call-interactively 'ediff-copy-B-to-A))

;;; eide-compare.el ends here
