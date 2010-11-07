;;; eide-compare.el --- Emacs-IDE, compare

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

(provide 'eide-compare)

(defvar eide-compare-other-projects-list nil)
(defvar eide-compare-other-project-name nil)
(defvar eide-compare-other-project-directory nil)

(defvar eide-compare-buffer-name nil)
(defvar eide-compare-current-line nil)
(defvar eide-compare-other-buffer-name nil)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Start ediff mode.
;; ----------------------------------------------------------------------------
(defun eide-i-compare-ediff-mode-start ()
  (ad-deactivate 'select-window)
  (eide-keys-configure-for-ediff))

;; ----------------------------------------------------------------------------
;; Stop ediff mode.
;; ----------------------------------------------------------------------------
(defun eide-i-compare-ediff-mode-stop ()
  (ad-activate 'select-window)
  (eide-keys-configure-for-editor))

;; ----------------------------------------------------------------------------
;; Hook for exiting ediff: Close temporary buffer, and restore display.
;;
;; input  : eide-compare-buffer-name : name of compared buffer.
;;          eide-compare-current-line : current line in compared buffer (before
;;              ediff session).
;;          eide-compare-other-buffer-name : name of temporary buffer.
;;          eide-current-buffer : current buffer (before ediff session).
;; ----------------------------------------------------------------------------
(defun eide-i-compare-ediff-quit-hook ()
  ;; Call default hook
  (ediff-cleanup-mess)
  ;; Restore default hook
  (setq ediff-quit-hook 'ediff-cleanup-mess)
  (eide-i-compare-ediff-mode-stop)
  ;; Delete other windows, otherwise current line is not restored in
  ;; eide-compare-buffer-name, unless it is the same as eide-current-buffer
  ;; (and I don't know why !!)
  ;;(delete-other-windows)
  ;; Restore cursor position in the buffer that has been compared
  ;; TODO: Restoring cursor position does not work anymore
  (set-buffer eide-compare-buffer-name)
  (goto-line eide-compare-current-line)
  ;; Back to current buffer
  (switch-to-buffer eide-current-buffer)
  ;; Build windows layout
  (eide-windows-layout-build)
  (kill-buffer eide-compare-other-buffer-name))

;; ----------------------------------------------------------------------------
;; Compare a buffer and a file.
;;
;; input  : p-other-buffer-filename : name of compared file.
;;          p-other-buffer-name-prefix : prefix to add before file buffer name.
;;          p-buffer-in-left-window-flag : t = buffer | file,
;;              nil = file | buffer.
;;          p-force-major-mode-flag : t = force syntax highlighting for file
;;              (necessary for ".ref" or ".new" files)
;;          eide-compare-buffer-name : name of compared buffer.
;; output : eide-compare-current-line : current line in compared buffer (before
;;              ediff session).
;;          eide-compare-other-buffer-name : name of compared file buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-compare-ediff-buffer-and-file (p-other-buffer-filename p-other-buffer-name-prefix p-buffer-in-left-window-flag p-force-major-mode-flag)
  (eide-i-compare-ediff-mode-start)
  (setq ediff-quit-hook 'eide-i-compare-ediff-quit-hook)
  ;; Hide menu
  (eide-windows-layout-unbuild)
  ;; Save current line of buffer to compare
  (set-buffer eide-compare-buffer-name)
  (setq eide-compare-current-line (count-lines (point-min) (point)))
  (if (= (current-column) 0)
    (setq eide-compare-current-line (1+ eide-compare-current-line)))

  (if p-force-major-mode-flag
    (let ((l-auto-mode-alist auto-mode-alist))
      ;; Add .ref and .new files in auto-mode-alist (with current buffer major
      ;; mode)
      (push (cons "\\.ref\\'" major-mode) auto-mode-alist)
      (push (cons "\\.new\\'" major-mode) auto-mode-alist)
      (eide-windows-find-file-without-advice p-other-buffer-filename)
      ;; Restore auto-mode-alist
      (setq auto-mode-alist l-auto-mode-alist)
      ;; Turn hide/show mode off, because if emacs is closed before this
      ;; temporary buffer is closed, it will be loaded next time, with an error
      ;; because default major mode is Fundamental
      (if hs-minor-mode
        (hs-minor-mode)))
    (eide-windows-find-file-without-advice p-other-buffer-filename))

  (setq eide-compare-other-buffer-name (concat p-other-buffer-name-prefix eide-compare-buffer-name))
  (rename-buffer eide-compare-other-buffer-name)
  (if p-buffer-in-left-window-flag
    (ediff-buffers eide-compare-buffer-name eide-compare-other-buffer-name)
    (ediff-buffers eide-compare-other-buffer-name eide-compare-buffer-name)))

;; ----------------------------------------------------------------------------
;; Select ediff control window (before calling ediff command).
;; ----------------------------------------------------------------------------
(defun eide-i-compare-select-control-window ()
  (let ((l-control-window nil))
    (save-excursion
      (set-buffer "*Ediff Control Panel*")
      (setq l-control-window ediff-control-window))
    (select-window l-control-window)))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Build the list of other projects.
;;
;; input  : eide-root-directory : root directory of current project.
;; output : eide-compare-other-projects-list : list of other projects (name and
;;              directory).
;; ----------------------------------------------------------------------------
(defun eide-compare-build-other-projects-list ()
  (setq eide-compare-other-projects-list nil)
  ;; eide-root-directory:                                 <...>/current_project/
  ;; directory-file-name removes last "/":                <...>/current_project
  ;; file-name-directory removes last directory name:     <...>/
  ;; directory-files returns a list of directory content: <...>/another_project
  ;; file-name-as-directory adds "/":                     <...>/another_project/
  (dolist (l-dir (mapcar 'file-name-as-directory (directory-files (file-name-directory (directory-file-name eide-root-directory)) t)))
    (if (and (not (string-equal l-dir eide-root-directory))
             (file-exists-p (concat l-dir eide-project-file)))
      ;; Another project has been defined in this directory, retrieve project name
      ;; l-dir:                                                                   <...>/another_project/
      ;; directory-file-name removes last "/":                                    <...>/another_project
      ;; file-name-nondirectory retrieves last directory name from complete path: another_project
      (let ((l-other-project-name (file-name-nondirectory (directory-file-name l-dir))))
        ;; Do not add special directories (. and ..)
        (if (not (or (string-equal l-other-project-name ".") (string-equal l-other-project-name "..")))
          ;; Add this project to the list
          (setq eide-compare-other-projects-list (append (list (cons l-other-project-name l-dir)) eide-compare-other-projects-list)))))))

;; ----------------------------------------------------------------------------
;; Select another project for comparison.
;;
;; input  : p-project-name : project name.
;;          p-project-directory : project directory.
;; output : eide-compare-other-project-name : project name.
;;          eide-compare-other-project-directory : project directory.
;; ----------------------------------------------------------------------------
(defun eide-compare-select-another-project (p-project-name p-project-directory)
  (setq eide-compare-other-project-name p-project-name)
  (setq eide-compare-other-project-directory p-project-directory)
  (message (concat "Now you can compare files with project \"" p-project-name "\"")))

;; ----------------------------------------------------------------------------
;; Compare selected file (".new" version) with ".ref" version.
;;
;; input  : p-buffer-name : name of compared buffer.
;; output : eide-compare-buffer-name : name of compared buffer.
;; ----------------------------------------------------------------------------
(defun eide-compare-with-ref-file (p-buffer-name)
  (setq eide-compare-buffer-name p-buffer-name)
  (eide-i-compare-ediff-buffer-and-file (concat (buffer-file-name (get-buffer eide-compare-buffer-name)) ".ref") "* (REF) " nil t))

;; ----------------------------------------------------------------------------
;; Compare selected file (".ref" version) with ".new" version.
;;
;; input  : p-buffer-name : name of compared buffer.
;; output : eide-compare-buffer-name : name of compared buffer.
;; ----------------------------------------------------------------------------
(defun eide-compare-with-new-file (p-buffer-name)
  (setq eide-compare-buffer-name p-buffer-name)
  (eide-i-compare-ediff-buffer-and-file (concat (buffer-file-name (get-buffer eide-compare-buffer-name)) ".new") "* (NEW) " t t))

;; ----------------------------------------------------------------------------
;; Compare selected file with version in another project.
;;
;; input  : p-buffer-name : name of compared buffer.
;; output : eide-compare-buffer-name : name of compared buffer.
;; ----------------------------------------------------------------------------
(defun eide-compare-with-other-project (p-buffer-name)
  (setq eide-compare-buffer-name p-buffer-name)
  (eide-i-compare-ediff-buffer-and-file (concat eide-compare-other-project-directory (substring (buffer-file-name (get-buffer eide-compare-buffer-name)) (length eide-root-directory))) (concat "* (" eide-compare-other-project-name ") ") nil nil))

;; ----------------------------------------------------------------------------
;; Quit ediff session.
;; ----------------------------------------------------------------------------
(defun eide-compare-quit ()
  (interactive)
  (eide-i-compare-select-control-window)
  (call-interactively 'ediff-quit))

;; ----------------------------------------------------------------------------
;; Update diffs.
;; ----------------------------------------------------------------------------
(defun eide-compare-update ()
  (interactive)
  (eide-i-compare-select-control-window)
  (ediff-update-diffs))

;; ----------------------------------------------------------------------------
;; Go to previous diff.
;; ----------------------------------------------------------------------------
(defun eide-compare-go-to-previous-diff ()
  (interactive)
  (eide-i-compare-select-control-window)
  (ediff-previous-difference))

;; ----------------------------------------------------------------------------
;; Go to next diff.
;; ----------------------------------------------------------------------------
(defun eide-compare-go-to-next-diff ()
  (interactive)
  (eide-i-compare-select-control-window)
  (ediff-next-difference))

;; ----------------------------------------------------------------------------
;; Copy A to B.
;; ----------------------------------------------------------------------------
(defun eide-compare-copy-a-to-b ()
  (interactive)
  (eide-i-compare-select-control-window)
  (call-interactively 'ediff-copy-A-to-B))

;; ----------------------------------------------------------------------------
;; Copy B to A.
;; ----------------------------------------------------------------------------
(defun eide-compare-copy-b-to-a ()
  (interactive)
  (eide-i-compare-select-control-window)
  (call-interactively 'ediff-copy-B-to-A))

;;; eide-compare.el ends here
