;;; eide-project.el --- Emacs-IDE, project

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

(provide 'eide-project)


;; Shell command for creating tags
(setq eide-create-tags-for-c-command    "rm -f TAGS ; find . -type f -name \"*.[ch]\" -exec etags -a {} \\;")
;; TODO : Lisp : pas seulement *.el !...
(setq eide-create-tags-for-lisp-command "rm -f TAGS ; find . -type f -name \"*.el\" -exec etags -a {} \\;")
;(setq eide-create-tags-command   "rm -f TAGS ; ctags -eR")

;; Shell command for creating cscope.files
;; -type f : excludes links
;; cscope.out will be generated on next search
(setq eide-create-cscope-command "rm -f cscope.files cscope.out ; find . -type f -name \"*.[ch]\" > cscope.files")

;; eide-project-start-shell-alias is necessary for bash but not csh (.cshrc is run automatically)
(if (string-equal shell-file-name "/bin/bash")
  (setq eide-project-start-shell-alias       ". ~/.bashrc") ; for bash
  (setq eide-project-start-shell-alias       ""))           ; for csh


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-project-internal-create-tags ()
  (shell-command (concat "cd " eide-project-directory " ; " eide-create-tags-command)))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-project-internal-create-cscope-list-of-files ()
  (shell-command (concat "cd " eide-project-directory " ; " eide-create-cscope-command)))
;  (cscope-index-files nil))

;; ----------------------------------------------------------------------------
;; Compile project (with given command)
;;
;; input  : parameter
;; ----------------------------------------------------------------------------
(defun eide-project-internal-compile (parameter)
  (eide-windows-select-window-results t)
  ;; sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer !
  (switch-to-buffer "*results*")
  (setq compile-command (eide-project-get-full-command parameter))
  ;; Change current directory (of unused buffer "*results*")
  (setq default-directory eide-project-directory)
  (compile compile-command)
  (setq eide-buffer-compile (buffer-name))
  (eide-toolbar-update)
  (eide-windows-select-window-file t))

;; ----------------------------------------------------------------------------
;; Run project (with given command)
;;
;; input  : command
;; ----------------------------------------------------------------------------
(defun eide-project-internal-run (parameter)
  (eide-windows-select-window-results t)
  ;; sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer !
  (switch-to-buffer "*results*")
  ;; Changing current directory has no effect with shell-command
  ;; Instead, we must change current directory in the command itself
  ;; Command ends with "&" otherwise emacs gets frozen until gdb is closed
  (setq eide-run-command (concat "cd " eide-project-directory " ; " (eide-project-get-full-command parameter) " &"))
  ;; Run buffer name will be updated asynchronously in eide-windows-internal-special-display-function
  (setq eide-menu-update-run-buffer t)
  (shell-command eide-run-command)
;  (setq eide-buffer-run (buffer-name))
  (eide-windows-select-window-results t))

;; ----------------------------------------------------------------------------
;; Debug project (with given program)
;;
;; input  : this-command : gdb command
;;          program : program to debug
;; ----------------------------------------------------------------------------
(defun eide-project-internal-debug (parameter)
  (eide-windows-select-window-results t)
  ;; sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer !
  (switch-to-buffer "*results*")
  (setq eide-debug-command (eide-custom-get-project-value parameter))
  ;; Change current directory (of unused buffer "*results*")
  (setq default-directory eide-project-directory)
  (gdb eide-debug-command)
  (setq eide-buffer-debug (buffer-name))
  (eide-toolbar-update)
  (eide-windows-select-window-results t))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Create a project
;; input  :  project-type      : string describing the language of the project
;; output :  eide-project-name : project name
;; ----------------------------------------------------------------------------
(defun eide-project-create (project-type)
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (if (eide-popup-question-yes-or-no-p (concat "Create " project-type " project in " eide-project-directory " ?"))
    (progn
      (setq eide-session-project t)
      (setq eide-project-type project-type)

      (eide-windows-select-window-file t)
      (eide-project-start-with-project)
      ;; Update menu (project is active now)
      (eide-menu-update t)
      (if eide-custom-use-toolbar
        (eide-toolbar-update))
      ;; Update key bindings for project
      (eide-key-bindings-configure-for-editor))))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-project-start-with-project ()
  ;; "Lock" project
  ;(shell-command (concat "touch " eide-project-directory eide-project-lock-file))

  ;; Load or create project file
  (find-file-noselect (concat eide-project-directory eide-project-file))
  (eide-custom-rebuild-project-file)
  (eide-custom-apply-project-configuration)

  ;; Select appropriate command for tags, according to project type
  (if (string-equal eide-project-type "C")
    (setq eide-create-tags-command eide-create-tags-for-c-command)
    (if (string-equal eide-project-type "Lisp")
      (setq eide-create-tags-command eide-create-tags-for-lisp-command)
      (progn
        (setq eide-project-type "C")
        (setq eide-create-tags-command eide-create-tags-for-c-command))))

  (if (and (string-equal eide-project-type "C") eide-option-use-cscope-flag)
    ;; Create cscope database if necessary
    (if (not (file-exists-p (concat eide-project-directory "cscope.files")))
      (progn
        (message "Creating cscope list of files...")
;        (shell-command (concat "cd " eide-project-directory " ; cscope -bR"))
        (eide-project-internal-create-cscope-list-of-files)
        (message "Creating cscope list of files...done"))))

  (if (or (not (string-equal eide-project-type "C")) (or (not eide-option-use-cscope-flag) eide-option-use-cscope-and-tags-flag))
    (progn
      ;; Create tags if necessary
      (if (not (file-exists-p (concat eide-project-directory "TAGS")))
        (progn
          (message "Creating tags...")
          (eide-project-internal-create-tags)
          (message "Creating tags...done")))
      ;; Load tags now, otherwise first tag search will take some time...
      (find-file-noselect (concat eide-project-directory "TAGS"))))

  (if (not (file-exists-p (concat eide-project-directory eide-project-notes-file)))
    ;; Create empty project notes file
    (shell-command (concat "touch " eide-project-directory eide-project-notes-file)))

  ;; TODO : sous flag
  ;; Tag file name with full path
  (setq tags-file-name (concat eide-project-directory "TAGS"))

  ;; Create session file if necessary
  (if (not (file-exists-p (concat eide-project-directory ".emacs.desktop")))
    (progn
      (message "Creating .emacs.desktop...")
      (desktop-save eide-project-directory)
      (message "Creating .emacs.desktop...done"))))

;; ----------------------------------------------------------------------------
;; Update tags
;; ----------------------------------------------------------------------------
(defun eide-project-update-tags ()
  (interactive)
  (message "Updating tags...")
  (eide-project-internal-create-tags)
  (message "Updating tags...done"))

;; ----------------------------------------------------------------------------
;; Update cscope list of files
;; ----------------------------------------------------------------------------
(defun eide-project-update-cscope-list-of-files ()
  (interactive)
  (message "Updating cscope list of files...")
;  (shell-command (concat "cd " eide-project-directory " ; rm -f cscope.files cscope.out"))
  (eide-project-internal-create-cscope-list-of-files)
;  (shell-command (concat "cd " eide-project-directory " ; cscope -bR"))
  (message "Updating cscope list of files...done"))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-project-update-frame-title ()
  (setq frame-title-format (concat eide-project-name " - Emacs")))

;; ----------------------------------------------------------------------------
;; Get full command (init command + compile/run command)
;;
;; input  : command
;; return : full command
;; ----------------------------------------------------------------------------
(defun eide-project-get-full-command (parameter)
;  (let ((my-command (concat "cd " eide-project-directory " ; ")) (my-init (eide-custom-get-project-value "init_command")))
;    (if (not (string-equal my-init ""))
;      (setq my-command (concat my-command my-init " ; ")))
;    (concat my-command (eide-custom-get-project-value parameter))))
  (let ((my-init (eide-custom-get-project-value "init_command")))
    (if (string-equal my-init "")
      (eide-custom-get-project-value parameter)
      (concat my-init " ; " (eide-custom-get-project-value parameter)))))

;; ----------------------------------------------------------------------------
;; Remove project path from directory
;;
;; input  : eide-project-directory : project directory path
;;          this-directory : directory to shorten (?)
;; return : short directory
;; ----------------------------------------------------------------------------
(defun eide-project-get-short-directory (this-directory)
  ;; Remove project base path if the file is part of it (otherwise display full path)
  (if (and (<= (length eide-project-directory) (length this-directory)) (string-equal eide-project-directory (substring this-directory 0 (length eide-project-directory))))
    (substring this-directory (length eide-project-directory))
    (setq this-directory this-directory)))

;; ----------------------------------------------------------------------------
;; Compile project (1st compile command)
;; ----------------------------------------------------------------------------
(defun eide-project-compile-1 ()
  (interactive)
  (eide-project-internal-compile "compile_command_1"))

;; ----------------------------------------------------------------------------
;; Compile project (2nd compile command)
;; ----------------------------------------------------------------------------
(defun eide-project-compile-2 ()
  (interactive)
  (eide-project-internal-compile "compile_command_2"))

;; ----------------------------------------------------------------------------
;; Run project (1st run command)
;; ----------------------------------------------------------------------------
(defun eide-project-run-1 ()
  (interactive)
  (eide-project-internal-run "run_command_1"))

;; ----------------------------------------------------------------------------
;; Run project (2nd run command)
;; ----------------------------------------------------------------------------
(defun eide-project-run-2 ()
  (interactive)
  (eide-project-internal-run "run_command_2"))

;; ----------------------------------------------------------------------------
;; Debug project (1st debug command)
;; ----------------------------------------------------------------------------
(defun eide-project-debug-1 ()
  (interactive)
  (eide-project-internal-debug "debug_command_1"))

;; ----------------------------------------------------------------------------
;; Debug project (2nd debug command)
;; ----------------------------------------------------------------------------
(defun eide-project-debug-2 ()
  (interactive)
  (eide-project-internal-debug "debug_command_2"))

;;; eide-project.el ends here
