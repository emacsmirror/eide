;;; eide-project.el --- Emacs-IDE, project

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

(provide 'eide-project)

(require 'desktop) ; for all desktop-* functions

(require 'eide-search) ; for eide-search-tags-available-flag, eide-search-cscope-available-flag, eide-search-create-tags, eide-search-update-cscope-status, and eide-search-create-cscope-list-of-files

(defvar eide-root-directory nil)

;; Test if xcscope is available
(defvar eide-option-use-cscope-flag nil)
(if (locate-library "xcscope")
  (progn
    (require 'xcscope)
    (setq eide-option-use-cscope-flag t)))

(defvar eide-project-name nil)

(defvar eide-project-is-gdb-session-running-flag nil)
(defvar eide-project-is-gdb-session-visible-flag nil)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Compile project.
;;
;; input  : p-parameter : option parameter in project configuration for
;;              compile command.
;;          eide-root-directory : project root directory.
;; output : eide-windows-update-output-buffer-id : "c" for "compile".
;; ----------------------------------------------------------------------------
(defun eide-i-project-compile (p-parameter)
  (eide-windows-select-output-window)
  ;; Sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer!
  (switch-to-buffer "*results*")
  ;; Change current directory (of unused buffer "*results*")
  (setq default-directory eide-root-directory)
  (let ((l-compile-command (eide-project-get-full-command p-parameter)))
    ;; Compile buffer name will be updated in eide-i-windows-display-buffer-function
    (setq eide-windows-update-output-buffer-id "c")
    (compile l-compile-command))
  (end-of-buffer)
  (eide-windows-select-source-window t))

;; ----------------------------------------------------------------------------
;; Run project.
;;
;; input  : p-parameter : option parameter in project configuration for
;;              run command.
;;          eide-root-directory : project root directory.
;; output : eide-windows-update-output-buffer-id : "r" for "run".
;; ----------------------------------------------------------------------------
(defun eide-i-project-run (p-parameter)
  (eide-windows-select-output-window)
  ;; Sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer!
  (switch-to-buffer "*results*")
  ;; Changing current directory has no effect with shell-command
  ;; Instead, we must change current directory in the command itself
  ;; Command ends with "&" otherwise emacs gets frozen until gdb is closed
  (let ((l-run-command (concat "cd " eide-root-directory " ; " (eide-project-get-full-command p-parameter) " &")))
    ;; Run buffer name will be updated in eide-i-windows-display-buffer-function
    (setq eide-windows-update-output-buffer-id "r")
    (shell-command l-run-command)))

;; ----------------------------------------------------------------------------
;; Debug project.
;;
;; input  : p-program : option parameter in project configuration for gdb
;;              program.
;;          eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-i-project-debug (p-program)
  (eide-windows-select-output-window)
  ;; Sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer!
  (switch-to-buffer "*results*")
  ;; Change current directory (of unused buffer "*results*")
  (setq default-directory eide-root-directory)
  (let ((l-eide-debug-command (eide-project-get-full-gdb-command p-program)))
    (gdb l-eide-debug-command)))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Create a project.
;;
;; input  : eide-root-directory : project root directory.
;; output : eide-project-name : project name.
;; ----------------------------------------------------------------------------
(defun eide-project-create ()
  (if (eide-popup-question-yes-or-no-p (concat "Create project in " eide-root-directory " ?"))
    (progn
      (eide-windows-select-source-window t)
      ;; Create empty project file
      (shell-command (concat "touch " eide-root-directory eide-project-config-file))
      (eide-project-start-with-project)
      ;; Update frame title and menu (project is active now)
      (eide-project-update-frame-title)
      (eide-menu-update t)
      ;; Update key bindings for project
      (eide-keys-configure-for-editor))))

;; ----------------------------------------------------------------------------
;; Delete current project.
;;
;; input  : eide-root-directory : project root directory.
;; output : eide-project-name : project name (nil).
;; ----------------------------------------------------------------------------
(defun eide-project-delete ()
  (if (eide-popup-question-yes-or-no-p (concat "Delete project in " eide-root-directory " ?"))
    (progn
      (setq eide-project-name nil)
      (kill-buffer eide-project-config-file)
      (if (get-buffer "TAGS")
        (kill-buffer "TAGS"))
      (shell-command (concat "cd " eide-root-directory " ; rm -f TAGS cscope.files cscope.out .emacs-ide.*"))
      ;; Delete desktop file and disable automatic saving
      (desktop-remove)
      (desktop-save-mode -1)
      ;; Update frame title and menu (project is inactive now)
      (eide-project-update-frame-title)
      (eide-menu-update t)
      ;; Update key bindings for project
      (eide-keys-configure-for-editor))))

;; ----------------------------------------------------------------------------
;; Start with current project.
;;
;; input  : eide-root-directory : project root directory.
;; output : eide-search-tags-available-flag : t if tags are already available.
;;          eide-search-cscope-available-flag : t if cscope is already
;;              available.
;; ----------------------------------------------------------------------------
(defun eide-project-start-with-project ()
  ;; Get project name from directory
  ;; eide-root-directory:                                                     <...>/current_project/
  ;; directory-file-name removes last "/":                                    <...>/current_project
  ;; file-name-nondirectory retrieves last directory name from complete path: current_project
  (setq eide-project-name (file-name-nondirectory (directory-file-name eide-root-directory)))

  ;; "Lock" project
  ;;(shell-command (concat "touch " eide-root-directory eide-project-lock-file))

  ;; Rebuild project file
  (eide-config-rebuild-project-file)

  ;; Create tags if necessary
  (if (file-exists-p (concat eide-root-directory "TAGS"))
    (setq eide-search-tags-available-flag t)
    (eide-search-create-tags))
  ;; Load tags now, otherwise first tag search will take some time...
  ;;(find-file-noselect (concat eide-root-directory "TAGS"))

  (if eide-option-use-cscope-flag
    ;; Create cscope database if necessary
    (if (file-exists-p (concat eide-root-directory "cscope.files"))
      (progn
        (eide-search-update-cscope-status)
        (setq eide-search-cscope-available-flag t))
      (eide-search-create-cscope-list-of-files)))

  ;; Migration from Emacs-IDE 1.5
  (if (and (not (file-exists-p eide-project-notes-file))
           (file-exists-p ".emacs-ide.project_notes"))
    (shell-command (concat "mv .emacs-ide.project_notes " eide-project-notes-file)))
  (if (not (file-exists-p (concat eide-root-directory eide-project-notes-file)))
    ;; Create empty project notes file
    (shell-command (concat "touch " eide-root-directory eide-project-notes-file)))

  ;; TODO: sous flag
  ;; Tag file name with full path
  (setq tags-file-name (concat eide-root-directory "TAGS"))

  ;; Enable desktop save mode: desktop is read and will be saved automatically on exit.
  (desktop-save-mode 1)
  ;; Desktop must be saved without asking (if .emacs.desktop does not exist)
  (setq desktop-save t)
  ;; Set desktop directory (set to nil when desktop save mode is disabled)
  (setq desktop-dirname eide-root-directory))

;; ----------------------------------------------------------------------------
;; Update frame title with project name (or root directory if no project)
;;
;; input  : eide-project-name : project name.
;; ----------------------------------------------------------------------------
(defun eide-project-update-frame-title ()
  (if eide-project-name
    (setq frame-title-format (concat eide-project-name " - Emacs"))
    (setq frame-title-format (concat eide-root-directory " - Emacs"))))

;; ----------------------------------------------------------------------------
;; Get full command (init command + compile/run command).
;;
;; input  : p-parameter : option parameter in project configuration.
;; return : full command.
;; ----------------------------------------------------------------------------
(defun eide-project-get-full-command (p-parameter)
  (let ((l-init-command (eide-config-get-project-value "init_command")))
    (if (string-equal l-init-command "")
      (eide-config-get-project-value p-parameter)
      (concat l-init-command " ; " (eide-config-get-project-value p-parameter)))))

;; ----------------------------------------------------------------------------
;; Get full gdb command (gdb command + "--annotate=3" + program name).
;;
;; input  : p-program : option parameter in project configuration for gdb
;;              program.
;; return : full command.
;; ----------------------------------------------------------------------------
(defun eide-project-get-full-gdb-command (p-program)
  (concat (eide-config-get-project-value "debug_command") " --annotate=3 " (eide-config-get-project-value p-program)))

;; ----------------------------------------------------------------------------
;; Get short gdb command (short gdb command + "--annotate=3" + program name)
;; for popup menu.
;;
;; input  : p-program : option parameter in project configuration for gdb
;;              program.
;; return : short command (hide gdb command path).
;; ----------------------------------------------------------------------------
(defun eide-project-get-short-gdb-command (p-program)
  (let ((l-gdb-command (eide-config-get-project-value "debug_command")) (l-short-gdb-command nil))
    (if (string-match "/" l-gdb-command)
      (setq l-short-gdb-command (concat "[...]/" (car (last (split-string l-gdb-command "/")))))
      (setq l-short-gdb-command l-gdb-command))
    (concat l-short-gdb-command " --annotate=3 " (eide-config-get-project-value p-program))))

;; ----------------------------------------------------------------------------
;; Get project relative path from absolute path (remove project absolute path
;; from directory).
;;
;; input  : p-directory : directory (absolute path).
;;          eide-root-directory : project root directory.
;; return : directory (project relative path).
;; ----------------------------------------------------------------------------
(defun eide-project-get-short-directory (p-directory)
  ;; Remove project base path if the file is part of it (otherwise display full path)
  (if (and (<= (length eide-root-directory) (length p-directory)) (string-equal eide-root-directory (substring p-directory 0 (length eide-root-directory))))
    (substring p-directory (length eide-root-directory))
    p-directory))

;; ----------------------------------------------------------------------------
;; Compile project (1st compile command).
;; ----------------------------------------------------------------------------
(defun eide-project-compile-1 ()
  (interactive)
  (eide-i-project-compile "compile_command_1"))

;; ----------------------------------------------------------------------------
;; Compile project (2nd compile command).
;; ----------------------------------------------------------------------------
(defun eide-project-compile-2 ()
  (interactive)
  (eide-i-project-compile "compile_command_2"))

;; ----------------------------------------------------------------------------
;; Compile project (3rd compile command).
;; ----------------------------------------------------------------------------
(defun eide-project-compile-3 ()
  (interactive)
  (eide-i-project-compile "compile_command_3"))

;; ----------------------------------------------------------------------------
;; Compile project (4th compile command).
;; ----------------------------------------------------------------------------
(defun eide-project-compile-4 ()
  (interactive)
  (eide-i-project-compile "compile_command_4"))

;; ----------------------------------------------------------------------------
;; Run project (1st run command).
;; ----------------------------------------------------------------------------
(defun eide-project-run-1 ()
  (interactive)
  (eide-i-project-run "run_command_1"))

;; ----------------------------------------------------------------------------
;; Run project (2nd run command).
;; ----------------------------------------------------------------------------
(defun eide-project-run-2 ()
  (interactive)
  (eide-i-project-run "run_command_2"))

;; ----------------------------------------------------------------------------
;; Start debug mode.
;; ----------------------------------------------------------------------------
(defun eide-project-debug-mode-start ()
  ;; Restore colors (in case user was reading help or config)
  (eide-config-set-colors-for-files)
  (eide-keys-configure-for-gdb)
  (eide-windows-layout-unbuild)
  ;; Show gdb toolbar
  (if window-system
    (tool-bar-mode 1))
  (setq display-buffer-function nil)
  (setq eide-project-is-gdb-session-visible-flag t)
  (setq eide-project-is-gdb-session-running-flag t))

;; ----------------------------------------------------------------------------
;; Stop debug mode.
;; ----------------------------------------------------------------------------
(defun eide-project-debug-mode-stop ()
  (eide-keys-configure-for-editor)
  (eide-windows-layout-build)
  ;; Hide gdb toolbar
  (if window-system
    (tool-bar-mode -1))
  (setq display-buffer-function 'eide-i-windows-display-buffer-function)
  (setq eide-project-is-gdb-session-visible-flag nil))

;; ----------------------------------------------------------------------------
;; Debug project (1st debug command).
;; ----------------------------------------------------------------------------
(defun eide-project-debug-1 ()
  (interactive)
  (eide-i-project-debug "debug_program_1"))

;; ----------------------------------------------------------------------------
;; Debug project (2nd debug command).
;; ----------------------------------------------------------------------------
(defun eide-project-debug-2 ()
  (interactive)
  (eide-i-project-debug "debug_program_2"))

;;; eide-project.el ends here
