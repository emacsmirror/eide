;;; eide-project.el --- Emacs-IDE, project

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

(provide 'eide-project)

(require 'desktop)

(require 'eide-config)
(require 'eide-search)

;; Check --no-desktop option before it is removed from command-line-args by desktop in after-init-hook
(defvar eide-no-desktop-option nil)
(if (member "--no-desktop" command-line-args)
  (setq eide-no-desktop-option t))

(defvar eide-root-directory nil)

;; Test if xcscope is available
(defvar eide-option-use-cscope-flag nil)
(if (locate-library "xcscope")
  (progn
    (require 'xcscope)
    (setq eide-option-use-cscope-flag t)))

(defvar eide-project-name nil)

(defvar eide-project-gdb-option nil)
(defvar eide-project-tool-bar-mode-before-debug nil)
(defvar eide-project-is-gdb-session-running-flag nil)
(defvar eide-project-is-gdb-session-visible-flag nil)

(if (locate-library "gdb-mi")
  (progn
    (require 'gdb-mi)
    (setq eide-project-gdb-option " -i=mi "))
  (progn
    (require 'gdb-ui) ; deprecated
    (setq eide-project-gdb-option " --annotate=3 ")))

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-project-compile (p-parameter)
  "Compile project.
- p-parameter: option parameter in project configuration for compile command."
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
  ;; Although eide-compilation-buffer is supposed to be displayed in output window,
  ;; it is necessary to set it as current buffer
  (set-buffer eide-compilation-buffer)
  (end-of-buffer)
  (eide-windows-select-source-window t))

(defun eide-i-project-run (p-parameter)
  "Run project.
- p-parameter: option parameter in project configuration for run command."
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

(defun eide-i-project-debug (p-program)
  "Debug project.
- p-program: option parameter in project configuration for gdb program."
  (eide-windows-select-output-window)
  ;; Sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer!
  (switch-to-buffer "*results*")
  ;; Change current directory (of unused buffer "*results*")
  (setq default-directory eide-root-directory)
  (let ((l-eide-debug-command (eide-project-get-full-gdb-command p-program)))
    (gdb l-eide-debug-command)))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-project-create ()
  "Create a project."
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

(defun eide-project-delete ()
  "Delete current project."
  (if (eide-popup-question-yes-or-no-p (concat "Delete project in " eide-root-directory " ?"))
    (progn
      (setq eide-project-name nil)
      (kill-buffer eide-project-config-file)
      (if (get-buffer "TAGS")
        (kill-buffer "TAGS"))
      (shell-command (concat "cd " eide-root-directory " ; rm -f TAGS cscope.files cscope.out .emacs-ide-project.*"))
      ;; Delete desktop file and disable automatic saving
      (if eide-no-desktop-option
        (progn
          ;; desktop-remove needs desktop-save-mode to be enabled
          (desktop-save-mode 1)
          (setq desktop-dirname eide-root-directory)))
      (desktop-remove)
      (desktop-save-mode -1)
      ;; Update frame title and menu (project is inactive now)
      (eide-project-update-frame-title)
      (eide-menu-update t)
      ;; Update key bindings for project
      (eide-keys-configure-for-editor))))

(defun eide-project-start-with-project ()
  "Start with current project."
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
        (setq eide-search-cscope-available-flag t)
        (if (not (file-exists-p (concat eide-root-directory "cscope.out")))
          (setq eide-search-cscope-update-database-request-pending-flag t)))
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

  (if (not eide-no-desktop-option)
    (progn
      ;; Enable desktop save mode: desktop is read and will be saved automatically on exit.
      (desktop-save-mode 1)
      ;; Desktop must be saved without asking (if .emacs.desktop does not exist)
      (setq desktop-save t)
      ;; Set desktop directory (set to nil when desktop save mode is disabled)
      (setq desktop-dirname eide-root-directory))))

(defun eide-project-update-frame-title ()
  "Update frame title with project name (or root directory if no project)."
  (if eide-project-name
    (setq frame-title-format (concat eide-project-name " - Emacs"))
    (setq frame-title-format (concat eide-root-directory " - Emacs"))))

(defun eide-project-get-full-command (p-parameter)
  "Get full command (init command + compile/run command).
- p-parameter: option parameter in project configuration."
  (let ((l-init-command (eide-config-get-project-value "init_command")))
    (if (string-equal l-init-command "")
      (eide-config-get-project-value p-parameter)
      (concat l-init-command " ; " (eide-config-get-project-value p-parameter)))))

(defun eide-project-get-full-gdb-command (p-program)
  "Get full gdb command (gdb command + gdb option + program name).
- p-program: option parameter in project configuration for gdb program."
  (concat (eide-config-get-project-value "debug_command") eide-project-gdb-option (eide-config-get-project-value p-program)))

(defun eide-project-get-short-gdb-command (p-program)
  "Get short gdb command (short gdb command + gdb option + program name) for popup
menu (hide gdb command path).
- p-program: option parameter in project configuration for gdb program."
  (let ((l-gdb-command (eide-config-get-project-value "debug_command")) (l-short-gdb-command nil))
    (if (string-match "/" l-gdb-command)
      (setq l-short-gdb-command (concat "[...]/" (car (last (split-string l-gdb-command "/")))))
      (setq l-short-gdb-command l-gdb-command))
    (concat l-short-gdb-command eide-project-gdb-option (eide-config-get-project-value p-program))))

(defun eide-project-get-short-directory (p-directory)
  "Get the path relative to project root directory from absolute path if it is
part of the project (remove root directory from absolute path).
- p-directory: directory (absolute path)."
  ;; Remove project base path if the file is part of it (otherwise display full path)
  (if (and (<= (length eide-root-directory) (length p-directory)) (string-equal eide-root-directory (substring p-directory 0 (length eide-root-directory))))
    (substring p-directory (length eide-root-directory))
    p-directory))

(defun eide-project-compile-1 ()
  "Compile project (1st compile command)."
  (interactive)
  (eide-i-project-compile "compile_command_1"))

(defun eide-project-compile-2 ()
  "Compile project (2nd compile command)."
  (interactive)
  (eide-i-project-compile "compile_command_2"))

(defun eide-project-compile-3 ()
  "Compile project (3rd compile command)."
  (interactive)
  (eide-i-project-compile "compile_command_3"))

(defun eide-project-compile-4 ()
  "Compile project (4th compile command)."
  (interactive)
  (eide-i-project-compile "compile_command_4"))

(defun eide-project-run-1 ()
  "Run project (1st run command)."
  (interactive)
  (eide-i-project-run "run_command_1"))

(defun eide-project-run-2 ()
  "Run project (2nd run command)."
  (interactive)
  (eide-i-project-run "run_command_2"))

(defun eide-project-debug-mode-start ()
  "Start debug mode."
  ;; Restore colors (in case user was reading help or config)
  (eide-config-set-colors-for-files)
  (eide-keys-configure-for-gdb)
  (eide-windows-layout-unbuild)
  (if window-system
    (progn
      ;; Show gdb toolbar
      ;; NB: eide-project-debug-mode-start may be called twice: do not overwrite
      ;; eide-project-tool-bar-mode-before-debug on second call
      (if (not eide-project-is-gdb-session-visible-flag)
        (setq eide-project-tool-bar-mode-before-debug tool-bar-mode))
      (tool-bar-mode 1)))
  (setq display-buffer-function nil)
  (setq eide-project-is-gdb-session-visible-flag t)
  (setq eide-project-is-gdb-session-running-flag t))

(defun eide-project-debug-mode-stop ()
  "Stop debug mode."
  (eide-keys-configure-for-editor)
  (eide-windows-layout-build)
  (if window-system
    ;; Hide tool bar if necessary (restore previous state)
    (tool-bar-mode (if eide-project-tool-bar-mode-before-debug 1 -1)))
  (setq display-buffer-function 'eide-i-windows-display-buffer-function)
  (setq eide-project-is-gdb-session-visible-flag nil))

(defun eide-project-debug-1 ()
  "Debug project (1st debug command)."
  (interactive)
  (eide-i-project-debug "debug_program_1"))

(defun eide-project-debug-2 ()
  "Debug project (2nd debug command)."
  (interactive)
  (eide-i-project-debug "debug_program_2"))

;;; eide-project.el ends here
