;;; eide-project.el --- Emacs-IDE, project

;; Copyright (C) 2008-2013 Cédric Marie

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
(defvar eide-root-directory-at-startup nil)

;; Test if xcscope is available
(defvar eide-option-use-cscope-flag nil)
(if (locate-library "xcscope")
  (progn
    (require 'xcscope)
    (setq eide-option-use-cscope-flag t)))

(defvar eide-project-current-workspace 1)

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

(defvar eide-project-projects-file "~/.emacs-ide/workspace1/projects-list")
(defvar eide-project-projects-buffer-name "* Emacs-IDE projects *")

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-project-force-desktop-read-hook ()
  "Hook to be called at startup, to force to read the desktop when after-init-hook
has already been called."
  (if (not desktop-file-modtime)
    ;; Desktop has not been read: read it now.
    (desktop-read eide-root-directory)))

(defun eide-i-project-update-frame-title ()
  "Update frame title with project name (or root directory if no project)."
  (if eide-project-name
    (setq frame-title-format (concat eide-project-name " - Emacs"))
    (setq frame-title-format (concat eide-root-directory " - Emacs"))))

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

(defun eide-i-project-add-project (p-startup-flag)
  "Add current project to projects list.
- p-startup-flag: t when called from the init."
  (save-current-buffer
    (if (get-buffer eide-project-projects-buffer-name)
      (progn
        (set-buffer eide-project-projects-buffer-name)
        (setq buffer-read-only nil))
      (let ((l-buffer-name (find-file-noselect eide-project-projects-file)))
        (set-buffer l-buffer-name)
        (rename-buffer eide-project-projects-buffer-name)))
    (goto-char (point-min))
    (if (not (re-search-forward (concat "^" eide-root-directory "$") nil t))
      (progn
        (goto-line 2)
        (while (and (not (eobp))
                    (string-lessp (buffer-substring-no-properties (point) (line-end-position)) eide-root-directory))
          (forward-line 2))
        (if (not (eobp))
          (forward-line -1))
        (put-text-property (point) (progn (insert eide-project-name) (point)) 'face 'eide-config-project-current-name-face)
        (insert "\n")
        (insert eide-root-directory)
        (insert "\n")
        (if (not p-startup-flag)
          (ad-deactivate 'save-buffer))
        (save-buffer)
        (if (not p-startup-flag)
          (ad-activate 'save-buffer))))
    (kill-this-buffer)))

(defun eide-i-project-remove-project ()
  "Remove current project from projects list."
  (save-current-buffer
    (if (get-buffer eide-project-projects-buffer-name)
      (progn
        (set-buffer eide-project-projects-buffer-name)
        (setq buffer-read-only nil))
      (let ((l-buffer-name (find-file-noselect eide-project-projects-file)))
        (set-buffer l-buffer-name)
        (rename-buffer eide-project-projects-buffer-name)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" eide-root-directory "$") nil t)
      (progn
        (forward-line -1)
        (delete-region (point) (progn (forward-line 2) (point)))
        (ad-deactivate 'save-buffer)
        (save-buffer)
        (ad-activate 'save-buffer)))
    (kill-this-buffer)))

(defun eide-i-project-open ()
  "Open project on current line."
  (interactive)
  (let ((l-project-path (progn (beginning-of-line) (forward-line) (buffer-substring-no-properties (point) (line-end-position)))))
    ;; Close projects list (so that it can be modified by another Emacs session)
    (kill-this-buffer)
    ;; Restore editor configuration
    (eide-config-set-colors-for-files)
    (eide-keys-configure-for-editor)
    ;; Changing desktop (desktop-change-dir) sometimes unbuild the windows layout!...
    ;; Therefore it is necessary to unbuild it intentionally before loading the new desktop,
    ;; otherwise we get errors for non-existing windows
    (eide-windows-layout-unbuild)
    ;; Set root directory
    (setq eide-root-directory l-project-path)
    (eide-project-load nil)
    (eide-menu-update t)
    (eide-windows-layout-build)))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-project-create-workspaces ()
  "Create directories for workspaces, if missing."
  (let ((l-workspace-number 1))
    (while (<= l-workspace-number eide-custom-number-of-workspaces)
      (let ((l-workspace-dir (concat "~/.emacs-ide/workspace" (number-to-string l-workspace-number))))
        (if (not (file-directory-p l-workspace-dir))
          (make-directory l-workspace-dir)))
      (setq l-workspace-number (+ l-workspace-number 1)))))

(defun eide-project-set-current-workspace (p-workspace-number)
  "Set current workspace.
- p-workspace-number: new workspace number."
  (if (<= p-workspace-number eide-custom-number-of-workspaces)
    (progn
      (setq eide-project-current-workspace p-workspace-number)
      ;; Change projects list file
      (setq eide-project-projects-file (concat "~/.emacs-ide/workspace" (number-to-string p-workspace-number) "/projects-list"))
      ;; Restore initial root directory
      (setq eide-project-name nil)
      (setq eide-root-directory eide-root-directory-at-startup)
      (if (not eide-no-desktop-option)
        (progn
          ;; Clear desktop (even if a project is defined)
          (eide-windows-layout-unbuild)
          (desktop-save-mode -1)
          ;; Close all buffers
          (desktop-clear)
          (eide-menu-update t)
          (eide-windows-layout-build))))))

(defun eide-project-create ()
  "Create a project."
  (if (eide-popup-question-yes-or-no-p (concat "Create a project in " eide-root-directory " ?"))
    (progn
      (eide-windows-select-source-window t)
      ;; Create empty project file
      (shell-command (concat "touch " eide-root-directory eide-project-config-file))
      (eide-project-start-with-project nil t)
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
      (eide-i-project-update-frame-title)
      (eide-menu-update t)
      ;; Update key bindings for project
      (eide-keys-configure-for-editor)
      ;; Remove from projects list
      (eide-i-project-remove-project))))

(defun eide-project-load (p-startup-flag)
  "Load project information (depends on root directory).
- p-startup-flag: t when called from the init."
  ;; Check if a project is defined, and start it.
  ;; NB: It is important to read desktop after mode-hooks have been defined,
  ;; otherwise mode-hooks may not apply.
  (if (file-exists-p (concat eide-root-directory eide-project-config-file))
    (progn
      ;; A project is defined in this directory
      (eide-project-start-with-project p-startup-flag nil)
      ;; When Emacs-IDE is loaded from a file after init ("emacs -l file.el"),
      ;; the desktop is not read, because after-init-hook has already been called.
      ;; In that case, we need to force to read it (except if --no-desktop option is set).
      ;; The solution is to register a hook on emacs-startup-hook, which is
      ;; called after the loading of file.el.
      ;; Drawback: a file in argument ("emacs -l file.el main.c") will be loaded
      ;; but not displayed, because desktop is read after the loading of main.c
      ;; and selects its own current buffer.
      (if (and p-startup-flag (not eide-no-desktop-option))
        (add-hook 'emacs-startup-hook 'eide-i-project-force-desktop-read-hook)))
    (progn
      ;; There is no project in this directory
      (setq eide-project-name nil)
      (if (not eide-no-desktop-option)
        (progn
          (desktop-save-mode -1)
          ;; Close all buffers
          (desktop-clear)))))
  ;; Start with "editor" mode
  (eide-keys-configure-for-editor)
  ;; Kill projects list in case it is present in desktop
  (if (get-buffer eide-project-projects-buffer-name)
    (kill-buffer eide-project-projects-buffer-name))
  ;; Close temporary buffers from ediff sessions (if emacs has been closed during
  ;; an ediff session, .emacs.desktop contains temporary buffers (.ref or .new
  ;; files) and they have been loaded in this new emacs session).
  (let ((l-buffer-name-list (mapcar 'buffer-name (buffer-list))))
    (dolist (l-buffer-name l-buffer-name-list)
      (if (or (string-match "^\* (REF)" l-buffer-name) (string-match "^\* (NEW)" l-buffer-name))
        ;; this is a "useless" buffer (.ref or .new)
        (kill-buffer l-buffer-name))))
  ;; Set current buffer
  (setq eide-current-buffer (buffer-name)))

(defun eide-project-start-with-project (p-startup-flag p-creation-flag)
  "Start with current project.
- p-startup-flag: t when called from the init.
- p-creation-flag: t when the project is created."
  ;; Get project name from directory
  ;; eide-root-directory:                                                     <...>/current_project/
  ;; directory-file-name removes last "/":                                    <...>/current_project
  ;; file-name-nondirectory retrieves last directory name from complete path: current_project
  (setq eide-project-name (file-name-nondirectory (directory-file-name eide-root-directory)))

  ;; "Lock" project
  ;;(shell-command (concat "touch " eide-root-directory eide-project-lock-file))

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

  (if (not (file-exists-p (concat eide-root-directory eide-project-notes-file)))
    ;; Create empty project notes file
    (shell-command (concat "touch " eide-root-directory eide-project-notes-file)))

  (if (not eide-no-desktop-option)
    (progn
      (if (not p-startup-flag)
        ;; No need to update menu for every restored buffer
        (ad-deactivate 'switch-to-buffer))
      (if desktop-dirname
        ;; A desktop is already loaded: switch to the new one.
        ;; desktop-change-dir saves the desktop, close all buffers, and read the new desktop.
        (desktop-change-dir eide-root-directory)
        (progn
          ;; Enable desktop save mode: desktop is read and will be saved automatically on exit.
          (desktop-save-mode 1)
          ;; Desktop must be saved without asking (if .emacs.desktop does not exist)
          (setq desktop-save t)
          ;; Set desktop directory (set to nil when desktop save mode is disabled)
          (setq desktop-dirname eide-root-directory)
          (if (not (or p-startup-flag p-creation-flag))
            (progn
              ;; It is necessary to close all buffers before loading the new desktop.
              (desktop-clear)
              (desktop-read eide-root-directory)))))
      (if (not p-startup-flag)
        (ad-activate 'switch-to-buffer))))

  ;; Update frame title
  (eide-i-project-update-frame-title)

  ;; Close any existing TAGS file, to make sure we will use the right one
  (if (get-buffer "TAGS")
    (kill-buffer "TAGS"))
  ;; Use tags-table-list instead of tags-file-name because when switching to
  ;; another project, Emacs asks either to append or to overwrite tags file
  ;; name in the list, and we want to overwrite without asking
  (setq tags-table-list (list (concat eide-root-directory "TAGS")))

  ;; Set cscope root directory
  (if eide-option-use-cscope-flag
    (cscope-set-initial-directory eide-root-directory))

  ;; Close any existing config file, to make sure we will use the right one
  (if (get-buffer eide-project-config-file)
    (kill-buffer eide-project-config-file))
  ;; Rebuild project file after the desktop has been changed (in case of project switching)
  (eide-config-rebuild-project-file)
  (eide-i-project-add-project p-startup-flag))

(defun eide-project-change-root ()
  "Change root directory."
  (let ((l-do-it t))
    (if (and (not eide-project-name)
             eide-menu-files-list
             (not (eide-popup-question-yes-or-no-p "The list of open files will be lost. Do you want to continue?")))
      (setq l-do-it nil))
    (if l-do-it
      (let ((l-layout-visible-flag eide-windows-is-layout-visible-flag))
        ;; Changing desktop (desktop-change-dir) sometimes unbuild the windows layout!...
        ;; Therefore it is necessary to unbuild it intentionally before loading the new desktop,
        ;; otherwise we get errors for non-existing windows
        (eide-windows-layout-unbuild)
        (call-interactively 'dired)
        ;; Set root directory (expand-file-name replaces ~ with /home/<user>)
        (setq eide-root-directory (expand-file-name default-directory))
        ;; Exit browsing mode (kill dired buffer)
        (eide-menu-browsing-mode-stop)
        (eide-project-load nil)
        (eide-menu-update t)
        (if l-layout-visible-flag
          (eide-windows-layout-build))))))

(defun eide-project-open-list ()
  "Display projects list (full frame)."
  (let ((l-do-it t))
    (if (and (not eide-project-name)
             eide-menu-files-list
             (not (eide-popup-question-yes-or-no-p "The list of open files will be lost. Do you want to continue?")))
      (setq l-do-it nil))
    (if l-do-it
      (progn
        (eide-windows-layout-unbuild)
        (eide-config-set-colors-for-config)
        (eide-keys-configure-for-special-buffer)
        (ad-deactivate 'switch-to-buffer)
        (if (get-buffer eide-project-projects-buffer-name)
          (switch-to-buffer eide-project-projects-buffer-name)
          (progn
            (find-file eide-project-projects-file)
            (rename-buffer eide-project-projects-buffer-name)))
        (goto-char (point-min))
        (while (not (eobp))
          (if (string-equal (buffer-substring-no-properties (point) (line-end-position)) eide-project-name)
            ;; Current project (can't be selected)
            (put-text-property (point) (line-end-position) 'face 'eide-config-project-current-name-face)
            ;; Other projects
            (progn
              (put-text-property (point) (line-end-position) 'keymap project-name-map)
              (put-text-property (point) (line-end-position) 'face 'eide-config-project-name-face)
              (put-text-property (point) (line-end-position) 'mouse-face 'highlight)))
          (forward-line 2))
        ;; Clear modified status (text properties don't need to be saved)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (goto-char (point-min))
        (ad-activate 'switch-to-buffer)))))

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

;; ----------------------------------------------------------------------------
;; KEYMAPS
;; ----------------------------------------------------------------------------

(setq project-name-map (make-sparse-keymap))
(define-key project-name-map [mouse-1] 'eide-i-project-open)

;;; eide-project.el ends here
