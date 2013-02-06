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

(require 'eide-compare)
(require 'eide-config)
(require 'eide-popup)
(require 'eide-search)

;; Check --no-desktop option before it is removed from command-line-args by desktop in after-init-hook
(defvar eide-no-desktop-option nil)
(if (member "--no-desktop" command-line-args)
  (setq eide-no-desktop-option t))

;; expand-file-name replaces ~ with /home/<user>
(defvar eide-root-directory (expand-file-name default-directory))
(defvar eide-root-directory-at-startup eide-root-directory)

;; Test if xcscope is available
(defvar eide-option-use-cscope-flag nil)
(if (locate-library "xcscope")
  (progn
    (require 'xcscope)
    (setq eide-option-use-cscope-flag t)))

(defvar eide-project-config-file ".emacs-ide-project.cfg")
(defvar eide-project-notes-file  ".emacs-ide-project.txt")

(defvar eide-project-current-workspace 1)
(defvar eide-project-current-projects-list nil)

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

(defvar eide-project-comparison-project-point nil)

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

(defun eide-i-project-load (p-startup-flag p-creation-flag)
  "Update environment according to the project in root directory:
- close all files,
- load the project desktop,
- check project information (tags, cscope, configuration),
- add the project (or update its name) in projects list.
Arguments:
- p-startup-flag: t when called from the init.
- p-creation-flag: t when the project is created."
  ;; Get project name from directory
  ;; eide-root-directory:                                                     <...>/current_project/
  ;; directory-file-name removes last "/":                                    <...>/current_project
  ;; file-name-nondirectory retrieves last directory name from complete path: current_project
  (setq eide-project-name (file-name-nondirectory (directory-file-name eide-root-directory)))

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

  ;; Update version control show status
  (eide-vc-update-show-svn-status)
  (eide-vc-update-show-git-status)

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
  (eide-project-rebuild-config-file p-startup-flag)
  ;; Add the project to current workspace
  (eide-project-add-in-list p-startup-flag))

(defun eide-i-project-update-internal-projects-list ()
  ;; Create internal projects list
  (setq eide-project-current-projects-list nil)
  (save-current-buffer
    (set-buffer (find-file-noselect eide-project-projects-file))
    (goto-char (point-min))
    (forward-line)
    (while (not (eobp))
      (push (buffer-substring-no-properties (point) (line-end-position)) eide-project-current-projects-list)
      (forward-line 2))
    (kill-this-buffer)))

(defun eide-i-project-open-selected-project ()
  "Open project on current line."
  (interactive)
  (if (or (not eide-project-name) (and eide-search-tags-available-flag eide-search-cscope-available-flag))
    (let ((l-project-dir (progn (beginning-of-line) (forward-line) (buffer-substring-no-properties (point) (line-end-position)))))
      (if (file-directory-p l-project-dir)
        (progn
          ;; Close projects list (so that it can be modified by another Emacs session)
          (kill-this-buffer)
          ;; Restore editor configuration
          (eide-windows-set-colors-for-files)
          (eide-keys-configure-for-editor)
          (if (not (string-equal l-project-dir eide-root-directory))
            (progn
              ;; Changing desktop (desktop-change-dir) sometimes unbuild the windows layout!...
              ;; Therefore it is necessary to unbuild it intentionally before loading the new desktop,
              ;; otherwise we get errors for non-existing windows
              (eide-windows-layout-unbuild)
              ;; Set root directory
              (setq eide-root-directory l-project-dir)
              (eide-project-load-root-directory-content nil)
              (eide-menu-update t)))
          (eide-windows-layout-build))
        (if (eide-popup-question-yes-or-no-p "This directory does not exist anymore... Do you want to remove this project from current workspace?")
          (let ((buffer-read-only nil))
            (setq eide-project-current-projects-list (remove l-project-dir eide-project-current-projects-list))
            (if (string-equal l-project-dir eide-compare-other-project-directory)
              (progn
                ;; Clear the project selected for comparison
                (setq eide-compare-other-project-name nil)
                (setq eide-compare-other-project-directory nil)))
            (forward-line -1)
            (delete-region (point) (progn (forward-line 2) (point)))
            (ad-deactivate 'save-buffer)
            (save-buffer)
            (ad-activate 'save-buffer)))))
    (eide-popup-message "Please wait for tags and cscope list of files to be created...")))

(defun eide-i-project-set-colors-for-config ()
  "Set colors for config buffer."
  (set-background-color eide-config-config-background-color)
  (set-foreground-color eide-config-config-foreground-color)
  (set-face-background 'fringe eide-config-config-background-color))

(defun eide-i-project-get-config-value-if-defined (p-parameter)
  "Get the value of a parameter in a config (current buffer), returns nil if
not defined.
Argument:
- p-parameter: config parameter."
  (goto-char (point-min))
  (if (re-search-forward (concat "^" p-parameter " = ") nil t)
    (buffer-substring-no-properties (point) (line-end-position))
    nil))

(defun eide-i-project-rebuild-config-line (p-parameter p-default-value)
  "Update a line with a parameter and its value (use default value if not
found).
Arguments:
- p-parameter: config parameter.
- p-default-value: config default value."
  (let ((l-value nil))
    (save-current-buffer
      (set-buffer eide-project-config-file)
      (setq l-value (eide-i-project-get-config-value-if-defined p-parameter)))
    (if (not l-value)
      (setq l-value p-default-value))
    (insert p-parameter)
    (insert " = ")
    (insert l-value)
    (insert "\n")))

(defun eide-i-project-compile (p-parameter)
  "Compile project.
Argument:
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
  (goto-char (point-max))
  (eide-windows-select-source-window t))

(defun eide-i-project-run (p-parameter)
  "Run project.
Argument:
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
Argument:
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

(defun eide-project-create-workspaces ()
  "Create directories and files for workspaces, if missing."
  (let ((l-workspace-number 1))
    (while (<= l-workspace-number eide-custom-number-of-workspaces)
      (let ((l-workspace-dir nil) (l-projects-list-file nil))
        (setq l-workspace-dir (concat "~/.emacs-ide/workspace" (number-to-string l-workspace-number)))
        ;; "touch" command requires expand-file-name (which replaces ~ with /home/<user>)
        (setq l-projects-list-file (expand-file-name (concat l-workspace-dir "/projects-list")))
        (if (not (file-directory-p l-workspace-dir))
          (make-directory l-workspace-dir))
        (if (not (file-exists-p l-projects-list-file))
          (shell-command (concat "touch \"" l-projects-list-file "\""))))
      (setq l-workspace-number (+ l-workspace-number 1))))
  (eide-i-project-update-internal-projects-list))

(defun eide-project-set-current-workspace (p-workspace-number)
  "Set current workspace.
Argument:
- p-workspace-number: new workspace number."
  (if (or (not eide-project-name) (and eide-search-tags-available-flag eide-search-cscope-available-flag))
    (if (<= p-workspace-number eide-custom-number-of-workspaces)
      (progn
        (setq eide-project-current-workspace p-workspace-number)
        ;; Change projects list file
        (setq eide-project-projects-file (concat "~/.emacs-ide/workspace" (number-to-string p-workspace-number) "/projects-list"))
        ;; Restore initial root directory
        (setq eide-project-name nil)
        (setq eide-root-directory eide-root-directory-at-startup)
        ;; Clear the project selected for comparison
        (setq eide-compare-other-project-name nil)
        (setq eide-compare-other-project-directory nil)
        (if (not eide-no-desktop-option)
          (progn
            ;; Clear desktop (even if a project is defined)
            (eide-windows-layout-unbuild)
            (desktop-save-mode -1)
            ;; Close all buffers
            (desktop-clear)
            (setq desktop-dirname nil)
            (eide-menu-update t)
            (eide-windows-layout-build)))
        (eide-i-project-update-internal-projects-list)
        ;; Update default directory if current buffer is not visiting a file
        (if (not buffer-file-name)
          (setq default-directory eide-root-directory))))
    (eide-popup-message "Please wait for tags and cscope list of files to be created...")))

(defun eide-project-create ()
  "Create a project in root directory, and add it in projects list."
  (if (eide-popup-question-yes-or-no-p (concat "Create a project in " eide-root-directory " ?"))
    (progn
      (eide-windows-select-source-window t)
      ;; Create empty project file
      (shell-command (concat "touch " eide-root-directory eide-project-config-file))
      (eide-i-project-load nil t)
      ;; Update frame title
      (eide-i-project-update-frame-title)
      ;; Update project name in menu
      (eide-menu-update-project-name)
      ;; Update key bindings for project
      (eide-keys-configure-for-editor))))

(defun eide-project-delete ()
  "Delete current project."
  (if (eide-popup-question-yes-or-no-p (concat "Delete project in " eide-root-directory " ?"))
    (progn
      ;; Stop creation of tags and cscope list of files (in case it is not finished yet)
      (if (not eide-search-tags-available-flag)
        (delete-process "create-tags"))
      (if (not eide-search-cscope-available-flag)
        (delete-process "create-cscope"))
      (setq eide-search-tags-available-flag nil)
      (setq eide-search-cscope-available-flag nil)
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
      (setq desktop-dirname nil)
      ;; Update frame title and menu (project is inactive now)
      (eide-i-project-update-frame-title)
      (eide-menu-update t)
      ;; Update key bindings for project
      (eide-keys-configure-for-editor)
      ;; Remove from projects list
      (eide-project-remove-from-list))))

(defun eide-project-load-root-directory-content (p-startup-flag)
  "Update environment according to root directory content.
If a project is defined:
- close all files,
- load the project desktop,
- check project information (tags, cscope, configuration),
- add the project (or update its name) in projects list,
- update the display.
Otherwise:
- close all files,
- disable the desktop,
- update the display.
Argument:
- p-startup-flag: t when called from the init."
  ;; Check if a project is defined, and start it.
  ;; NB: It is important to read desktop after mode-hooks have been defined,
  ;; otherwise mode-hooks may not apply.
  (if (file-exists-p (concat eide-root-directory eide-project-config-file))
    (progn
      ;; A project is defined in this directory
      (eide-i-project-load p-startup-flag nil)
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
          (desktop-clear)
          (setq desktop-dirname nil)))))
  ;; Update frame title
  (eide-i-project-update-frame-title)
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
  ;; Update default directory if current buffer is not visiting a file
  (if (not buffer-file-name)
    (setq default-directory eide-root-directory))
  ;; Set current buffer
  (setq eide-current-buffer (buffer-name)))

(defun eide-project-change-root ()
  "Change root directory."
  (if (or (not eide-project-name) (and eide-search-tags-available-flag eide-search-cscope-available-flag))
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
          (eide-project-load-root-directory-content nil)
          (eide-menu-update t)
          (if l-layout-visible-flag
            (eide-windows-layout-build)))))
    (eide-popup-message "Please wait for tags and cscope list of files to be created...")))

(defun eide-project-open-list ()
  "Display projects list (full frame), and rebuild internal projects list."
  (let ((l-do-it t))
    (if (and (not eide-project-name)
             eide-menu-files-list
             (not (eide-popup-question-yes-or-no-p "The list of open files will be lost if you select a project. Do you want to continue?")))
      (setq l-do-it nil))
    (if l-do-it
      (progn
        ;; The internal projects list will also be rebuilt
        (setq eide-project-current-projects-list nil)
        (setq eide-project-comparison-project-point nil)
        (eide-windows-layout-unbuild)
        (eide-i-project-set-colors-for-config)
        (eide-keys-configure-for-special-buffer)
        (ad-deactivate 'switch-to-buffer)
        (if (get-buffer eide-project-projects-buffer-name)
          (switch-to-buffer eide-project-projects-buffer-name)
          (progn
            (find-file eide-project-projects-file)
            (rename-buffer eide-project-projects-buffer-name)))
        (goto-char (point-min))
        (forward-line)
        (while (not (eobp))
          (let ((l-project-dir (buffer-substring-no-properties (point) (line-end-position))))
            (forward-line -1)
            (if (string-equal l-project-dir eide-root-directory)
              ;; Current project (can't be selected)
              (put-text-property (point) (line-end-position) 'face 'eide-config-project-current-name-face)
              (if (and eide-compare-other-project-name
                       (string-equal l-project-dir eide-compare-other-project-directory))
                ;; Project selected for comparison
                (progn
                  (setq eide-project-comparison-project-point (point))
                  (put-text-property (point) (line-end-position) 'face 'eide-config-project-comparison-name-face))
                ;; Other projects
                (put-text-property (point) (line-end-position) 'face 'eide-config-project-name-face)))
            (put-text-property (point) (line-end-position) 'keymap project-name-map)
            (put-text-property (point) (line-end-position) 'mouse-face 'highlight)
            (push l-project-dir eide-project-current-projects-list)
            (forward-line 3)))
        ;; Clear modified status (text properties don't need to be saved)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (goto-char (point-min))
        (ad-activate 'switch-to-buffer)))))

(defun eide-project-add-in-list (p-startup-flag)
  "Add current project to the projects list of current workspace.
Argument:
- p-startup-flag: t when called from the init."
  (save-current-buffer
    (if (get-buffer eide-project-projects-buffer-name)
      (progn
        (set-buffer eide-project-projects-buffer-name)
        (setq buffer-read-only nil))
      (set-buffer (find-file-noselect eide-project-projects-file)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" eide-root-directory "$") nil t)
      (progn
        ;; This project is already in the list
        (forward-line -1)
        (if (not (string-equal eide-project-name (buffer-substring-no-properties (point) (line-end-position))))
          (progn
            ;; Update the project name
            (delete-region (point) (line-end-position))
            (put-text-property (point) (progn (insert eide-project-name) (point)) 'face 'eide-config-project-current-name-face)
            (if (not p-startup-flag)
              (ad-deactivate 'save-buffer))
            (save-buffer)
            (if (not p-startup-flag)
              (ad-activate 'save-buffer)))))
      (progn
        ;; This project is not in the list: let's insert it in the right place
        ;; (root directories in alphabetical order)
        (goto-char (point-min))
        (forward-line)
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
    (kill-this-buffer))
  (push eide-root-directory eide-project-current-projects-list))

(defun eide-project-remove-from-list ()
  "Remove current project from the projects list of current workspace."
  (save-current-buffer
    (if (get-buffer eide-project-projects-buffer-name)
      (progn
        (set-buffer eide-project-projects-buffer-name)
        (setq buffer-read-only nil))
      (set-buffer (find-file-noselect eide-project-projects-file)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" eide-root-directory "$") nil t)
      (progn
        (forward-line -1)
        (delete-region (point) (progn (forward-line 2) (point)))
        (ad-deactivate 'save-buffer)
        (save-buffer)
        (ad-activate 'save-buffer)))
    (kill-this-buffer))
  (setq eide-project-current-projects-list (remove eide-root-directory eide-project-current-projects-list))
  (if (string-equal eide-root-directory eide-compare-other-project-directory)
    (progn
      ;; Clear the project selected for comparison
      (setq eide-compare-other-project-name nil)
      (setq eide-compare-other-project-directory nil))))

(defun eide-project-update-name ()
  "Update current project name in frame title and in the projects list of
current workspace."
  ;; Update frame title
  (eide-i-project-update-frame-title)
  ;; Update projects list
  (save-current-buffer
    (if (get-buffer eide-project-projects-buffer-name)
      (progn
        (set-buffer eide-project-projects-buffer-name)
        (setq buffer-read-only nil))
      (set-buffer (find-file-noselect eide-project-projects-file)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" eide-root-directory "$") nil t)
      (progn
        (forward-line -1)
        (delete-region (point) (line-end-position))
        (put-text-property (point) (progn (insert eide-project-name) (point)) 'face 'eide-config-project-current-name-face)
        (ad-deactivate 'save-buffer)
        (save-buffer)
        (ad-activate 'save-buffer)))
    (kill-this-buffer)))

(defun eide-project-remove-selected-project ()
  "Remove the project on current line from current workspace."
  (let ((buffer-read-only nil))
    (forward-line)
    (let ((l-project-dir (buffer-substring-no-properties (point) (line-end-position))))
      (setq eide-project-current-projects-list (remove l-project-dir eide-project-current-projects-list))
      (if (string-equal l-project-dir eide-compare-other-project-directory)
        (progn
          ;; Clear the project selected for comparison
          (setq eide-compare-other-project-name nil)
          (setq eide-compare-other-project-directory nil))))
    (forward-line -1)
    (delete-region (point) (progn (forward-line 2) (point)))
    (ad-deactivate 'save-buffer)
    (save-buffer)
    (ad-activate 'save-buffer)))

(defun eide-project-select-for-comparison ()
  "Select project on current line for comparison."
  (let ((buffer-read-only nil) (l-project-name nil) (l-project-dir nil))
    (setq l-project-name (buffer-substring-no-properties (point) (line-end-position)))
    (forward-line)
    (setq l-project-dir (buffer-substring-no-properties (point) (line-end-position)))
    (eide-compare-select-another-project l-project-name l-project-dir)
    (forward-line -1)
    (let ((l-new-point (point)))
      (if (not (string-equal l-project-dir eide-root-directory))
        ;; Highlight selected project
        (put-text-property (point) (line-end-position) 'face 'eide-config-project-comparison-name-face))
      (if eide-project-comparison-project-point
        ;; Clear previous selected project
        (progn
          (goto-char eide-project-comparison-project-point)
          (forward-line)
          (let ((l-old-project-dir (buffer-substring-no-properties (point) (line-end-position))))
            (forward-line -1)
            (if (string-equal l-old-project-dir eide-root-directory)
              (put-text-property (point) (line-end-position) 'face 'eide-config-project-current-name-face)
              (put-text-property (point) (line-end-position) 'face 'eide-config-project-name-face)))))
      (setq eide-project-comparison-project-point l-new-point))
    ;; Clear modified status (text properties don't need to be saved)
    (set-buffer-modified-p nil)))

(defun eide-project-rebuild-config-file (p-startup-flag)
  "Update project file.
Argument:
- p-startup-flag: t when called from the init."
  (save-current-buffer
    ;; Define target config file
    (setq eide-config-target-buffer (concat eide-project-config-file "_temp"))

    ;; Open these config files
    (if (not (get-buffer eide-project-config-file))
      (find-file-noselect (concat eide-root-directory eide-project-config-file)))
    (get-buffer-create eide-config-target-buffer)
    (set-buffer eide-config-target-buffer)
    (erase-buffer)

    (insert "# *****************************************************************************\n")
    (insert "# Emacs-IDE project configuration\n")
    (insert "# *****************************************************************************\n\n")
    (insert "# --> Click right to exit this page.\n")
    (insert "# --> To restore the default value of a parameter, delete the line\n")
    (insert "#     (project configuration file is rebuilt when you exit this page).\n\n")

    (let ((l-project-name nil) (l-project-name-has-changed-flag nil))
      (save-current-buffer
        (set-buffer eide-project-config-file)
        (setq l-project-name (eide-i-project-get-config-value-if-defined "project_name")))
      (if (or (not l-project-name) (string-equal l-project-name ""))
        ;; Get project name from directory:
        ;; - directory-file-name removes last "/"
        ;; - file-name-nondirectory retrieves last directory name from complete path
        (setq l-project-name (file-name-nondirectory (directory-file-name eide-root-directory))))
      (insert "project_name = ")
      (insert l-project-name)
      (insert "\n\n")

      (if (not (and eide-project-name (string-equal eide-project-name l-project-name)))
        (progn
          (setq eide-project-name l-project-name)
          (setq l-project-name-has-changed-flag t)))

      (insert "# Init command is called before all 'compile' and 'run' commands.\n")
      (eide-i-project-rebuild-config-line "init_command"      eide-custom-project-default-init-command)
      (eide-i-project-rebuild-config-line "compile_command_1" eide-custom-project-default-compile-command-1)
      (eide-i-project-rebuild-config-line "compile_command_2" eide-custom-project-default-compile-command-2)
      (eide-i-project-rebuild-config-line "compile_command_3" eide-custom-project-default-compile-command-3)
      (eide-i-project-rebuild-config-line "compile_command_4" eide-custom-project-default-compile-command-4)
      (eide-i-project-rebuild-config-line "run_command_1"     eide-custom-project-default-run-command-1)
      (eide-i-project-rebuild-config-line "run_command_2"     eide-custom-project-default-run-command-1)
      (eide-i-project-rebuild-config-line "debug_command"     eide-custom-project-default-debug-command)
      (eide-i-project-rebuild-config-line "debug_program_1"   eide-custom-project-default-debug-program-1)
      (eide-i-project-rebuild-config-line "debug_program_2"   eide-custom-project-default-debug-program-2)

      ;; Replace source file by target buffer if different
      (if (not (equal (compare-buffer-substrings eide-project-config-file nil nil eide-config-target-buffer nil nil) 0))
        (progn
          (set-buffer eide-project-config-file)
          (erase-buffer)
          (insert-buffer-substring eide-config-target-buffer)
          (if (not p-startup-flag)
            (ad-deactivate 'save-buffer))
          (save-buffer)
          (if (not p-startup-flag)
            (ad-activate 'save-buffer))))
      ;; Close temporary buffer
      (kill-buffer eide-config-target-buffer)
      ;; Return t if the project name has changed, nil otherwise
      l-project-name-has-changed-flag)))

(defun eide-project-get-config-value (p-parameter)
  "Get the value of a parameter in project config (empty string if not defined).
Argument:
- p-parameter: config parameter."
  (save-current-buffer
    (if (not (get-buffer eide-project-config-file))
      (find-file-noselect (concat eide-root-directory eide-project-config-file)))
    (set-buffer eide-project-config-file)
    (let ((l-value (eide-i-project-get-config-value-if-defined p-parameter)))
      (if l-value
        l-value
        ""))))

(defun eide-project-open-config-file ()
  "Display project file (full frame)."
  (eide-windows-layout-unbuild)
  (eide-i-project-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-windows-find-file-without-advice (concat eide-root-directory eide-project-config-file))
  (goto-char (point-min)))

(defun eide-project-open-notes-file ()
  "Display project notes file (full frame)."
  (eide-windows-layout-unbuild)
  (eide-i-project-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-windows-find-file-without-advice (concat eide-root-directory eide-project-notes-file)))

(defun eide-project-get-full-command (p-parameter)
  "Get full command (init command + compile/run command).
Argument:
- p-parameter: option parameter in project configuration."
  (let ((l-init-command (eide-project-get-config-value "init_command")))
    (if (string-equal l-init-command "")
      (eide-project-get-config-value p-parameter)
      (concat l-init-command " ; " (eide-project-get-config-value p-parameter)))))

(defun eide-project-get-full-gdb-command (p-program)
  "Get full gdb command (gdb command + gdb option + program name).
Argument:
- p-program: option parameter in project configuration for gdb program."
  (concat (eide-project-get-config-value "debug_command") eide-project-gdb-option (eide-project-get-config-value p-program)))

(defun eide-project-get-short-gdb-command (p-program)
  "Get short gdb command (short gdb command + gdb option + program name) for popup
menu (hide gdb command path).
Argument:
- p-program: option parameter in project configuration for gdb program."
  (let ((l-gdb-command (eide-project-get-config-value "debug_command")) (l-short-gdb-command nil))
    (if (string-match "/" l-gdb-command)
      (setq l-short-gdb-command (concat "[...]/" (car (last (split-string l-gdb-command "/")))))
      (setq l-short-gdb-command l-gdb-command))
    (concat l-short-gdb-command eide-project-gdb-option (eide-project-get-config-value p-program))))

(defun eide-project-get-short-directory (p-directory)
  "Get the path relative to project root directory from absolute path if it is
part of the project (remove root directory from absolute path).
Argument:
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
  (eide-windows-set-colors-for-files)
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
(define-key project-name-map [mouse-1] 'eide-i-project-open-selected-project)
(define-key project-name-map [mouse-3] 'eide-popup-open-menu-for-project)

;;; eide-project.el ends here
