;;; eide-popup.el --- Emacs-IDE, popup

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

(provide 'eide-popup)

(require 'eide-compare) ; for eide-compare-other-projects-list

(defvar eide-popup-menu nil)
(defvar eide-popup-menu-actions-list nil)
(defvar eide-popup-menu-separator-flag nil)

(setq eide-confirm-dialog
      '(("yes" . "y")
        ("no"  . "n")))

(setq eide-message-dialog
      '(("continue" . "c")))

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Initialize a popup menu.
;;
;; output : eide-popup-menu : empty menu.
;;          eide-popup-menu-actions-list : empty actions list.
;; ----------------------------------------------------------------------------
(defun eide-i-popup-menu-init ()
  (setq eide-popup-menu nil)
  (setq eide-popup-menu-actions-list nil)
  (if (not eide-option-menu-buffer-popup-groups-flags)
    (setq eide-popup-menu-separator-flag nil)))

;; ----------------------------------------------------------------------------
;; Add an action in action list (for popup menu).
;;
;; input  : p-action-name : action name in menu.
;;          p-action-function : action function.
;;          p-enabled-flag : t if this action is enabled.
;;          eide-popup-menu-actions-list : actions list.
;; output : eide-popup-menu-actions-list : updated actions list.
;; ----------------------------------------------------------------------------
(defun eide-i-popup-menu-add-action (p-action-name p-action-function p-enabled-flag)
  (if (> (length p-action-name) 120)
    (setq p-action-name (concat (substring p-action-name 0 120) " [...]")))
  (if p-enabled-flag
    (setq eide-popup-menu-actions-list (append (list (cons p-action-name p-action-function)) eide-popup-menu-actions-list))
    (setq eide-popup-menu-actions-list (append (list p-action-name) eide-popup-menu-actions-list))))

;; ----------------------------------------------------------------------------
;; Add action list to popup menu.
;;
;; input  : p-actions-list-name : name of actions list.
;;          eide-popup-menu : popup menu.
;;          eide-popup-menu-actions-list : actions list.
;; output : eide-popup-menu : updated popup menu.
;;          eide-popup-menu-actions-list : empty actions list.
;; ----------------------------------------------------------------------------
(defun eide-i-popup-menu-close-action-list (p-actions-list-name)
  (if eide-popup-menu-actions-list
    (if eide-option-menu-buffer-popup-groups-flags
      (setq eide-popup-menu (append (list (cons p-actions-list-name eide-popup-menu-actions-list)) eide-popup-menu))
      (progn
        (if eide-popup-menu-separator-flag
          (setq eide-popup-menu (append (list (cons "-" "-")) eide-popup-menu))
          ;;(setq eide-popup-menu (append (append (list (cons "-" "-")) eide-popup-menu-actions-list) eide-popup-menu))
          (setq eide-popup-menu-separator-flag t))
        (setq eide-popup-menu (append eide-popup-menu-actions-list eide-popup-menu)))))
  (setq eide-popup-menu-actions-list nil))

;; ----------------------------------------------------------------------------
;; Open popup menu.
;;
;; input  : p-menu-title : title of popup menu.
;;          eide-popup-menu : popup menu.
;; ----------------------------------------------------------------------------
(defun eide-i-popup-menu-open (p-menu-title)
  (if eide-popup-menu
    (progn
      (setq eide-popup-menu (reverse eide-popup-menu))

      (if (not eide-option-menu-buffer-popup-groups-flags)
        (setq eide-popup-menu (list (cons "single group" eide-popup-menu))))

      (let ((l-result (x-popup-menu t (cons p-menu-title eide-popup-menu))))
        (if (bufferp l-result)
          (switch-to-buffer l-result)
          (eval (car (read-from-string l-result))))))))

;; ----------------------------------------------------------------------------
;; Open popup menu with the list of other projects.
;;
;; input  : eide-compare-other-projects-list : other projects list.
;; ----------------------------------------------------------------------------
(defun eide-i-popup-open-menu-for-another-project ()
  (eide-compare-build-other-projects-list)
  (if eide-compare-other-projects-list
    (progn
      (eide-i-popup-menu-init)
      (dolist (l-project eide-compare-other-projects-list)
        (eide-i-popup-menu-add-action (car l-project) (concat "(eide-compare-select-another-project \"" (car l-project) "\" \"" (cdr l-project) "\")") t))
      (eide-i-popup-menu-close-action-list "Other projects")
      (eide-i-popup-menu-open "Select another project:"))
    ;; eide-root-directory:                             <...>/current_project/
    ;; directory-file-name removes last "/":            <...>/current_project
    ;; file-name-directory removes last directory name: <...>/
    (eide-popup-message (concat "There is no other project in " (file-name-directory (directory-file-name eide-root-directory))))))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Prompt for a confirmation.
;;
;; input  : p-string : question to be answered yes or no.
;; return : t = "yes", nil = "no".
;; ----------------------------------------------------------------------------
(defun eide-popup-question-yes-or-no-p (p-string)
  ;;(yes-or-no-p p-string))
  (string-equal (x-popup-dialog t (cons p-string eide-confirm-dialog)) "y"))

;; ----------------------------------------------------------------------------
;; Display a message.
;;
;; input  : p-string : message.
;; ----------------------------------------------------------------------------
(defun eide-popup-message (p-string)
  (x-popup-dialog t (cons p-string eide-message-dialog)))

;; ----------------------------------------------------------------------------
;; Open a popup menu related to project.
;;
;; input  : eide-project-name : project name.
;;          eide-compare-other-project-name : other project name (for
;;              comparison).
;;          eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu ()
  (let ((popup-header ""))
    (eide-i-popup-menu-init)
    (if eide-project-name
      ;; Project already created
      (progn
        (if (not (string-equal (eide-config-get-project-value "compile_command_1") ""))
          (eide-i-popup-menu-add-action (concat "Compile (1): " (eide-project-get-full-command "compile_command_1")) "(eide-project-compile-1)" t))
        (if (not (string-equal (eide-config-get-project-value "compile_command_2") ""))
          (eide-i-popup-menu-add-action (concat "Compile (2): " (eide-project-get-full-command "compile_command_2")) "(eide-project-compile-2)" t))
        (if (not (string-equal (eide-config-get-project-value "compile_command_3") ""))
          (eide-i-popup-menu-add-action (concat "Compile (3): " (eide-project-get-full-command "compile_command_3")) "(eide-project-compile-3)" t))
        (if (not (string-equal (eide-config-get-project-value "compile_command_4") ""))
          (eide-i-popup-menu-add-action (concat "Compile (4): " (eide-project-get-full-command "compile_command_4")) "(eide-project-compile-4)" t))
        (if (not (string-equal (eide-config-get-project-value "run_command_1") ""))
          (eide-i-popup-menu-add-action (concat "Run (1): " (eide-project-get-full-command "run_command_1")) "(eide-project-run-1)" t))
        (if (not (string-equal (eide-config-get-project-value "run_command_2") ""))
          (eide-i-popup-menu-add-action (concat "Run (2): " (eide-project-get-full-command "run_command_2")) "(eide-project-run-2)" t))
        (if (not (string-equal (eide-config-get-project-value "debug_program_1") ""))
          (eide-i-popup-menu-add-action (concat "Debug (1): " (eide-project-get-short-gdb-command "debug_program_1")) "(eide-project-debug-1)" t))
        (if (not (string-equal (eide-config-get-project-value "debug_program_2") ""))
          (eide-i-popup-menu-add-action (concat "Debug (2): " (eide-project-get-short-gdb-command "debug_program_2")) "(eide-project-debug-2)" t))
        (eide-i-popup-menu-close-action-list "Execute")
        (if (or (not eide-option-use-cscope-flag) eide-option-use-cscope-and-tags-flag)
          (eide-i-popup-menu-add-action "Update tags" "(eide-project-update-tags)" t))
        (if eide-option-use-cscope-flag
          (eide-i-popup-menu-add-action "Update cscope list of files" "(eide-project-update-cscope-list-of-files)" t))
        (eide-i-popup-menu-close-action-list "Update")
        (if eide-compare-other-project-name
          (eide-i-popup-menu-add-action (concat "Select another project for comparison (current: \"" eide-compare-other-project-name "\")") "(eide-i-popup-open-menu-for-another-project)" t)
          (eide-i-popup-menu-add-action "Select another project for comparison" "(eide-i-popup-open-menu-for-another-project)" t))
        (eide-i-popup-menu-close-action-list "Projects comparison")
        (eide-i-popup-menu-add-action "Project configuration" "(eide-config-open-project-file)" t)
        (eide-i-popup-menu-add-action "Project notes" "(eide-config-open-project-notes-file)" t)
        (eide-i-popup-menu-close-action-list "Configuration")
        (eide-i-popup-menu-add-action "Delete project" "(eide-project-delete)" t)
        (eide-i-popup-menu-close-action-list "Destroy")
        (setq popup-header (concat "Project: " eide-project-name)))
      ;; Project not created yet
      (progn
        (eide-i-popup-menu-add-action "Create project" "(eide-project-create)" t)
        (eide-i-popup-menu-close-action-list "Create")
        (setq popup-header (concat "Root directory: " eide-root-directory))))

    (eide-i-popup-menu-add-action "Options" "(eide-config-open-options-file)" t)
    (eide-i-popup-menu-close-action-list "User config")

    (eide-i-popup-menu-add-action "Help" "(eide-help-open)" t)
    (eide-i-popup-menu-close-action-list "About")

    (eide-i-popup-menu-open popup-header)))

;; ----------------------------------------------------------------------------
;; Open a popup menu related to selected directory.
;;
;; input  : eide-menu-files-list : list of opened files.
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-directory ()
  (interactive)
  (eide-windows-select-menu-window)
  (move-to-window-line (cdr (last (mouse-position))))

  (let ((l-directory-name-in-title (eide-menu-get-directory-name-on-current-line)) (l-directory-name nil))
    (setq l-directory-name (if (string-equal l-directory-name-in-title "./")
                             ""
                             l-directory-name-in-title))
    (eide-i-popup-menu-init)
    (eide-i-popup-menu-add-action "Close all files from this directory" (concat "(eide-menu-directory-close \"" l-directory-name "\")") t)

    (let ((l-buffer-read-only-flag nil) (l-buffer-read-write-flag nil) (l-buffer-status-none-flag nil) (l-buffer-status-new-flag nil) (l-buffer-status-ref-flag nil) (l-buffer-svn-modified-flag nil) (l-svn-modified-files-list-string ""))
      ;; Parse list of opened buffers, and find the ones located in this
      ;; directory, to check, for every possible property (read only, REF file,
      ;; ...) if at least one of them matches.
      (dolist (l-buffer eide-menu-files-list)
        (if (eide-menu-is-file-in-directory-p l-buffer l-directory-name)
          ;; The buffer is located in the directory
          (save-excursion
            (set-buffer l-buffer)
            (if (not (string-equal eide-menu-local-edit-status "nofile"))
              (progn
                ;; Check all properties
                (if buffer-read-only
                  (setq l-buffer-read-only-flag t)
                  (setq l-buffer-read-write-flag t))
                (if (string-equal eide-menu-local-edit-status "")
                  (setq l-buffer-status-none-flag t)
                  (if (string-equal eide-menu-local-edit-status "new")
                    (setq l-buffer-status-new-flag t)
                    (if (string-equal eide-menu-local-edit-status "ref")
                      (setq l-buffer-status-ref-flag t))))
                (if (and eide-config-show-svn-status-flag eide-menu-local-svn-modified-status-flag)
                  (progn
                    (setq l-buffer-svn-modified-flag t)
                    ;; Get file name from buffer name (remove <n> if present)
                    (let ((l-index (string-match "<[0-9]+>$" l-buffer)) (l-file-name nil))
                      (if l-index
                        (setq l-file-name (substring l-buffer 0 l-index))
                        (setq l-file-name l-buffer))
                      (setq l-svn-modified-files-list-string (concat l-svn-modified-files-list-string " " l-file-name))))))))))
      ;; Actions are enabled only if it can apply to one buffer at least
      (eide-i-popup-menu-add-action "Set all files read/write" (concat "(eide-edit-action-on-directory 'eide-edit-set-rw \"" l-directory-name "\")") l-buffer-read-only-flag)
      (eide-i-popup-menu-add-action "Set all files read only" (concat "(eide-edit-action-on-directory 'eide-edit-set-r \"" l-directory-name "\")") l-buffer-read-write-flag)
      (eide-i-popup-menu-add-action "Backup original files (REF) to work on copies (NEW)" (concat "(eide-edit-action-on-directory 'eide-edit-make-ref-file \"" l-directory-name "\")") l-buffer-status-none-flag)
      (eide-i-popup-menu-add-action "Switch to REF files" (concat "(eide-edit-action-on-directory 'eide-edit-use-ref-file \"" l-directory-name "\")") l-buffer-status-new-flag)
      (eide-i-popup-menu-add-action "Discard REF files" (concat "(eide-edit-action-on-directory 'eide-edit-discard-ref-file \"" l-directory-name "\" \"discard all REF files\")") l-buffer-status-new-flag)
      (eide-i-popup-menu-add-action "Restore REF files" (concat "(eide-edit-action-on-directory 'eide-edit-restore-ref-file \"" l-directory-name "\" \"restore all REF files\")") l-buffer-status-new-flag)
      (eide-i-popup-menu-add-action "Switch to NEW files" (concat "(eide-edit-action-on-directory 'eide-edit-use-new-file \"" l-directory-name "\")") l-buffer-status-ref-flag)
      (eide-i-popup-menu-add-action "Discard NEW files" (concat "(eide-edit-action-on-directory 'eide-edit-discard-new-file \"" l-directory-name "\" \"discard all NEW files\")") l-buffer-status-ref-flag)
      (eide-i-popup-menu-close-action-list "Edit")

      (eide-i-popup-menu-add-action "Untabify and indent all read/write files" (concat "(eide-edit-action-on-directory 'eide-edit-untabify-and-indent \"" l-directory-name "\" \"untabify and indent all read/write files\")") l-buffer-read-write-flag)
      (eide-i-popup-menu-add-action "Delete trailing spaces in all read/write files" (concat "(eide-edit-action-on-directory 'eide-edit-delete-trailing-spaces \"" l-directory-name "\" \"delete trailing spaces in all read/write files\")") l-buffer-read-write-flag)
      (eide-i-popup-menu-add-action "Convert end of line in all read/write files: DOS to UNIX" (concat "(eide-edit-action-on-directory 'eide-edit-dos-to-unix \"" l-directory-name "\" \"convert end of line (DOS to UNIX) in all read/write files\")") l-buffer-read-write-flag)
      (eide-i-popup-menu-add-action "Convert end of line in all read/write files: UNIX to DOS" (concat "(eide-edit-action-on-directory 'eide-edit-unix-to-dos \"" l-directory-name "\" \"convert end of line (UNIX to DOS) in all read/write files\")") l-buffer-read-write-flag)
      (eide-i-popup-menu-close-action-list "Clean")

      (if eide-config-show-svn-status-flag
        (progn
          (eide-i-popup-menu-add-action "svn diff" (concat "(eide-svn-diff-files-in-directory \"" l-directory-name "\" \"" l-svn-modified-files-list-string "\")") l-buffer-svn-modified-flag)
          (eide-i-popup-menu-add-action "svn revert (all modified files)" (concat "(eide-edit-action-on-directory 'eide-svn-revert \"" l-directory-name "\" \"revert all modified files\")") l-buffer-svn-modified-flag)
          (eide-i-popup-menu-close-action-list "svn"))))

    (eide-i-popup-menu-open l-directory-name-in-title)))

;; ----------------------------------------------------------------------------
;; Open a popup menu related to selected file.
;;
;; input  : eide-compare-other-project-name : other project name (for
;;              comparison).
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-file ()
  (interactive)
  (eide-windows-select-menu-window)
  (move-to-window-line (cdr (last (mouse-position))))

  (let ((l-buffer (eide-menu-get-buffer-name-on-current-line))
        (l-buffer-status nil) (l-buffer-rw-flag t) (l-buffer-svn-modified-flag nil))
    (eide-i-popup-menu-init)

    (save-excursion
      (set-buffer l-buffer)
      (setq l-buffer-status eide-menu-local-edit-status)

      ;; Check buffer status (r/w)
      (if buffer-read-only
        (setq l-buffer-rw-flag nil))
      ;; check version control status
      (if eide-config-show-svn-status-flag
        (setq l-buffer-svn-modified-flag eide-menu-local-svn-modified-status-flag)))

    (eide-i-popup-menu-add-action "Close" (concat "(eide-menu-file-close \"" l-buffer "\")") t)

    (if (not (string-equal l-buffer-status "nofile"))
      (progn

        ;; Option "Set read/write"
        (if l-buffer-rw-flag
          (eide-i-popup-menu-add-action "Set read only" (concat "(eide-edit-action-on-file 'eide-edit-set-r \"" l-buffer "\")") t)
          (eide-i-popup-menu-add-action "Set read/write" (concat "(eide-edit-action-on-file 'eide-edit-set-rw \"" l-buffer "\")") t))

        ;; Option for "edit"
        (if (string-equal l-buffer-status "ref")
          (eide-i-popup-menu-add-action "Switch to NEW file" (concat "(eide-edit-action-on-file 'eide-edit-use-new-file \"" l-buffer "\")") t)
          (if (string-equal l-buffer-status "new")
            (eide-i-popup-menu-add-action "Switch to REF file" (concat "(eide-edit-action-on-file 'eide-edit-use-ref-file \"" l-buffer "\")") t)
            (eide-i-popup-menu-add-action "Backup original file (REF) to work on a copy (NEW)" (concat "(eide-edit-action-on-file 'eide-edit-make-ref-file \"" l-buffer "\")") t)))

        (if (string-equal l-buffer-status "ref")
          (eide-i-popup-menu-add-action "Discard NEW file" (concat "(eide-edit-action-on-file 'eide-edit-discard-new-file \"" l-buffer "\" \"discard NEW file\")") t)
          (if (string-equal l-buffer-status "new")
            (progn
              (eide-i-popup-menu-add-action "Discard REF file" (concat "(eide-edit-action-on-file 'eide-edit-discard-ref-file \"" l-buffer "\" \"discard REF file\")") t)
              (eide-i-popup-menu-add-action "Restore REF file" (concat "(eide-edit-action-on-file 'eide-edit-restore-ref-file \"" l-buffer "\" \"restore REF file\")") t))))))

    (eide-i-popup-menu-close-action-list "Edit")

    (if (not (string-equal l-buffer-status "nofile"))
      (progn
        (eide-i-popup-menu-add-action "Untabify and indent" (concat "(eide-edit-action-on-file 'eide-edit-untabify-and-indent \"" l-buffer "\" \"untabify and indent this file\")") l-buffer-rw-flag)
        (eide-i-popup-menu-add-action "Delete trailing spaces" (concat "(eide-edit-action-on-file 'eide-edit-delete-trailing-spaces \"" l-buffer "\" \"delete trailing spaces\")") l-buffer-rw-flag)

        (eide-i-popup-menu-add-action "Convert end of line: DOS to UNIX" (concat "(eide-edit-action-on-file 'eide-edit-dos-to-unix \"" l-buffer "\" \"convert end of line (DOS to UNIX)\")") l-buffer-rw-flag)
        (eide-i-popup-menu-add-action "Convert end of line: UNIX to DOS" (concat "(eide-edit-action-on-file 'eide-edit-unix-to-dos \"" l-buffer "\" \"convert end of line (UNIX to DOS)\")") l-buffer-rw-flag)

        (eide-i-popup-menu-close-action-list "Clean")

        ;; Option for "compare"
        (if (string-equal l-buffer-status "ref")
          (eide-i-popup-menu-add-action "Compare REF and NEW files" (concat "(eide-compare-with-new-file \"" l-buffer "\")") t)
          (if (string-equal l-buffer-status "new")
            (eide-i-popup-menu-add-action "Compare REF and NEW files" (concat "(eide-compare-with-ref-file \"" l-buffer "\")") t)))

        (if eide-compare-other-project-name
          (eide-i-popup-menu-add-action (concat "Compare with file in project \"" eide-compare-other-project-name "\"") (concat "(eide-compare-with-other-project \"" l-buffer "\")") t))

        (eide-i-popup-menu-close-action-list "Compare")

        ;; Option "svn diff"
        (if l-buffer-svn-modified-flag
          (progn
            (eide-i-popup-menu-add-action "svn diff" (concat "(eide-edit-action-on-file 'eide-svn-diff \"" l-buffer "\")") t)
            (eide-i-popup-menu-add-action "svn revert" (concat "(eide-edit-action-on-file 'eide-svn-revert \"" l-buffer "\" \"revert this file\")") t)))

        (eide-i-popup-menu-close-action-list "svn")))

    (eide-i-popup-menu-open l-buffer)))

;; ----------------------------------------------------------------------------
;; Open a popup menu to select a buffer to display in "output" window.
;;
;; input  : eide-menu-grep-results-list : list of grep results.
;;          eide-menu-cscope-results-list : list of cscope results.
;;          eide-compilation-buffer : compilation buffer name.
;;          eide-execution-buffer : execution buffer name.
;;          eide-shell-buffer : shell buffer name.
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-search-results ()
  (eide-i-popup-menu-init)
  (if eide-menu-grep-results-list
    (progn
      (dolist (l-grep-result eide-menu-grep-results-list)
        ;; Protect \ in grep search buffer name
        (let ((l-grep-result-parameter (replace-regexp-in-string "\\\\" "\\\\" l-grep-result t t)))
          (eide-i-popup-menu-add-action l-grep-result (concat "(eide-search-view-result-buffer \"" l-grep-result-parameter "\")") t)))
      (eide-i-popup-menu-close-action-list "Grep results")))
  (if eide-menu-cscope-results-list
    (progn
      (dolist (l-grep-result eide-menu-cscope-results-list)
        (eide-i-popup-menu-add-action l-grep-result (concat "(eide-search-view-result-buffer \"" l-grep-result "\")") t))
      (eide-i-popup-menu-close-action-list "Cscope results")))
  (eide-i-popup-menu-add-action "Compilation" (concat "(eide-search-view-result-buffer \"" eide-compilation-buffer "\")") eide-compilation-buffer)
  (eide-i-popup-menu-add-action "Execution" (concat "(eide-search-view-result-buffer \"" eide-execution-buffer "\")") eide-execution-buffer)
  (eide-i-popup-menu-add-action "Shell" (concat "(eide-search-view-result-buffer \"" eide-shell-buffer "\")") eide-shell-buffer)
  (eide-i-popup-menu-close-action-list "Compilation / Execution / Shell")
  (eide-i-popup-menu-add-action "Debug session" (concat "(gdb-restore-windows)") eide-project-is-gdb-session-running-flag)
  (eide-i-popup-menu-close-action-list "Debug")
  (eide-i-popup-menu-open "Switch to:"))

;; ----------------------------------------------------------------------------
;; Open a popup menu to select a search result to delete.
;;
;; input  : eide-menu-grep-results-list : list of grep results.
;;          eide-menu-cscope-results-list : list of cscope results.
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-search-results-delete ()
  (eide-i-popup-menu-init)
  (if eide-menu-grep-results-list
    (progn
      (dolist (l-grep-result eide-menu-grep-results-list)
        ;; Protect \ in grep search buffer name
        (let ((l-grep-result-parameter (replace-regexp-in-string "\\\\" "\\\\" l-grep-result t t)))
          (eide-i-popup-menu-add-action (concat "Delete " l-grep-result) (concat "(eide-search-close-grep-buffer \"" l-grep-result-parameter "\")") t)))
      (if (> (length eide-menu-grep-results-list) 1)
        (eide-i-popup-menu-add-action "Delete all grep results" "(eide-search-close-all-grep-buffers)" t))
      (eide-i-popup-menu-close-action-list "Grep results")))
  (if eide-menu-cscope-results-list
    (progn
      (dolist (l-grep-result eide-menu-cscope-results-list)
        (eide-i-popup-menu-add-action (concat "Delete " l-grep-result) (concat "(eide-search-close-cscope-buffer \"" l-grep-result "\")") t))
      (if (> (length eide-menu-cscope-results-list) 1)
        (eide-i-popup-menu-add-action "Delete all cscope results" "(eide-search-close-all-cscope-buffers)" t))
      (eide-i-popup-menu-close-action-list "Cscope results")))
  (eide-i-popup-menu-open "*** DELETE *** search results"))

;; ----------------------------------------------------------------------------
;; Open a popup menu to search for selected text.
;;
;; input  : eide-project-name : project name.
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-search ()
  (eide-i-popup-menu-init)
  (let ((l-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (if eide-project-name
      (progn
        (eide-i-popup-menu-add-action "Go to definition (tag)" (concat "(eide-search-find-tag \"" l-string "\")") t)
        (eide-i-popup-menu-add-action "Find symbol (cscope)" (concat "(eide-search-find-symbol \"" l-string "\")") t)
        (eide-i-popup-menu-add-action "Grep in whole project" (concat "(eide-search-grep-global \"" l-string "\")") t)))
    (eide-i-popup-menu-add-action "Grep in current directory" (concat "(eide-search-grep-local \"" l-string "\")") t)
    (eide-i-popup-menu-close-action-list "Search")
    (eide-i-popup-menu-open (concat "Search: " l-string))))

;; ----------------------------------------------------------------------------
;; Open a popup menu to clean selected lines.
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-cleaning ()
  (eide-i-popup-menu-init)
  (eide-i-popup-menu-add-action "Untabify" "(progn (untabify (region-beginning) (region-end)) (save-buffer))" t)
  (eide-i-popup-menu-add-action "Indent" "(progn (indent-region (region-beginning) (region-end) nil) (save-buffer))" t)
  (eide-i-popup-menu-close-action-list "Cleaning")
  (eide-i-popup-menu-open "Clean selection"))

;;; eide-popup.el ends here
