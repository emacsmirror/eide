;;; eide-popup.el --- Emacs-IDE, popup

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

(provide 'eide-popup)

(setq eide-confirm-dialog
  '(("yes" . "y")
    ("no"  . "n")))

;  '(("Close"  . "close")
;    ("Rename" . "mv")
;    ("Copy"   . "cp")
;    ("Delete" . "rm")))

;  '(("Delete" . "(eide-edit-delete (eide-menu-get-buffer-name-on-current-line)")
;    ("Copy"   . "(eide-edit-copy (eide-menu-get-buffer-name-on-current-line))")
;    ("Rename" . "(eide-edit-rename (eide-menu-get-buffer-name-on-current-line))")
;    ("Close"  . "(eide-menu-file-close)")))


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Initialize a popup (must be called first)
;; ----------------------------------------------------------------------------
(defun eide-popup-internal-menu-create ()
  (setq eide-file-popup nil)
  (setq eide-file-action-list nil)

  (if (not eide-option-menu-buffer-popup-groups-flags)
    (setq eide-file-separator nil)))

;; ----------------------------------------------------------------------------
;; Add an action in action list (for popup)
;;
;; input/ouput :   eide-file-action-list : action list
;; ----------------------------------------------------------------------------
(defun eide-popup-internal-menu-add-action (action-name action-function)
  (if (> (length action-name) 120)
    (setq action-name (concat (substring action-name 0 120) " [...]")))
  (setq eide-file-action-list (append (list (cons action-name action-function)) eide-file-action-list)))

;; ----------------------------------------------------------------------------
;; Add action list to popup
;;
;; input  :  action-list-name      : name of action list
;;           eide-file-action-list : action list
;;           eide-file-popup       : popup
;; output :  eide-file-popup       : modified popup
;;           eide-file-action-list : empty action list
;; return :
;; ----------------------------------------------------------------------------
(defun eide-popup-internal-menu-close-action-list (action-list-name)
  (if eide-file-action-list
    (if eide-option-menu-buffer-popup-groups-flags
      (setq eide-file-popup (append (list (cons action-list-name eide-file-action-list)) eide-file-popup))
      (progn
        (if eide-file-separator
          (setq eide-file-popup (append (list (cons "-" "-")) eide-file-popup))
;          (setq eide-file-popup (append (append (list (cons "-" "-")) eide-file-action-list) eide-file-popup))
          (setq eide-file-separator t))
        (setq eide-file-popup (append eide-file-action-list eide-file-popup)))))
  (setq eide-file-action-list nil))

;; ----------------------------------------------------------------------------
;; Display popup
;;
;; input  : menu-title      : name of popup
;;          eide-file-popup : popup
;; ----------------------------------------------------------------------------
(defun eide-popup-internal-menu-open (menu-title)
  (if eide-file-popup
    (progn
      (setq eide-file-popup (nreverse eide-file-popup))

      ;; Version séparateurs
      (if (not eide-option-menu-buffer-popup-groups-flags)
        (setq eide-file-popup (list (cons "single group" eide-file-popup))))

      (setq my-result (x-popup-menu t (cons menu-title eide-file-popup)))
      (if (bufferp my-result)
        (switch-to-buffer my-result)
        (eval (car (read-from-string my-result)))))))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-popup-internal-open-menu-for-another-project ()
  (interactive)
  (eide-compare-build-other-projects-list)

  (eide-popup-internal-menu-create)
  (dolist (this-project eide-compare-other-projects-list)
    (eide-popup-internal-menu-add-action (car this-project) (concat "(eide-compare-select-another-project \"" (car this-project) "\" \"" (cdr this-project) "\")")))
  (eide-popup-internal-menu-close-action-list "Other projects")
  (eide-popup-internal-menu-open "Select another project :"))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Prompt for a confirmation
;;
;; input  : string : question to be answered yes or no
;; return : t for "yes", nil for "no"
;; ----------------------------------------------------------------------------
(defun eide-popup-question-yes-or-no-p (string)
;  (yes-or-no-p string))
  (if (featurep 'xemacs)
    (string-equal (read-string (concat string " (y/n) ")) "y")
    (string-equal (x-popup-dialog t (cons string eide-confirm-dialog)) "y")))

;; ----------------------------------------------------------------------------
;; Open popup menu related to project
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (let ((popup-header ""))
    (eide-popup-internal-menu-create)
    (if eide-project-name
      ;; Project already created
      (progn
        (eide-popup-internal-menu-add-action (concat "Compile (1) : " (eide-project-get-full-command "compile_command_1")) "(eide-project-compile-1)")
        (eide-popup-internal-menu-add-action (concat "Compile (2) : " (eide-project-get-full-command "compile_command_2")) "(eide-project-compile-2)")
        (eide-popup-internal-menu-add-action (concat "Run (1) : " (eide-project-get-full-command "run_command_1")) "(eide-project-run-1)")
        (eide-popup-internal-menu-add-action (concat "Run (2) : " (eide-project-get-full-command "run_command_2")) "(eide-project-run-2)")
        (eide-popup-internal-menu-add-action (concat "Debug (1) : " (eide-custom-get-project-value "debug_command_1")) "(eide-project-debug-1)")
        (eide-popup-internal-menu-add-action (concat "Debug (2) : " (eide-custom-get-project-value "debug_command_2")) "(eide-project-debug-2)")
        (eide-popup-internal-menu-close-action-list "Execute")

        (if (and (string-equal eide-project-type "C") eide-option-use-cscope-flag)
          (eide-popup-internal-menu-add-action "Update cscope list of files" "(eide-project-update-cscope-list-of-files)"))
        (if (or (not eide-option-use-cscope-flag) eide-option-use-cscope-and-tags-flag)
          (eide-popup-internal-menu-add-action "Update tags" "(eide-project-update-tags)"))
        (eide-popup-internal-menu-close-action-list "Update")
        (if eide-compare-other-project-name
          (eide-popup-internal-menu-add-action (concat "Select another project for comparison (current : \"" eide-compare-other-project-name "\")") "(eide-popup-internal-open-menu-for-another-project)")
          (eide-popup-internal-menu-add-action "Select another project for comparison" "(eide-popup-internal-open-menu-for-another-project)"))
        (eide-popup-internal-menu-close-action-list "Projects comparison")
        (eide-popup-internal-menu-add-action "Project configuration" "(eide-custom-open-project-file)")
        (eide-popup-internal-menu-add-action "Project notes" "(eide-custom-open-project-notes-file)")
        (eide-popup-internal-menu-close-action-list "Configuration")
        (setq popup-header (concat eide-project-type " project : " eide-project-name)))
      ;; Project not created yet
      (progn
        (eide-popup-internal-menu-add-action "Create C project" "(eide-project-create \"C\")")
        (eide-popup-internal-menu-add-action "Create Lisp project" "(eide-project-create \"Lisp\")")
        (eide-popup-internal-menu-close-action-list "Create")
        (setq popup-header (concat "No project (root directory : " eide-project-directory ")"))))

    (eide-popup-internal-menu-add-action "Options" "(eide-custom-open-options-file)")
    (eide-popup-internal-menu-close-action-list "User config")

    (eide-popup-internal-menu-add-action "Help" "(eide-help-open)")
    (eide-popup-internal-menu-close-action-list "Help")

    (eide-popup-internal-menu-open popup-header)))

;; ----------------------------------------------------------------------------
;; Open a popup menu related to selected file
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-file ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  ;; TODO : à ajuster +/-1 pour windows
  (eide-windows-select-window-menu t)
  (move-to-window-line (cdr (last (mouse-position))))
  (if (string-equal eide-custom-toolbar-position "top")
    (forward-line -2))

  (setq my-buffer (eide-menu-get-buffer-name-on-current-line))
  (eide-popup-internal-menu-create)

  (save-excursion
    (set-buffer my-buffer)
;    (setq my-buffer-filename buffer-file-name)
    (setq my-buffer-status (eide-edit-get-buffer-status))

    ;; Check buffer status (r/w, modified)
    (if buffer-read-only
      (setq local-rw nil)
      (setq local-rw t))
    (if (buffer-modified-p)
      (setq local-mod t)
      (setq local-mod nil)))

  (setq eide-file-action-list
    '(("Close"  . "(eide-menu-file-close (eide-menu-get-buffer-name-on-current-line))")))

;; TODO : pas encore tout à fait au point...
;  (eide-popup-internal-menu-add-action "View clean" "(eide-edit-view-clean-buffer (eide-menu-get-buffer-name-on-current-line))")

  ;; Option "Set R/W"
  (if local-rw
    (eide-popup-internal-menu-add-action "Set read only" "(eide-edit-set-r (eide-menu-get-buffer-name-on-current-line))")
    (eide-popup-internal-menu-add-action "Set read/write" "(eide-edit-set-rw (eide-menu-get-buffer-name-on-current-line))"))

  (eide-popup-internal-menu-close-action-list "File")

  ;; Option for "edit"
  (if (string-equal my-buffer-status "ref")
    (eide-popup-internal-menu-add-action "Switch to NEW file" "(eide-edit-use-new-file (eide-menu-get-buffer-name-on-current-line))")
    (if (string-equal my-buffer-status "new")
      (eide-popup-internal-menu-add-action "Switch to REF file" "(eide-edit-use-ref-file (eide-menu-get-buffer-name-on-current-line))")
      (eide-popup-internal-menu-add-action "Backup original file (REF) to work on a copy (NEW)" "(eide-edit-make-ref-file (eide-menu-get-buffer-name-on-current-line))")))

  (if (string-equal my-buffer-status "ref")
    (eide-popup-internal-menu-add-action "Restore REF file" "(eide-edit-restore-ref-file-using-ref (eide-menu-get-buffer-name-on-current-line))")
    (if (string-equal my-buffer-status "new")
      (eide-popup-internal-menu-add-action "Restore REF file" "(eide-edit-restore-ref-file-using-new (eide-menu-get-buffer-name-on-current-line))")))

  (if (string-equal my-buffer-status "new")
    (eide-popup-internal-menu-add-action "Discard REF file" "(eide-edit-discard-ref-file-using-new (eide-menu-get-buffer-name-on-current-line))"))

  (if local-rw
    (eide-popup-internal-menu-add-action "Clean (untabify and indent)" "(eide-edit-pretty-buffer (eide-menu-get-buffer-name-on-current-line))"))

  (eide-popup-internal-menu-close-action-list "Edit")

  ;; Option for "compare"
  (if (string-equal my-buffer-status "ref")
    (eide-popup-internal-menu-add-action "Compare REF and NEW files" "(eide-compare-with-new-file (eide-menu-get-buffer-name-on-current-line))")
    (if (string-equal my-buffer-status "new")
      (eide-popup-internal-menu-add-action "Compare REF and NEW files" "(eide-compare-with-ref-file (eide-menu-get-buffer-name-on-current-line))")))

  (if eide-compare-other-project-name
    (progn
      (eide-popup-internal-menu-add-action (concat "Compare with file in project \"" eide-compare-other-project-name "\"") "(eide-compare-with-other-project (eide-menu-get-buffer-name-on-current-line))")))

  (eide-popup-internal-menu-close-action-list "Compare")

  (eide-popup-internal-menu-open my-buffer))

;; ----------------------------------------------------------------------------
;; Open popup menu related to "compile" tab
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-compile ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-popup-internal-menu-create)
  (eide-popup-internal-menu-add-action (concat "Compile (1) : " (eide-project-get-full-command "compile_command_1")) "(eide-project-compile-1)")
  (eide-popup-internal-menu-add-action (concat "Compile (2) : " (eide-project-get-full-command "compile_command_2")) "(eide-project-compile-2)")
  (eide-popup-internal-menu-close-action-list "Execute")
  (eide-popup-internal-menu-open "Compile"))

;; ----------------------------------------------------------------------------
;; Open popup menu related to "run" tab
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-run ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-popup-internal-menu-create)
  (eide-popup-internal-menu-add-action (concat "Run (1) : " (eide-project-get-full-command "run_command_1")) "(eide-project-run-1)")
  (eide-popup-internal-menu-add-action (concat "Run (2) : " (eide-project-get-full-command "run_command_2")) "(eide-project-run-2)")
  (eide-popup-internal-menu-close-action-list "Execute")
  (eide-popup-internal-menu-open "Run"))

;; ----------------------------------------------------------------------------
;; Open popup menu related to "debug" tab
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-debug ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-popup-internal-menu-create)
  (eide-popup-internal-menu-add-action (concat "Debug (1) : " (eide-custom-get-project-value "debug_command_1")) "(eide-project-debug-1)")
  (eide-popup-internal-menu-add-action (concat "Debug (2) : " (eide-custom-get-project-value "debug_command_2")) "(eide-project-debug-2)")
  (eide-popup-internal-menu-close-action-list "Execute")
  (eide-popup-internal-menu-open "Debug"))

;; ----------------------------------------------------------------------------
;; Open popup menu related to "shell" tab
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-shell ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-popup-internal-menu-create)
  (eide-popup-internal-menu-add-action "Open shell" "(eide-shell-open)")
  (eide-popup-internal-menu-close-action-list "Execute")
  (eide-popup-internal-menu-open "Shell"))

;; ----------------------------------------------------------------------------
;; Open a menu to select a grep result to view
;; Called when user press mouse-3 in "results" buffer
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-search-results ()
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-popup-internal-menu-create)
  (if eide-menu-grep-result-list
    (progn
      (dolist (this-grep-result eide-menu-grep-result-list)
        ;; Protect \ in grep search buffer name
        (setq this-grep-result-parameter (replace-regexp-in-string "\\\\" "\\\\" this-grep-result t t))
        (eide-popup-internal-menu-add-action this-grep-result (concat "(eide-search-view-result-buffer \"" this-grep-result-parameter "\")")))
      (eide-popup-internal-menu-close-action-list "Grep results")))
  (if eide-menu-cscope-result-list
    (progn
      (dolist (this-grep-result eide-menu-cscope-result-list)
        (eide-popup-internal-menu-add-action this-grep-result (concat "(eide-search-view-result-buffer \"" this-grep-result "\")")))
      (eide-popup-internal-menu-close-action-list "Cscope results")))
  (if eide-buffer-compile
    (eide-popup-internal-menu-add-action "Compile buffer" (concat "(eide-search-view-result-buffer \"" eide-buffer-compile "\")")))
  (if eide-buffer-run
    (eide-popup-internal-menu-add-action "Run buffer" (concat "(eide-search-view-result-buffer \"" eide-buffer-run "\")")))
  (if eide-buffer-debug
    (eide-popup-internal-menu-add-action "Debug buffer" (concat "(eide-search-view-result-buffer \"" eide-buffer-debug "\")")))
  (if eide-buffer-shell
    (eide-popup-internal-menu-add-action "Shell buffer" (concat "(eide-search-view-result-buffer \"" eide-buffer-shell "\")")))
  (eide-popup-internal-menu-close-action-list "Compile / run / debug")
  (eide-popup-internal-menu-open "Search results"))

;; ----------------------------------------------------------------------------
;; Open a menu to select a grep result to delete
;; Called when user press control-mouse-3 in "results" buffer
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-search-results-delete ()
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-popup-internal-menu-create)
  (if eide-menu-grep-result-list
    (progn
      (dolist (this-grep-result eide-menu-grep-result-list)
        ;; Protect \ in grep search buffer name
        (setq this-grep-result-parameter (replace-regexp-in-string "\\\\" "\\\\" this-grep-result t t))
        (eide-popup-internal-menu-add-action this-grep-result (concat "(eide-search-close-grep-buffer \"" this-grep-result-parameter "\")")))
      (eide-popup-internal-menu-close-action-list "Grep results")))
  (if eide-menu-cscope-result-list
    (progn
      (dolist (this-grep-result eide-menu-cscope-result-list)
        (eide-popup-internal-menu-add-action this-grep-result (concat "(eide-search-close-cscope-buffer \"" this-grep-result "\")")))
      (eide-popup-internal-menu-close-action-list "Cscope results")))
  (eide-popup-internal-menu-open "*** DELETE *** search results"))

;; ----------------------------------------------------------------------------
;; Open a menu with search actions on selected item
;; Called when user press mouse-3 and some text is selected on a single line
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-search ()
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-popup-internal-menu-create)
  (setq string (buffer-substring-no-properties (region-beginning) (region-end)))
  (if eide-project-name
    (progn
      (eide-popup-internal-menu-add-action "Go to definition" (concat "(eide-search-find-tag \"" string "\")"))
      (eide-popup-internal-menu-add-action "Find symbol" (concat "(eide-search-find-symbol \"" string "\")"))
      (eide-popup-internal-menu-add-action "Grep in whole project" (concat "(eide-search-grep-find \"" string "\")"))))
  (eide-popup-internal-menu-add-action "Grep in current directory" (concat "(eide-search-grep \"" string "\")"))
  (eide-popup-internal-menu-close-action-list "Search")
  (eide-popup-internal-menu-open (concat "Search : " string)))

;; ----------------------------------------------------------------------------
;; Open a menu with cleaning actions on selected lines
;; Called when user press mouse-3 and some text is selected on several lines
;; ----------------------------------------------------------------------------
(defun eide-popup-open-menu-for-cleaning ()
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-popup-internal-menu-create)
  (eide-popup-internal-menu-add-action "Untabify" "(untabify (region-beginning) (region-end))")
  (eide-popup-internal-menu-add-action "Indent" (concat "(indent-region (region-beginning) (region-end) nil)"))
  (eide-popup-internal-menu-close-action-list "Cleaning")
  (eide-popup-internal-menu-open "Clean selection"))

;;; eide-popup.el ends here
