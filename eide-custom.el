;;; eide-custom.el --- Emacs-IDE, customization

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

(provide 'eide-custom)


;;;; ==========================================================================
;;;; OPTIONS
;;;; ==========================================================================

;; Option values : t = on / nil = off

;; Grep-find
(setq eide-option-search-grep-find-on-2-lines-flag nil)

;; Exclude "_" from word delimiters (when selecting by double-click)
(setq eide-option-select-whole-symbol-flag t)

;; Use tags instead of cscope for symbol definition
(setq eide-option-use-cscope-and-tags-flag t)

;; Options for Menu Buffer
;; -----------------------

;; Group files by type in buffer menu
(setq eide-option-buffer-menu-group-files-by-type-flag nil)

;; Insert empty line before new directory
(setq eide-option-buffer-menu-group-files-by-dir-with-empty-line-flag t)

;; Bug XEmacs : mouse highlight with unfolded files
(if (featurep 'xemacs)
  (setq eide-option-buffer-menu-bug-highlight-flag t)
  (setq eide-option-buffer-menu-bug-highlight-flag nil))

;; XEmacs : property is not applied to the whole line with \n
(if (featurep 'xemacs)
  (setq eide-option-buffer-menu-stuff-line-with-spaces-flag t)
  (setq eide-option-buffer-menu-stuff-line-with-spaces-flag nil))

(setq eide-option-buffer-menu-adapt-to-pale-background-flag nil)
(setq eide-option-buffer-menu-white-background-flag t)

;; When using a file (.ref or .new for example), update file date,
;; so that compilation takes it into account.
(setq eide-option-touch-files-when-using-flag t)

(setq eide-option-menu-buffer-popup-groups-flags nil)


;;;; ==========================================================================
;;;; SYNTAX HIGHLIGHTING
;;;; ==========================================================================

;; Enable syntax highlighting
(if (not (featurep 'xemacs))
  (global-font-lock-mode t))

(setq eide-option-color-background          "old lace")
(setq eide-option-color-background-for-menu "white")
;(setq eide-option-color-background-for-menu "white smoke")
;(setq eide-option-color-background-for-menu "gray98")
;(setq eide-option-color-background-for-menu "wheat")
(setq eide-option-color-background-for-config "gray90")

;; Normal text
(if (not (featurep 'xemacs))
  (set-foreground-color "black")
  (set-face-foreground 'default "black"))

;; Left and right borders (here : same color as background)
(if (not (featurep 'xemacs))
  (set-face-background 'fringe eide-option-color-background))

;; Keywords
(set-face-foreground 'font-lock-keyword-face "brown")
(make-face-bold 'font-lock-keyword-face)

;; Functions
(set-face-foreground 'font-lock-function-name-face "red")
;(set-face-background 'font-lock-function-name-face "light yellow")
(make-face-bold 'font-lock-function-name-face)

;; Variables
(set-face-foreground 'font-lock-variable-name-face "OrangeRed")

;; Constant values
(set-face-foreground 'font-lock-constant-face "deep pink") ;steel blue")
(set-face-background 'font-lock-constant-face "misty rose")

;; Types
(set-face-foreground 'font-lock-type-face "sea green") ;"indian red")

;; Pre-processor
(if (featurep 'xemacs)
  (progn
    (set-face-foreground 'font-lock-preprocessor-face "red")
    (set-face-background 'font-lock-preprocessor-face "yellow"))
  (progn
    (set-face-foreground 'font-lock-builtin-face "red")
    (set-face-background 'font-lock-builtin-face "yellow")))

;; Comments
(set-face-foreground 'font-lock-comment-face "light slate blue") ; peru") ; "tan"
;(set-face-background 'font-lock-comment-face "alice blue")

;; Strings
(set-face-foreground 'font-lock-string-face "black") 
(set-face-background 'font-lock-string-face "white") 

;; Selected text
(if (featurep 'xemacs)
  (progn
    (set-face-background 'zmacs-region "bisque")
    (set-face-foreground 'zmacs-region "black"))
  (progn
    (set-face-background 'region "bisque")
    (set-face-foreground 'region "black")))


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in a config
;;
;; return : value as a string, or nil if it doesn't exist
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-get-value-if-defined (my-var)
  (goto-char (point-min))
;    (if (search-forward (concat my-var "....") nil t)
;      (progn
;        (skip-chars-forward ".")
;        (skip-chars-forward " ")
  (if (search-forward (concat my-var ":") nil t)
    (setq this-value (buffer-substring-no-properties (point) (line-end-position)))
    (progn
      (goto-char (point-min))
      (if (and (search-forward (concat my-var "(") nil t) (search-forward "):" nil t))
        (setq this-value (buffer-substring-no-properties (point) (line-end-position)))
        (setq this-value nil)))))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in a config
;;
;; return : value as a string ("" if it is not defined)
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-get-value (my-var)
  (setq this-value (eide-custom-internal-get-value-if-defined my-var))
  (if this-value
    this-value
    ""))

;; ----------------------------------------------------------------------------
;; Prepare update of config file
;;
;; input  : my-path : path of config file
;;          my-file : name of config file
;; return : nil if buffer was already open, t otherwise
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-rebuild-start (my-path my-file)
  ;; Define source and target config files
  (setq eide-custom-path my-path)
  (setq eide-custom-source-buffer my-file)
  (setq eide-custom-target-buffer (concat my-file "_temp"))

  (let ((ret-flag nil))
    ;; Open these config files
    (if (not (get-buffer eide-custom-source-buffer))
      (progn
        (find-file-noselect (concat eide-custom-path eide-custom-source-buffer))
        (setq ret-flag t)))
    (find-file-noselect (concat eide-custom-path eide-custom-target-buffer))
    (set-buffer eide-custom-target-buffer)
    (delete-region (point-min) (point-max))
    ret-flag))

;; ----------------------------------------------------------------------------
;; Clean after update of config file
;;
;; input  : t if buffer needs to be closed, nil otherwise (should be the value
;;          returned by eide-custom-internal-rebuild-start)
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-rebuild-stop (my-close-flag)
  ;; Save target file
  (set-buffer eide-custom-target-buffer)
  (save-buffer)

  ;; Replace source file by target file if different
  (shell-command (concat "cd " eide-custom-path " ; diff -q " eide-custom-source-buffer " " eide-custom-target-buffer " || cp -f " eide-custom-target-buffer " " eide-custom-source-buffer " ; rm -f " eide-custom-target-buffer))

  (if my-close-flag
    ;; Close buffer
    (kill-buffer eide-custom-source-buffer)
    (progn
      ;; Update buffer from file
      (set-buffer eide-custom-source-buffer)
      (revert-buffer)))
  ;; Close temp buffer
  (kill-buffer eide-custom-target-buffer))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in config file
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-rebuild-get-current-value (my-param)
  (set-buffer eide-custom-source-buffer)
  (eide-custom-internal-get-value-if-defined my-param))

;; ----------------------------------------------------------------------------
;; Insert section header in config file
;;
;; input  : section header title
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-rebuild-insert-section (my-section)
  (set-buffer eide-custom-target-buffer)
  (insert "\n-------------------------------------------------------------------------------\n")
;  (insert "\n[")
  (insert my-section)
  (insert "\n-------------------------------------------------------------------------------\n\n"))
;  (insert "]\n"))

;; ----------------------------------------------------------------------------
;; Insert a line with a parameter and its value in config file
;;
;; input  : my-param : parameter as a string
;;          my-value : value as a string
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-rebuild-insert-parameter (my-param my-value &optional my-possibilities)
  (set-buffer eide-custom-target-buffer)
  (insert my-param)
  (if my-possibilities
    (progn
      (insert "(")
      (insert my-possibilities)
      (insert ")")))
  (insert ":")
;  (while (< (current-column) 30)
;    (insert "."))
;  (insert " ")
  (insert my-value)
  (insert "\n"))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in user config
;;
;; return : value as a string
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-get-option-value (my-param)
  (save-excursion
    (if (not (get-buffer eide-options-file))
      (find-file-noselect (concat "~/" eide-options-file)))
    (set-buffer eide-options-file)
    (eide-custom-internal-get-value my-param)))

;; ----------------------------------------------------------------------------
;; Update a line with a parameter and its value (default if not found)
;;
;; input  : my-param         : parameter as a string
;;          my-default-value : default value as a string
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-update-value (my-param my-default-value &optional my-possibilities)
  (setq my-value (eide-custom-internal-rebuild-get-current-value my-param))
  (if (not my-value)
    (setq my-value my-default-value))
  (eide-custom-internal-rebuild-insert-parameter my-param my-value my-possibilities))

;; ----------------------------------------------------------------------------
;; Update a line with a parameter and its value (default from custom if not
;; found)
;;
;; input  : my-param :        parameter as a string
;;          my-custom-param : parameter (default in custom) as a string
;; ----------------------------------------------------------------------------
(defun eide-custom-internal-update-value-from-options (my-param my-custom-param)
  (setq my-value (eide-custom-internal-rebuild-get-current-value my-param))
  (if (not my-value)
    (setq my-value (eide-custom-internal-get-option-value my-custom-param)))
  (eide-custom-internal-rebuild-insert-parameter my-param my-value))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Update custom file
;; ----------------------------------------------------------------------------
(defun eide-custom-rebuild-options-file ()
  ;; Configuration values are read from source file (current config file) and
  ;; wrote into target file (which will replace config file in the end).
  ;; - If a parameter is not set in source file - a new parameter for example -
  ;;   it will be given default value in target file.
  ;; - If a parameter is set in source file but doesn't exist anymore - a
  ;;   deprecated parameter for example - it will not be present in target
  ;;   file.
  ;; Therefore, config file is always compliant with current version.

  (save-excursion
    (let ((close-flag (eide-custom-internal-rebuild-start "~/" eide-options-file)))

      (eide-custom-internal-rebuild-insert-section "Display")
      (eide-custom-internal-update-value "font_size" "18")
      ;(eide-custom-internal-update-value "font_size_for_windows" "16")

      (eide-custom-internal-rebuild-insert-section "Windows layout")
      (eide-custom-internal-update-value "menu_position" "right" "left/right")
      (eide-custom-internal-update-value "menu_height" "half" "half/full")
      (eide-custom-internal-update-value "toolbar_enabled" "yes" "yes/no")
      (eide-custom-internal-update-value "toolbar_position" "middle" "top/middle/bottom")

      (eide-custom-internal-rebuild-insert-section "Coding rules")
;      (eide-custom-internal-update-value "use_tabs" "no")
      (eide-custom-internal-update-value "indent_offset" "2")

      (eide-custom-internal-rebuild-insert-section "Compilation / run / debug commands (default for new project)")
      (eide-custom-internal-update-value "project_default_init_command"      "")
      (eide-custom-internal-update-value "project_default_compile_command_1" "make")
      (eide-custom-internal-update-value "project_default_compile_command_2" "make")
      (eide-custom-internal-update-value "project_default_run_command_1"     "./program")
      (eide-custom-internal-update-value "project_default_run_command_2"     "./program")
      (eide-custom-internal-update-value "project_default_debug_command_1"   "gdb program")
      (eide-custom-internal-update-value "project_default_debug_command_2"   "gdb program")

      (eide-custom-internal-rebuild-stop close-flag))))

;; ----------------------------------------------------------------------------
;; Update .emacs-ide.project
;; ----------------------------------------------------------------------------
(defun eide-custom-rebuild-project-file ()
  (save-excursion
    (let ((close-flag (eide-custom-internal-rebuild-start eide-project-directory eide-project-file)))

      (find-file-noselect (concat "~/" eide-options-file))

      (eide-custom-internal-rebuild-insert-section "Project")
      ;; directory-file-name removes last "/"
      ;; file-name-nondirectory retrieves last directory name from complete path
      (eide-custom-internal-update-value "project_name" (file-name-nondirectory (directory-file-name eide-project-directory)))
      ;; This parameter cannot be changed (user edit is ignored)
      (eide-custom-internal-rebuild-insert-parameter "project_type" eide-project-type)

      (eide-custom-internal-rebuild-insert-section "Compilation / run /debug commands")
      (eide-custom-internal-update-value-from-options "init_command"      "project_default_init_command")
      (eide-custom-internal-update-value-from-options "compile_command_1" "project_default_compile_command_1")
      (eide-custom-internal-update-value-from-options "compile_command_2" "project_default_compile_command_2")
      (eide-custom-internal-update-value-from-options "run_command_1"     "project_default_run_command_1")
      (eide-custom-internal-update-value-from-options "run_command_2"     "project_default_run_command_2")
      (eide-custom-internal-update-value-from-options "debug_command_1"   "project_default_debug_command_1")
      (eide-custom-internal-update-value-from-options "debug_command_2"   "project_default_debug_command_2")

      (kill-buffer eide-options-file)

      (eide-custom-internal-rebuild-stop close-flag))))

;; ----------------------------------------------------------------------------
;; Get value of a parameter in project config
;;
;; return : value as a string
;; ----------------------------------------------------------------------------
(defun eide-custom-get-project-value (my-param)
  (save-excursion
    (if (not (get-buffer eide-project-file))
      (find-file-noselect (concat eide-project-directory eide-project-file)))
    (set-buffer eide-project-file)
    (eide-custom-internal-get-value my-param)))

;; ----------------------------------------------------------------------------
;; Get value of a parameter in project config
;;
;; return : value as a string
;; ----------------------------------------------------------------------------
(defun eide-custom-get-project-value-in-current-buffer (my-param)
  (eide-custom-internal-get-value my-param))

; not called !
;; ----------------------------------------------------------------------------
;; Set value of a parameter in project config
;;
;; return : value as a string
;; ----------------------------------------------------------------------------
(defun eide-custom-set-project-value (my-param my-value)
  (if eide-windows-is-layout-visible-flag
    (progn
      (setq my-window (selected-window))
      (eide-windows-select-window-file t)))
  ;; TODO : comprendre pourquoi : si on ne va pas dans la window "file",
  ;; le save-buffer à la fin ne fait rien.
  ;; Comme si le set-buffer/save-buffer dans la window "menu" posait problème
  ;; TODO : mémoriser la window courante n'est pas nécessaire (avec les appels
  ;; actuels) : à virer éventuellement, même si ça semble plus propre de le
  ;; faire.
  (save-excursion
    (set-buffer eide-project-file) ; TODO : load file if necessary...
    (goto-char (point-min))
;    (if (search-forward (concat my-param "....") nil t)
;      (progn
;        (skip-chars-forward ".")
;        (skip-chars-forward " ")
    (if (search-forward (concat my-param ":") nil t)
      (progn
        ;; Delete old value
        (delete-region (point) (line-end-position))
        ;; Insert new value
        (insert my-value)
        (save-buffer))))
  (if eide-windows-is-layout-visible-flag
    (select-window my-window)))

;; ----------------------------------------------------------------------------
;; Apply options
;; ----------------------------------------------------------------------------
(defun eide-custom-apply-options ()
  ;; Size of characters for X system
  (if window-system
;    (if (eq system-type 'windows-nt)
;      (set-default-font (concat "-*-fixed-medium-r-*-*-" (eide-custom-internal-get-option-value "font_size_for_windows") "-*-*-*-c-*-iso8859-1"))
      (let ((my-string (concat "-*-fixed-medium-r-*-*-" (eide-custom-internal-get-option-value "font_size") "-*-*-*-c-*-iso8859-1")))
        (if (featurep 'xemacs)
          (set-face-font 'default my-string)
          (set-default-font my-string))))

  ;; Windows layout : menu position
  (setq eide-custom-menu-position (eide-custom-internal-get-option-value "menu_position"))
  ;; If menu position is not correct, set default value
  (if (not (or (string-equal eide-custom-menu-position "left")
               (string-equal eide-custom-menu-position "right")))
    (setq eide-custom-menu-position "right"))

  ;; Windows layout : menu height
  (setq eide-custom-menu-height (eide-custom-internal-get-option-value "menu_height"))
  ;; If menu position is not correct, set default value
  (if (not (or (string-equal eide-custom-menu-height "half")
               (string-equal eide-custom-menu-height "full")))
    (setq eide-custom-menu-height "half"))

  ;; Windows layout : use toolbar ?
  (if (string-equal (eide-custom-internal-get-option-value "toolbar_enabled") "yes")
    (setq eide-custom-use-toolbar t)
    (setq eide-custom-use-toolbar nil))

  ;; Windows layout : toolbar position
  (setq eide-custom-toolbar-position (eide-custom-internal-get-option-value "toolbar_position"))
  ;; If toolbar position is not correct, set default value
  (if (not (or (string-equal eide-custom-toolbar-position "top")
               (string-equal eide-custom-toolbar-position "middle")
               (string-equal eide-custom-toolbar-position "bottom")))
    (setq eide-custom-toolbar-position "middle"))

  ;; Coding rules
  (setq eide-c-indent-offset (string-to-number (eide-custom-internal-get-option-value "indent_offset"))))
  ;; TODO : appliquer la valeur sans avoir à recharger les fichiers manuellement (F5)

;; ----------------------------------------------------------------------------
;; Apply project configuration changes
;; ----------------------------------------------------------------------------
(defun eide-custom-apply-project-configuration ()
  (setq eide-project-name (eide-custom-get-project-value "project_name"))
  (eide-project-update-frame-title)
  (eide-menu-update t))

;; ----------------------------------------------------------------------------
;; Display options file (full frame)
;; ----------------------------------------------------------------------------
(defun eide-custom-open-options-file ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-windows-layout-unbuild)
  (eide-set-background-color-for-config)
  (eide-key-bindings-configure-for-special-buffer)
  (eide-menu-find-file-without-hook (concat "~/" eide-options-file)))

;; ----------------------------------------------------------------------------
;; Display project file (full frame)
;; ----------------------------------------------------------------------------
(defun eide-custom-open-project-file ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-windows-layout-unbuild)
  (eide-set-background-color-for-config)
  (eide-key-bindings-configure-for-special-buffer)
  (eide-menu-find-file-without-hook (concat eide-project-directory eide-project-file))

  ;; Hide project type (should not be changed)
  (goto-char (point-min))
  (if (search-forward "project_type:" nil t)
    (overlay-put (make-overlay (line-beginning-position) (line-beginning-position 2)) 'invisible t))
  (goto-char (point-min)))

;; ----------------------------------------------------------------------------
;; Display project notes file (full frame)
;; ----------------------------------------------------------------------------
(defun eide-custom-open-project-notes-file ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (eide-windows-layout-unbuild)
  (eide-set-background-color-for-config)
  (eide-key-bindings-configure-for-special-buffer)
  (eide-menu-find-file-without-hook (concat eide-project-directory eide-project-notes-file)))

;;; eide-custom.el ends here
