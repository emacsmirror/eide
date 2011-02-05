;;; eide-config.el --- Emacs-IDE, config

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

(provide 'eide-config)

(require 'eide-popup) ; for eide-popup-message

(defvar eide-config-file         ".emacs-ide.cfg")
(defvar eide-project-config-file ".emacs-ide-project.cfg")
(defvar eide-project-notes-file  ".emacs-ide-project.txt")

(defvar eide-config-show-trailing-spaces-flag nil)
(defvar eide-config-menu-position nil)
(defvar eide-config-menu-height nil)
(defvar eide-config-show-svn-status-flag nil)
(defvar eide-config-svn-diff-command nil)

(defvar eide-config-use-emacs-options-flag nil)
(defvar eide-config-use-color-theme-for-source-flag nil)
(defvar eide-config-c-indent-offset nil)

(defvar eide-config-background-color nil)
(defvar eide-config-foreground-color nil)
(defvar eide-config-menu-background-color nil)
(defvar eide-config-menu-foreground-color nil)
(defvar eide-config-user-background-color nil)
(defvar eide-config-user-foreground-color nil)
(defvar eide-config-menu-file-highlight-background-color nil)
(defvar eide-config-config-background-color nil)
(defvar eide-config-config-foreground-color nil)

(defvar eide-config-user-keyword-foreground-color nil)
(defvar eide-config-user-type-foreground-color nil)
(defvar eide-config-user-function-foreground-color nil)
(defvar eide-config-user-variable-foreground-color nil)
(defvar eide-config-user-constant-background-color nil)
(defvar eide-config-user-constant-foreground-color nil)
(defvar eide-config-user-builtin-background-color nil)
(defvar eide-config-user-builtin-foreground-color nil)
(defvar eide-config-user-string-background-color nil)
(defvar eide-config-user-string-foreground-color nil)
(defvar eide-config-user-comment-foreground-color nil)
(defvar eide-config-user-selection-background-color nil)
(defvar eide-config-user-selection-foreground-color nil)

(defvar eide-config-source-buffer nil)
(defvar eide-config-target-buffer nil)

;;;; ==========================================================================
;;;; OPTIONS
;;;; ==========================================================================

;; Option values: t = on / nil = off

;; Exclude "_" from word delimiters (when selecting by double-click)
(defvar eide-option-select-whole-symbol-flag t)

;; Use tags instead of cscope for symbol definition
(defvar eide-option-use-cscope-and-tags-flag t)

;; Options for Menu Buffer
;; -----------------------

;; When using a file (.ref or .new for example), update file date,
;; so that compilation takes it into account.
(defvar eide-option-touch-files-when-using-flag t)

(defvar eide-option-menu-buffer-popup-groups-flags nil)

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "EMACS-IDE-CONFIG"
;;;; ==========================================================================

(define-derived-mode emacs-ide-config-mode fundamental-mode "Emacs-IDE config"
  (setq font-lock-defaults '('(("\\(#.*\\)"      1 'eide-config-config-comment-face) ; comment
                               ("\\[\\(.*\\)\\]" 1 'eide-config-config-section-face) ; section
                               ("\\(.*\\) = "    1 'eide-config-config-parameter-face) ; parameter
                               (" = "            . 'eide-config-config-separator-face) ; " = "
                               (" = \\(.*\\)"    1 'eide-config-config-value-face))))) ; value

(setq auto-mode-alist (append '(("\\.emacs-ide.cfg\\'" . emacs-ide-config-mode)
                                ("\\.emacs-ide-project.cfg\\'" . emacs-ide-config-mode)) auto-mode-alist))

;;;; ==========================================================================
;;;; SYNTAX HIGHLIGHTING
;;;; ==========================================================================

(require 'font-lock)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Menus (no effect on Windows)
(set-face-background 'menu "light grey")
(set-face-foreground 'menu "black")

;; Vertical scroll bar (no effect on Windows)
(set-face-background 'scroll-bar "light grey")

;; Menu
(make-face 'eide-config-menu-default-face)
(make-face 'eide-config-menu-project-header-face)
(make-face 'eide-config-menu-project-name-face)
(make-face 'eide-config-menu-directory-face)
(make-face 'eide-config-menu-directory-out-of-project-face)
(make-face 'eide-config-menu-file-rw-face)
(make-face 'eide-config-menu-file-ro-face)
(make-face 'eide-config-menu-file-nofile-face)
(make-face 'eide-config-menu-file-ref-face)
(make-face 'eide-config-menu-file-new-face)
(make-face 'eide-config-menu-file-svn-modified-face)
(make-face 'eide-config-menu-function-face)
(make-face 'eide-config-menu-function-with-highlight-face)
(make-face 'eide-config-menu-empty-list-face)

;; Help page
(make-face 'eide-config-help-title-face)
(make-face 'eide-config-help-chapter1-face)
(make-face 'eide-config-help-chapter2-face)

;; Config files
(make-face 'eide-config-config-comment-face)
(make-face 'eide-config-config-section-face)
(make-face 'eide-config-config-parameter-face)
(make-face 'eide-config-config-possibilities-face)
(make-face 'eide-config-config-separator-face)
(make-face 'eide-config-config-value-face)

(make-face-bold 'eide-config-config-section-face)
(make-face-bold 'eide-config-config-parameter-face)

;; Parenthese matching (requires show-paren-mode)
;;(set-face-background 'show-paren-match-face "orange")

;; Compilation warnings and file path in a grep result
;; (because grep uses "compile" mode to display its results)
(set-face-foreground 'font-lock-warning-face "tan")

;; Code
(make-face-bold 'font-lock-keyword-face)
(make-face-bold 'font-lock-function-name-face)

;; Menu
(make-face-bold 'eide-config-menu-project-header-face)
(make-face-bold 'eide-config-menu-project-name-face)

(make-face-bold 'eide-config-menu-file-rw-face)
(make-face-bold 'eide-config-menu-file-ro-face)
(set-face-foreground 'eide-config-menu-file-ref-face "orange red")
(make-face-bold 'eide-config-menu-file-ref-face)
(set-face-foreground 'eide-config-menu-file-new-face "medium sea green")
(make-face-bold 'eide-config-menu-file-new-face)
(set-face-foreground 'eide-config-menu-file-svn-modified-face "blue")
(make-face-bold 'eide-config-menu-file-svn-modified-face)

(make-face-italic 'eide-config-menu-empty-list-face)

;; Help page
(make-face-bold 'eide-config-help-title-face)

;; Hidden text (for hide/show minor mode)
;; Does not work with Emacs 22.3: I comment it until I can test
;; and maybe fix the bug.
;;(make-face 'font-selective-display-face)
;;(set-face-foreground 'font-selective-display-face "blue")
;;(set-face-background 'font-selective-display-face "lavender")
;;(setq font-selective-display-face-id (face-id 'font-selective-display-face))

;;(setq selective-display-vector (vconcat "{ ... }\n"))
;;(setq selective-display-vector (vconcat "\n" (mapcar '(lambda (x) (+ (* font-selective-display-face-id 524288) x)) selective-display-vector)))
;;(set-display-table-slot standard-display-table 'selective-display selective-display-vector)

;; Ediff
(copy-face 'default 'ediff-even-diff-face-A)
(set-face-background 'ediff-even-diff-face-A "wheat")
(set-face-foreground 'ediff-even-diff-face-A "black")

(copy-face 'default 'ediff-even-diff-face-B)
(set-face-background 'ediff-even-diff-face-B "wheat")
(set-face-foreground 'ediff-even-diff-face-B "black")

(copy-face 'default 'ediff-odd-diff-face-A)
(set-face-background 'ediff-odd-diff-face-A "wheat")
(set-face-foreground 'ediff-odd-diff-face-A "black")

(copy-face 'default 'ediff-odd-diff-face-B)
(set-face-background 'ediff-odd-diff-face-B "wheat")
(set-face-foreground 'ediff-odd-diff-face-B "black")

;; Current difference: what is common or only in one buffer
(copy-face 'default 'ediff-current-diff-face-A)
(set-face-background 'ediff-current-diff-face-A "pink")
(set-face-foreground 'ediff-current-diff-face-A "black")

(copy-face 'default 'ediff-current-diff-face-B)
(set-face-background 'ediff-current-diff-face-B "pink")
(set-face-foreground 'ediff-current-diff-face-B "black")

;; Current difference: what really differs
(copy-face 'default 'ediff-fine-diff-face-A)
(set-face-background 'ediff-fine-diff-face-A "plum")
(set-face-foreground 'ediff-fine-diff-face-A "black")

(copy-face 'default 'ediff-fine-diff-face-B)
(set-face-background 'ediff-fine-diff-face-B "plum")
(set-face-foreground 'ediff-fine-diff-face-B "black")

;;(require 'glasses)

;;(make-face-bold 'glasses-face)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in a config (current buffer).
;;
;; input  : p-parameter : config parameter.
;; return : value as a string, or nil if it is not defined.
;; ----------------------------------------------------------------------------
(defun eide-i-config-get-value-if-defined (p-parameter)
  (goto-char (point-min))
  (if (re-search-forward (concat "^" p-parameter " = ") nil t)
    (buffer-substring-no-properties (point) (line-end-position))
    ;; Migration from Emacs-IDE 1.5
    ;; Compatibility with old syntax (":")
    (progn
      (goto-char (point-min))
      (if (re-search-forward (concat "^" p-parameter ":") nil t)
        (buffer-substring-no-properties (point) (line-end-position))
        nil))))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in a config (current buffer).
;;
;; input  : p-parameter : config parameter.
;; return : value as a string, or "" if it is not defined.
;; ----------------------------------------------------------------------------
(defun eide-i-config-get-value (p-parameter)
  (let ((l-value (eide-i-config-get-value-if-defined p-parameter)))
    (if l-value
      l-value
      "")))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in config file.
;;
;; input  : p-parameter : config parameter.
;; return : config value.
;; ----------------------------------------------------------------------------
(defun eide-i-config-get-option-value (p-parameter)
  (save-excursion
    (if (not (get-buffer eide-config-file))
      (find-file-noselect (concat "~/" eide-config-file)))
    (set-buffer eide-config-file)
    (eide-i-config-get-value p-parameter)))

;; ----------------------------------------------------------------------------
;; Prepare update of config file (in a temporary file).
;;
;; input  : p-path : path of config file.
;;          p-file : name of config file.
;; output : eide-config-source-buffer : name of config file.
;;          eide-config-target-buffer : temporary buffer for update.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-start (p-path p-file)
  ;; Define source and target config files
  (setq eide-config-source-buffer p-file)
  (setq eide-config-target-buffer (concat p-file "_temp"))

  ;; Open these config files
  (if (not (get-buffer eide-config-source-buffer))
    (find-file-noselect (concat p-path eide-config-source-buffer)))
  (get-buffer-create eide-config-target-buffer)
  (set-buffer eide-config-target-buffer)
  (erase-buffer))

;; ----------------------------------------------------------------------------
;; Clean after update of config file.
;;
;; input  : eide-config-source-buffer : name of config file.
;;          eide-config-target-buffer : temporary buffer for update.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-stop ()
  ;; Replace source file by target buffer if different
  (if (not (equal (compare-buffer-substrings eide-config-source-buffer nil nil eide-config-target-buffer nil nil) 0))
    (progn
      (set-buffer eide-config-source-buffer)
      (erase-buffer)
      (insert-buffer eide-config-target-buffer)
      (ad-deactivate 'save-buffer)
      (save-buffer)
      (ad-activate 'save-buffer)))
  ;; Close temporary buffer
  (kill-buffer eide-config-target-buffer))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in config file.
;;
;; input  : p-parameter : config parameter.
;;          eide-config-source-buffer : name of config file.
;; return : config value.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-get-current-value (p-parameter)
  (set-buffer eide-config-source-buffer)
  (eide-i-config-get-value-if-defined p-parameter))

;; ----------------------------------------------------------------------------
;; Insert information in config file.
;;
;; input  : p-config-file : string describing config file.
;;          eide-config-target-buffer : temporary config buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-insert-info (p-config-file)
  (set-buffer eide-config-target-buffer)
  (insert "\n# *****************************************************************************\n")
  (insert (concat "# Emacs-IDE " p-config-file))
  (insert "\n# *****************************************************************************\n\n")
  (insert "# --> Click right to exit this page.\n")
  (if (string-equal p-config-file "configuration")
    (insert "# --> Press shift + click right to show/hide the list of available colors.\n"))
  (insert "# --> To restore the default value of a parameter, delete the line\n")
  (insert (concat "#     (" p-config-file " file is rebuilt when you exit this page).\n\n")))

;; ----------------------------------------------------------------------------
;; Insert dotted line in config file.
;;
;; input  : p-add-empty-line-before-flag : t = add an empty line before.
;;          eide-config-target-buffer : temporary config buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-insert-dotted-line (p-add-empty-line-before-flag)
  (set-buffer eide-config-target-buffer)
  (if p-add-empty-line-before-flag
    (insert "\n"))
  (insert "# -----------------------------------------------------------------------------\n"))

;; ----------------------------------------------------------------------------
;; Insert section header in config file.
;;
;; input  : p-section : section header (string).
;;          eide-config-target-buffer : temporary config buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-insert-section (p-section)
  (set-buffer eide-config-target-buffer)
  (insert (concat "\n[" p-section "]\n")))

;; ----------------------------------------------------------------------------
;; Insert comment in config file.
;;
;; input  : p-comment : comment (string).
;;          eide-config-target-buffer : temporary config buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-insert-comment (p-comment)
  (set-buffer eide-config-target-buffer)
  (insert (concat "# " p-comment "\n")))

;; ----------------------------------------------------------------------------
;; Insert a line with a parameter and its value in config file.
;;
;; input  : p-parameter : config parameter.
;;          p-value : config value.
;;          eide-config-target-buffer : temporary config buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-insert-parameter (p-parameter p-value)
  (set-buffer eide-config-target-buffer)
  (insert p-parameter)
  (insert " = ")
  (insert p-value)
  (insert "\n"))

;; ----------------------------------------------------------------------------
;; Update a line with a parameter and its value (default if not found).
;;
;; input  : p-parameter : config parameter.
;;          p-default-value : config default value.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-update-value (p-parameter p-default-value)
  (let ((l-value (eide-i-config-rebuild-get-current-value p-parameter)))
    ;; If the parameter is not present, or it is a color and the value is not
    ;; correct, we use default value.
    (if (not l-value)
      (setq l-value p-default-value))
    (if (and (or (string-match "^dark_color_theme_" p-parameter)
                 (string-match "^light_color_theme_" p-parameter))
             (not (color-defined-p l-value)))
      (progn
        (eide-popup-message (concat "Warning: " p-parameter " value \"" l-value "\" is not correct,\nusing default value \"" p-default-value "\" instead."))
        (setq l-value p-default-value)))
    (eide-i-config-rebuild-insert-parameter p-parameter l-value)))

;; ----------------------------------------------------------------------------
;; Update a line with a parameter and its value (default from config if not
;; found).
;;
;; input  : p-parameter : config parameter.
;;          p-config-parameter : related parameter in config.
;; ----------------------------------------------------------------------------
(defun eide-i-config-rebuild-update-value-from-config (p-parameter p-config-parameter)
  (let ((l-value (eide-i-config-rebuild-get-current-value p-parameter)))
    (if (not l-value)
      (setq l-value (eide-i-config-get-option-value p-config-parameter)))
    (eide-i-config-rebuild-insert-parameter p-parameter l-value)))

;; ----------------------------------------------------------------------------
;; Apply color theme.
;; ----------------------------------------------------------------------------
(defun eide-i-config-apply-color-theme ()
  (let ((l-color-theme (eide-i-config-get-option-value "color_theme")))
    (if (string-equal l-color-theme "dark")
      (progn
        ;; "dark" theme
        (setq eide-config-background-color (eide-i-config-get-option-value "dark_color_theme_background"))
        (setq eide-config-foreground-color (eide-i-config-get-option-value "dark_color_theme_foreground"))

        (if eide-config-use-color-theme-for-source-flag
          (progn
            ;; Code
            (set-face-foreground 'font-lock-keyword-face (eide-i-config-get-option-value "dark_color_theme_keyword_foreground"))
            (set-face-foreground 'font-lock-type-face (eide-i-config-get-option-value "dark_color_theme_type_foreground"))
            (set-face-foreground 'font-lock-function-name-face (eide-i-config-get-option-value "dark_color_theme_function_foreground"))
            (set-face-foreground 'font-lock-variable-name-face (eide-i-config-get-option-value "dark_color_theme_variable_foreground"))
            (set-face-background 'font-lock-constant-face (eide-i-config-get-option-value "dark_color_theme_constant_background"))
            (set-face-foreground 'font-lock-constant-face (eide-i-config-get-option-value "dark_color_theme_constant_foreground"))
            (set-face-background 'font-lock-builtin-face (eide-i-config-get-option-value "dark_color_theme_builtin_background"))
            (set-face-foreground 'font-lock-builtin-face (eide-i-config-get-option-value "dark_color_theme_builtin_foreground"))
            (set-face-background 'font-lock-string-face (eide-i-config-get-option-value "dark_color_theme_string_background"))
            (set-face-foreground 'font-lock-string-face (eide-i-config-get-option-value "dark_color_theme_foreground"))
            (set-face-foreground 'font-lock-comment-face (eide-i-config-get-option-value "dark_color_theme_comment_foreground"))
            (set-face-background 'region (eide-i-config-get-option-value "dark_color_theme_selection_background"))
            (set-face-foreground 'region (eide-i-config-get-option-value "dark_color_theme_foreground"))))

        ;; Menu
        (setq eide-config-menu-background-color (eide-i-config-get-option-value "dark_color_theme_menu_background"))
        (setq eide-config-menu-foreground-color "white")
        (set-face-foreground 'eide-config-menu-project-header-face "deep sky blue")
        (set-face-foreground 'eide-config-menu-project-name-face "orange")

        ;; Menu: directories
        (set-face-background 'eide-config-menu-directory-face "dim gray")
        (set-face-foreground 'eide-config-menu-directory-face "white")
        (set-face-background 'eide-config-menu-directory-out-of-project-face "saddle brown")
        (set-face-foreground 'eide-config-menu-directory-out-of-project-face "peach puff")

        ;; Menu: files
        (set-face-foreground 'eide-config-menu-file-rw-face "gray95")
        (set-face-foreground 'eide-config-menu-file-ro-face "gray65")
        (set-face-foreground 'eide-config-menu-file-nofile-face "gray95")
        (setq eide-config-menu-file-highlight-background-color "brown")

        ;; Menu: functions
        (set-face-foreground 'eide-config-menu-function-face "deep sky blue")
        (set-face-background 'eide-config-menu-function-with-highlight-face "navy")
        (set-face-foreground 'eide-config-menu-function-with-highlight-face "deep sky blue")

        ;; Help page
        (set-face-background 'eide-config-help-title-face "indian red")
        (set-face-foreground 'eide-config-help-title-face "white")
        (set-face-background 'eide-config-help-chapter1-face "brown")
        (set-face-foreground 'eide-config-help-chapter1-face "yellow")
        (set-face-background 'eide-config-help-chapter2-face "dark slate gray")
        (set-face-foreground 'eide-config-help-chapter2-face "pale green")

        ;; Config files
        (setq eide-config-config-background-color "gray20")
        (setq eide-config-config-foreground-color "white")
        (set-face-foreground 'eide-config-config-comment-face "deep sky blue")
        (set-face-foreground 'eide-config-config-section-face "orange")
        (set-face-foreground 'eide-config-config-parameter-face "salmon")
        (set-face-foreground 'eide-config-config-possibilities-face "medium sea green")
        (set-face-foreground 'eide-config-config-separator-face "orange red")
        (set-face-background 'eide-config-config-value-face "gray30")
        (set-face-foreground 'eide-config-config-value-face "white")

        ;; Information line
        (set-face-background 'mode-line "gray"))

      (progn
        ;; "light" theme
        (setq eide-config-background-color (eide-i-config-get-option-value "light_color_theme_background"))
        (setq eide-config-foreground-color (eide-i-config-get-option-value "light_color_theme_foreground"))

        (if eide-config-use-color-theme-for-source-flag
          (progn
            ;; Code
            (set-face-foreground 'font-lock-keyword-face (eide-i-config-get-option-value "light_color_theme_keyword_foreground"))
            (set-face-foreground 'font-lock-type-face (eide-i-config-get-option-value "light_color_theme_type_foreground"))
            (set-face-foreground 'font-lock-function-name-face (eide-i-config-get-option-value "light_color_theme_function_foreground"))
            (set-face-foreground 'font-lock-variable-name-face (eide-i-config-get-option-value "light_color_theme_variable_foreground"))
            (set-face-background 'font-lock-constant-face (eide-i-config-get-option-value "light_color_theme_constant_background"))
            (set-face-foreground 'font-lock-constant-face (eide-i-config-get-option-value "light_color_theme_constant_foreground"))
            (set-face-background 'font-lock-builtin-face (eide-i-config-get-option-value "light_color_theme_builtin_background"))
            (set-face-foreground 'font-lock-builtin-face (eide-i-config-get-option-value "light_color_theme_builtin_foreground"))
            (set-face-background 'font-lock-string-face (eide-i-config-get-option-value "light_color_theme_string_background"))
            (set-face-foreground 'font-lock-string-face (eide-i-config-get-option-value "light_color_theme_foreground"))
            (set-face-foreground 'font-lock-comment-face (eide-i-config-get-option-value "light_color_theme_comment_foreground"))
            (set-face-background 'region (eide-i-config-get-option-value "light_color_theme_selection_background"))
            (set-face-foreground 'region (eide-i-config-get-option-value "light_color_theme_foreground"))))

        ;; Menu
        (setq eide-config-menu-background-color (eide-i-config-get-option-value "light_color_theme_menu_background"))
        (setq eide-config-menu-foreground-color "black")
        (set-face-foreground 'eide-config-menu-project-header-face "blue")
        (set-face-foreground 'eide-config-menu-project-name-face "red")

        ;; Menu: directories
        (set-face-background 'eide-config-menu-directory-face "lavender blush")
        (set-face-foreground 'eide-config-menu-directory-face "dark violet")
        (set-face-background 'eide-config-menu-directory-out-of-project-face "bisque")
        (set-face-foreground 'eide-config-menu-directory-out-of-project-face "red")

        ;; Menu: files
        (set-face-foreground 'eide-config-menu-file-rw-face "black")
        (set-face-foreground 'eide-config-menu-file-ro-face "gray55")
        (set-face-foreground 'eide-config-menu-file-nofile-face "black")
        (setq eide-config-menu-file-highlight-background-color "yellow")

        ;; Menu: functions
        (set-face-foreground 'eide-config-menu-function-face "blue")
        (set-face-background 'eide-config-menu-function-with-highlight-face "aquamarine")
        (set-face-foreground 'eide-config-menu-function-with-highlight-face "blue")

        ;; Help page
        (set-face-background 'eide-config-help-title-face "gold")
        (set-face-foreground 'eide-config-help-title-face "brown")
        (set-face-background 'eide-config-help-chapter1-face "yellow")
        (set-face-foreground 'eide-config-help-chapter1-face "red")
        (set-face-background 'eide-config-help-chapter2-face "lavender")
        (set-face-foreground 'eide-config-help-chapter2-face "blue")

        ;; Config files
        (setq eide-config-config-background-color "gray90")
        (setq eide-config-config-foreground-color "black")
        (set-face-foreground 'eide-config-config-comment-face "slate blue")
        (set-face-foreground 'eide-config-config-section-face "red")
        (set-face-foreground 'eide-config-config-parameter-face "brown")
        (set-face-foreground 'eide-config-config-possibilities-face "sea green")
        (set-face-foreground 'eide-config-config-separator-face "red")
        (set-face-background 'eide-config-config-value-face "white")
        (set-face-foreground 'eide-config-config-value-face "black")

        ;; Information line
        (set-face-background 'mode-line "wheat")))

    (if eide-config-use-color-theme-for-source-flag
      (progn
        ;; Background
        (set-background-color eide-config-background-color)
        ;; Normal text
        (set-foreground-color eide-config-foreground-color)
        ;; Left and right borders (same color as background)
        (set-face-background 'fringe eide-config-background-color))
      (progn
        (set-background-color eide-config-user-background-color)
        (set-foreground-color eide-config-user-foreground-color)
        (set-face-background 'fringe eide-config-user-background-color)
        (set-face-foreground 'font-lock-keyword-face eide-config-user-keyword-foreground-color)
        (set-face-foreground 'font-lock-type-face eide-config-user-type-foreground-color)
        (set-face-foreground 'font-lock-function-name-face eide-config-user-function-foreground-color)
        (set-face-foreground 'font-lock-variable-name-face eide-config-user-variable-foreground-color)
        (set-face-background 'font-lock-constant-face eide-config-user-constant-background-color)
        (set-face-foreground 'font-lock-constant-face eide-config-user-constant-foreground-color)
        (set-face-background 'font-lock-builtin-face eide-config-user-builtin-background-color)
        (set-face-foreground 'font-lock-builtin-face eide-config-user-builtin-foreground-color)
        (set-face-background 'font-lock-string-face eide-config-user-string-background-color)
        (set-face-foreground 'font-lock-string-face eide-config-user-string-foreground-color)
        (set-face-foreground 'font-lock-comment-face eide-config-user-comment-foreground-color)
        (set-face-background 'region eide-config-user-selection-background-color)
        (set-face-foreground 'region eide-config-user-selection-foreground-color)))

    (set-face-background 'eide-config-menu-default-face eide-config-menu-background-color)
    (set-face-foreground 'eide-config-menu-default-face eide-config-menu-foreground-color)
    (set-face-background 'eide-config-menu-project-header-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-project-name-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-rw-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-ro-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-nofile-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-ref-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-new-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-svn-modified-face eide-config-menu-background-color)

    ;; Menu: current file
    (copy-face 'eide-config-menu-file-rw-face 'eide-config-menu-current-file-rw-face)
    (copy-face 'eide-config-menu-file-ro-face 'eide-config-menu-current-file-ro-face)
    (copy-face 'eide-config-menu-file-nofile-face 'eide-config-menu-current-file-nofile-face)
    (copy-face 'eide-config-menu-file-ref-face 'eide-config-menu-current-file-ref-face)
    (copy-face 'eide-config-menu-file-new-face 'eide-config-menu-current-file-new-face)
    (copy-face 'eide-config-menu-file-svn-modified-face 'eide-config-menu-current-file-svn-modified-face)
    (set-face-background 'eide-config-menu-current-file-rw-face eide-config-menu-file-highlight-background-color)
    (set-face-background 'eide-config-menu-current-file-ro-face eide-config-menu-file-highlight-background-color)
    (set-face-background 'eide-config-menu-current-file-nofile-face eide-config-menu-file-highlight-background-color)
    (set-face-background 'eide-config-menu-current-file-ref-face eide-config-menu-file-highlight-background-color)
    (set-face-background 'eide-config-menu-current-file-new-face eide-config-menu-file-highlight-background-color)
    (set-face-background 'eide-config-menu-current-file-svn-modified-face eide-config-menu-file-highlight-background-color)

    (set-face-background 'eide-config-menu-function-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-empty-list-face eide-config-menu-background-color)
    (set-face-foreground 'eide-config-menu-empty-list-face eide-config-menu-foreground-color)

    (eide-config-set-colors-for-files)))

;; ----------------------------------------------------------------------------
;; Apply config.
;;
;; output : eide-config-menu-position : menu position (windows layout).
;;          eide-config-menu-height : menu height (windows layout).
;;          eide-config-c-indent-offset : indentation offset for C files.
;; ----------------------------------------------------------------------------
(defun eide-i-config-apply-config ()
  ;; Windows layout: menu position
  (setq eide-config-menu-position (eide-i-config-get-option-value "menu_position"))
  ;; If menu position is not correct, set default value
  (if (not (or (string-equal eide-config-menu-position "left")
               (string-equal eide-config-menu-position "right")))
    (setq eide-config-menu-position "right"))

  ;; Windows layout: menu height
  (setq eide-config-menu-height (eide-i-config-get-option-value "menu_height"))
  ;; If menu position is not correct, set default value
  (if (not (or (string-equal eide-config-menu-height "half")
               (string-equal eide-config-menu-height "full")))
    (setq eide-config-menu-height "half"))

  ;; Version control
  (if (string-equal (eide-i-config-get-option-value "show_svn_status") "auto")
    (if (file-exists-p (concat eide-root-directory ".svn"))
      (setq eide-config-show-svn-status-flag t)
      (setq eide-config-show-svn-status-flag nil))
    (if (string-equal (eide-i-config-get-option-value "show_svn_status") "yes")
      (setq eide-config-show-svn-status-flag t)
      (setq eide-config-show-svn-status-flag nil)))

  (setq eide-config-svn-diff-command (eide-i-config-get-option-value "svn_diff_command"))
  (if (string-equal eide-config-svn-diff-command "")
    (setq eide-config-svn-diff-command "svn diff ")
    (setq eide-config-svn-diff-command (concat "svn diff --diff-cmd=" eide-config-svn-diff-command " ")))

  (if (string-equal (eide-i-config-get-option-value "use_emacs_options") "yes")
    (setq eide-config-use-emacs-options-flag t)
    (setq eide-config-use-emacs-options-flag nil))

  (if eide-config-use-emacs-options-flag
    (progn

      ;; Size of characters for X system
      (if window-system
        (set-face-attribute 'default nil :height (string-to-number (eide-i-config-get-option-value "font_height"))))

      (if (string-equal (eide-i-config-get-option-value "use_color_theme_for_source") "yes")
        (setq eide-config-use-color-theme-for-source-flag t)
        (setq eide-config-use-color-theme-for-source-flag nil))

      (if (string-equal (eide-i-config-get-option-value "show_trailing_spaces") "yes")
        (setq eide-config-show-trailing-spaces-flag t)
        (setq eide-config-show-trailing-spaces-flag nil))

      ;; Coding rules
      ;; TODO: appliquer la valeur sans avoir à recharger les fichiers manuellement (F5)
      (setq eide-config-c-indent-offset (string-to-number (eide-i-config-get-option-value "c_indent_offset"))))

    (progn
      (setq eide-config-use-color-theme-for-source-flag nil)
      (setq eide-config-show-trailing-spaces-flag nil)
      (setq eide-config-c-indent-offset nil)))

  (eide-i-config-apply-color-theme))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Initialize config.
;; ----------------------------------------------------------------------------
(defun eide-config-init ()
  (setq eide-config-user-background-color (face-background 'default))
  (setq eide-config-user-foreground-color (face-foreground 'default))

  (setq eide-config-user-keyword-foreground-color (face-foreground 'font-lock-keyword-face))
  (setq eide-config-user-type-foreground-color (face-foreground 'font-lock-type-face))
  (setq eide-config-user-function-foreground-color (face-foreground 'font-lock-function-name-face))
  (setq eide-config-user-variable-foreground-color (face-foreground 'font-lock-variable-name-face))
  (setq eide-config-user-constant-background-color (face-background 'font-lock-constant-face))
  (setq eide-config-user-constant-foreground-color (face-foreground 'font-lock-constant-face))
  (setq eide-config-user-builtin-background-color (face-background 'font-lock-builtin-face))
  (setq eide-config-user-builtin-foreground-color (face-foreground 'font-lock-builtin-face))
  (setq eide-config-user-string-background-color (face-background 'font-lock-string-face))
  (setq eide-config-user-string-foreground-color (face-foreground 'font-lock-string-face))
  (setq eide-config-user-comment-foreground-color (face-foreground 'font-lock-comment-face))
  (setq eide-config-user-selection-background-color (face-background 'region))
  (setq eide-config-user-selection-foreground-color (face-foreground 'region)))

;; ----------------------------------------------------------------------------
;; Update config file.
;; ----------------------------------------------------------------------------
(defun eide-config-rebuild-config-file ()
  ;; Configuration values are read from source file (current config file) and
  ;; wrote into target file (which will replace config file in the end).
  ;; - If a parameter is not set in source file - a new parameter for example -
  ;;   it will be given default value in target file.
  ;; - If a parameter is set in source file but doesn't exist anymore - a
  ;;   deprecated parameter for example - it will not be present in target
  ;;   file.
  ;; Therefore, config file is always compliant with current version.

  (save-excursion
    (eide-i-config-rebuild-start "~/" eide-config-file)

    (eide-i-config-rebuild-insert-info "configuration")

    (eide-i-config-rebuild-insert-dotted-line t)
    (eide-i-config-rebuild-insert-comment "Following options are specific to Emacs-IDE.")
    (eide-i-config-rebuild-insert-dotted-line nil)

    (eide-i-config-rebuild-insert-section "display")
    (eide-i-config-rebuild-insert-comment "Color theme applies to 'menu' window. It may also apply to all other windows if 'use_color_theme_for_source' is set (see below).")
    (eide-i-config-rebuild-insert-comment "Possible values: dark or light.")
    (eide-i-config-rebuild-update-value "color_theme" "light")

    (eide-i-config-rebuild-insert-section "windows_layout")
    (eide-i-config-rebuild-insert-comment "Possible values: left or right.")
    (eide-i-config-rebuild-update-value "menu_position" "right")
    (eide-i-config-rebuild-insert-comment "Possible values: half or full.")
    (eide-i-config-rebuild-update-value "menu_height" "half")

    (eide-i-config-rebuild-insert-section "version_control")
    (eide-i-config-rebuild-insert-comment "Possible values:")
    (eide-i-config-rebuild-insert-comment "- yes: always check svn status,")
    (eide-i-config-rebuild-insert-comment "- no: never check svn status,")
    (eide-i-config-rebuild-insert-comment "- auto: check svn status if root directory is versioned by svn.")
    (eide-i-config-rebuild-update-value "show_svn_status" "auto")
    (eide-i-config-rebuild-update-value "svn_diff_command" "")

    (eide-i-config-rebuild-insert-section "default_project_commands")
    (eide-i-config-rebuild-insert-comment "All default commands are set in project configuration when project is created.")
    (eide-i-config-rebuild-insert-comment "Init command is called before all 'compile' and 'run' commands.")
    (eide-i-config-rebuild-update-value "default_init_command"      "")
    (eide-i-config-rebuild-update-value "default_compile_command_1" "make")
    (eide-i-config-rebuild-update-value "default_compile_command_2" "")
    (eide-i-config-rebuild-update-value "default_compile_command_3" "")
    (eide-i-config-rebuild-update-value "default_compile_command_4" "")
    (eide-i-config-rebuild-update-value "default_run_command_1"     "")
    (eide-i-config-rebuild-update-value "default_run_command_2"     "")
    (eide-i-config-rebuild-update-value "default_debug_command"     "gdb")
    (eide-i-config-rebuild-update-value "default_debug_program_1"   "")
    (eide-i-config-rebuild-update-value "default_debug_program_2"   "")

    (eide-i-config-rebuild-insert-section "dark_color_theme_colors_for_menu")
    (eide-i-config-rebuild-update-value "dark_color_theme_menu_background" "black")

    (eide-i-config-rebuild-insert-section "light_color_theme_colors_for_menu")
    (eide-i-config-rebuild-update-value "light_color_theme_menu_background" "white")

    (eide-i-config-rebuild-insert-dotted-line t)
    (eide-i-config-rebuild-insert-comment "Following options are not specific to Emacs-IDE. You can disable them (see")
    (eide-i-config-rebuild-insert-comment "'use_emacs_options' below) and configure them in your own way, in your")
    (eide-i-config-rebuild-insert-comment "~/.emacs file.")
    (eide-i-config-rebuild-insert-dotted-line nil)
    (eide-i-config-rebuild-insert-section "emacs_options")
    (eide-i-config-rebuild-insert-comment "If this flag is not set ('no'), all following options will be ignored.")
    (eide-i-config-rebuild-insert-comment "Possible values: yes or no.")
    (eide-i-config-rebuild-update-value "use_emacs_options" "yes")

    (eide-i-config-rebuild-insert-section "display")
    (eide-i-config-rebuild-insert-comment "Font height: an integer in units of 1/10 point.")
    (eide-i-config-rebuild-update-value "font_height" "105")
    (eide-i-config-rebuild-insert-comment "Possible values:")
    (eide-i-config-rebuild-insert-comment "- yes: use color theme for all windows,")
    (eide-i-config-rebuild-insert-comment "- no: do not override user colors, apply color theme on 'menu' window only.")
    (eide-i-config-rebuild-update-value "use_color_theme_for_source" "yes")
    (eide-i-config-rebuild-insert-comment "Possible values: yes or no.")
    (eide-i-config-rebuild-update-value "show_trailing_spaces" "no")

    (eide-i-config-rebuild-insert-section "coding_rules")
    (eide-i-config-rebuild-insert-comment "Indentation offset for C language.")
    (eide-i-config-rebuild-update-value "c_indent_offset" "2")

    (eide-i-config-rebuild-insert-section "dark_color_theme_colors_for_source")
    (eide-i-config-rebuild-update-value "dark_color_theme_background" "gray15")
    (eide-i-config-rebuild-update-value "dark_color_theme_foreground" "gray90")
    (eide-i-config-rebuild-update-value "dark_color_theme_keyword_foreground" "salmon")
    (eide-i-config-rebuild-update-value "dark_color_theme_type_foreground" "medium sea green")
    (eide-i-config-rebuild-update-value "dark_color_theme_function_foreground" "orange")
    (eide-i-config-rebuild-update-value "dark_color_theme_variable_foreground" "dark orange")
    (eide-i-config-rebuild-update-value "dark_color_theme_constant_background" "maroon4")
    (eide-i-config-rebuild-update-value "dark_color_theme_constant_foreground" "misty rose")
    (eide-i-config-rebuild-update-value "dark_color_theme_builtin_background" "brown")
    (eide-i-config-rebuild-update-value "dark_color_theme_builtin_foreground" "yellow")
    (eide-i-config-rebuild-update-value "dark_color_theme_string_background" "gray30")
    (eide-i-config-rebuild-update-value "dark_color_theme_comment_foreground" "deep sky blue")
    (eide-i-config-rebuild-update-value "dark_color_theme_selection_background" "gray50")

    (eide-i-config-rebuild-insert-section "light_color_theme_colors_for_source")
    (eide-i-config-rebuild-update-value "light_color_theme_background" "old lace")
    (eide-i-config-rebuild-update-value "light_color_theme_foreground" "black")
    (eide-i-config-rebuild-update-value "light_color_theme_keyword_foreground" "brown")
    (eide-i-config-rebuild-update-value "light_color_theme_type_foreground" "sea green")
    (eide-i-config-rebuild-update-value "light_color_theme_function_foreground" "red")
    (eide-i-config-rebuild-update-value "light_color_theme_variable_foreground" "orange red")
    (eide-i-config-rebuild-update-value "light_color_theme_constant_background" "misty rose")
    (eide-i-config-rebuild-update-value "light_color_theme_constant_foreground" "deep pink")
    (eide-i-config-rebuild-update-value "light_color_theme_builtin_background" "yellow")
    (eide-i-config-rebuild-update-value "light_color_theme_builtin_foreground" "red")
    (eide-i-config-rebuild-update-value "light_color_theme_string_background" "white")
    (eide-i-config-rebuild-update-value "light_color_theme_comment_foreground" "light slate blue")
    (eide-i-config-rebuild-update-value "light_color_theme_selection_background" "bisque")

    (eide-i-config-rebuild-stop)
    (eide-i-config-apply-config)
    ;; Close config file
    (kill-buffer eide-config-file)))

;; ----------------------------------------------------------------------------
;; Update project file.
;;
;; input  : eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-config-rebuild-project-file ()
  (save-excursion
    (eide-i-config-rebuild-start eide-root-directory eide-project-config-file)
    ;; Temporarily open config file (to get default values for project)
    (find-file-noselect (concat "~/" eide-config-file))

    (eide-i-config-rebuild-insert-info "project configuration")

    (eide-i-config-rebuild-insert-section "project_commands")
    (eide-i-config-rebuild-insert-comment "Init command is called before all 'compile' and 'run' commands.")
    (eide-i-config-rebuild-update-value-from-config "init_command"      "default_init_command")
    (eide-i-config-rebuild-update-value-from-config "compile_command_1" "default_compile_command_1")
    (eide-i-config-rebuild-update-value-from-config "compile_command_2" "default_compile_command_2")
    (eide-i-config-rebuild-update-value-from-config "compile_command_3" "default_compile_command_3")
    (eide-i-config-rebuild-update-value-from-config "compile_command_4" "default_compile_command_4")
    (eide-i-config-rebuild-update-value-from-config "run_command_1"     "default_run_command_1")
    (eide-i-config-rebuild-update-value-from-config "run_command_2"     "default_run_command_2")
    (eide-i-config-rebuild-update-value-from-config "debug_command"     "default_debug_command")
    (eide-i-config-rebuild-update-value-from-config "debug_program_1"   "default_debug_program_1")
    (eide-i-config-rebuild-update-value-from-config "debug_program_2"   "default_debug_program_2")

    ;; Close config files
    (kill-buffer eide-config-file)
    (eide-i-config-rebuild-stop)))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in project config.
;;
;; input  : p-parameter : config parameter.
;;          eide-root-directory : project root directory.
;; return : config value.
;; ----------------------------------------------------------------------------
(defun eide-config-get-project-value (p-parameter)
  (save-excursion
    (if (not (get-buffer eide-project-config-file))
      (find-file-noselect (concat eide-root-directory eide-project-config-file)))
    (set-buffer eide-project-config-file)
    (eide-i-config-get-value p-parameter)))

;; ----------------------------------------------------------------------------
;; Display config file (full frame).
;; ----------------------------------------------------------------------------
(defun eide-config-open-config-file ()
  (eide-windows-layout-unbuild)
  (eide-config-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-windows-find-file-without-advice (concat "~/" eide-config-file)))

;; ----------------------------------------------------------------------------
;; Display project file (full frame).
;;
;; input  : eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-config-open-project-file ()
  (eide-windows-layout-unbuild)
  (eide-config-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-windows-find-file-without-advice (concat eide-root-directory eide-project-config-file))
  (goto-char (point-min)))

;; ----------------------------------------------------------------------------
;; Display project notes file (full frame).
;;
;; input  : eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-config-open-project-notes-file ()
  (eide-windows-layout-unbuild)
  (eide-config-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-windows-find-file-without-advice (concat eide-root-directory eide-project-notes-file)))

;; ----------------------------------------------------------------------------
;; Set colors for config buffer.
;; ----------------------------------------------------------------------------
(defun eide-config-set-colors-for-config ()
  (set-background-color eide-config-config-background-color)
  (set-foreground-color eide-config-config-foreground-color)
  (set-face-background 'fringe eide-config-config-background-color))

;; ----------------------------------------------------------------------------
;; Set colors for "help" buffer.
;; ----------------------------------------------------------------------------
(defun eide-config-set-colors-for-help ()
  (set-background-color eide-config-menu-background-color)
  (set-foreground-color eide-config-menu-foreground-color)
  (set-face-background 'fringe eide-config-menu-background-color))

;; ----------------------------------------------------------------------------
;; Set colors for edition mode.
;; ----------------------------------------------------------------------------
(defun eide-config-set-colors-for-files ()
  (if eide-config-use-color-theme-for-source-flag
    (progn
      (set-background-color eide-config-background-color)
      (set-foreground-color eide-config-foreground-color)
      (set-face-background 'fringe eide-config-background-color))
    (progn
      (set-background-color eide-config-user-background-color)
      (set-foreground-color eide-config-user-foreground-color)
      (set-face-background 'fringe eide-config-user-background-color))))

;;; eide-config.el ends here
