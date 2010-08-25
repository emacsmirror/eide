;;; eide-config.el --- Emacs-IDE, config

;; Copyright (C) 2005-2009 Cédric Marie

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(provide 'eide-config)


;;;; ==========================================================================
;;;; OPTIONS
;;;; ==========================================================================

;; Option values : t = on / nil = off

;; Grep-find
(defvar eide-option-search-grep-find-on-2-lines-flag nil)

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
  (setq font-lock-defaults '('(("\\(.*\\)(.*):" 1 'eide-config-config-parameter-face) ; parameter (with possibilities)
                               ("\\(.*\\):"     1 'eide-config-config-parameter-face) ; parameter
                               ("\\((.*)\\):"   1 'eide-config-config-possibilities-face) ; "(possibilities)"
                               (":"             . 'eide-config-config-separator-face) ; ":"
                               (":\\(.*\\)"     1 'eide-config-config-value-face))))) ; value

(setq auto-mode-alist (append '(("\\.emacs-ide.options\\'" . emacs-ide-config-mode)
                                ("\\.emacs-ide.project\\'" . emacs-ide-config-mode)) auto-mode-alist))


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
(make-face 'eide-config-menu-file-face)
(make-face 'eide-config-menu-file-ref-face)
(make-face 'eide-config-menu-file-new-face)
(make-face 'eide-config-menu-file-rw-face)
(make-face 'eide-config-menu-function-face)
(make-face 'eide-config-menu-function-with-highlight-face)
(make-face 'eide-config-menu-empty-list-face)

;; Help page
(make-face 'eide-config-help-title-face)
(make-face 'eide-config-help-chapter1-face)
(make-face 'eide-config-help-chapter2-face)

;; Config files
(make-face 'eide-config-config-parameter-face)
(make-face 'eide-config-config-possibilities-face)
(make-face 'eide-config-config-separator-face)
(make-face 'eide-config-config-value-face)

(make-face-bold 'eide-config-config-parameter-face)

;; Parenthese matching (requires show-paren-mode)
;;(set-face-background 'show-paren-match-face "orange")

;; Compilation warnings and file path in a grep result
;; (because grep uses "compile" mode to display its results)
(set-face-foreground 'font-lock-warning-face "tan")

;; Code
(make-face-bold 'font-lock-keyword-face)
(make-face-bold 'font-lock-function-name-face)

(make-face 'font-my-dos-face)
(set-face-foreground 'font-my-dos-face "wheat")

;; Menu
(make-face-bold 'eide-config-menu-project-header-face)
(make-face-bold 'eide-config-menu-project-name-face)

(make-face-bold 'eide-config-menu-file-face)
(set-face-foreground 'eide-config-menu-file-ref-face "orange red")
(make-face-bold 'eide-config-menu-file-ref-face)
(set-face-foreground 'eide-config-menu-file-new-face "medium sea green")
(make-face-bold 'eide-config-menu-file-new-face)
(make-face-bold 'eide-config-menu-file-rw-face)

(make-face-italic 'eide-config-menu-empty-list-face)

;; Help page
(make-face-bold 'eide-config-help-title-face)

;; Hidden text (for hide/show minor mode)
;; Does not work with Emacs 22.3 : I comment it until I can test
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

;; Current difference : what is common or only in one buffer
(copy-face 'default 'ediff-current-diff-face-A)
(set-face-background 'ediff-current-diff-face-A "pink")
(set-face-foreground 'ediff-current-diff-face-A "black")

(copy-face 'default 'ediff-current-diff-face-B)
(set-face-background 'ediff-current-diff-face-B "pink")
(set-face-foreground 'ediff-current-diff-face-B "black")

;; Current difference : what really differs
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
(defun eide-l-config-get-value-if-defined (p-parameter)
  (goto-char (point-min))
  (if (re-search-forward (concat "^" p-parameter ":") nil t)
    (buffer-substring-no-properties (point) (line-end-position))
    (progn
      (goto-char (point-min))
      (if (and (re-search-forward (concat "^" p-parameter "(") nil t) (search-forward "):" nil t))
        (buffer-substring-no-properties (point) (line-end-position))
        nil))))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in a config (current buffer).
;;
;; input  : p-parameter : config parameter.
;; return : value as a string, or "" if it is not defined.
;; ----------------------------------------------------------------------------
(defun eide-l-config-get-value (p-parameter)
  (let ((l-value (eide-l-config-get-value-if-defined p-parameter)))
    (if l-value
      l-value
      "")))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in options file.
;;
;; input  : p-parameter : config parameter.
;; return : config value.
;; ----------------------------------------------------------------------------
(defun eide-l-config-get-option-value (p-parameter)
  (save-excursion
    (if (not (get-buffer eide-options-file))
      (find-file-noselect (concat "~/" eide-options-file)))
    (set-buffer eide-options-file)
    (eide-l-config-get-value p-parameter)))

;; ----------------------------------------------------------------------------
;; Prepare update of config file (in a temporary file).
;;
;; input  : p-path : path of config file.
;;          p-file : name of config file.
;; output : eide-config-path : path of config file.
;;          eide-config-source-buffer : name of config file.
;;          eide-config-target-buffer : temporary buffer for update.
;; ----------------------------------------------------------------------------
(defun eide-l-config-rebuild-start (p-path p-file)
  ;; Define source and target config files
  (setq eide-config-path p-path)
  (setq eide-config-source-buffer p-file)
  (setq eide-config-target-buffer (concat p-file "_temp"))

  ;; Open these config files
  (if (not (get-buffer eide-config-source-buffer))
    (find-file-noselect (concat eide-config-path eide-config-source-buffer)))
  (find-file-noselect (concat eide-config-path eide-config-target-buffer))
  (set-buffer eide-config-target-buffer)
  (delete-region (point-min) (point-max)))

;; ----------------------------------------------------------------------------
;; Clean after update of config file.
;;
;; input  : eide-config-path : path of config file.
;;          eide-config-source-buffer : name of config file.
;;          eide-config-target-buffer : temporary buffer for update.
;; ----------------------------------------------------------------------------
(defun eide-l-config-rebuild-stop ()
  ;; Save target file
  (set-buffer eide-config-target-buffer)
  (ad-deactivate 'save-buffer)
  (save-buffer)
  (ad-activate 'save-buffer)
  ;; Replace source file by target file if different
  (shell-command (concat "cd " eide-config-path " ; diff -q " eide-config-source-buffer " " eide-config-target-buffer " > /dev/null || cp -f " eide-config-target-buffer " " eide-config-source-buffer " ; rm -f " eide-config-target-buffer))
  ;; Update buffer from file
  (set-buffer eide-config-source-buffer)
  (revert-buffer)
  ;; Close temp buffer
  (kill-buffer eide-config-target-buffer))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in config file.
;;
;; input  : p-parameter : config parameter.
;;          eide-config-source-buffer : name of config file.
;; return : config value.
;; ----------------------------------------------------------------------------
(defun eide-l-config-rebuild-get-current-value (p-parameter)
  (set-buffer eide-config-source-buffer)
  (eide-l-config-get-value-if-defined p-parameter))

;; ----------------------------------------------------------------------------
;; Insert information in config file.
;;
;; input  : p-config-file : string describing config file.
;;          eide-config-target-buffer : temporary config buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-config-rebuild-insert-info (p-config-file)
  (set-buffer eide-config-target-buffer)
  (insert "\n*******************************************************************************\n")
  (insert (concat "Emacs-IDE " p-config-file))
  (insert "\n*******************************************************************************\n\n")
  (insert "--> Click right to exit this page.\n")
  (if (string-equal p-config-file "options")
    (insert "--> Press shift + click right to show/hide the list of available colors.\n"))
  (insert "--> To restore the default value of a parameter, delete the line\n")
  (insert (concat "    (" p-config-file " file is rebuilt when you exit this page).\n\n")))

;; ----------------------------------------------------------------------------
;; Insert section header in config file.
;;
;; input  : p-section : section header (string).
;;          eide-config-target-buffer : temporary config buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-config-rebuild-insert-section (p-section)
  (set-buffer eide-config-target-buffer)
  (insert "\n-------------------------------------------------------------------------------\n")
  (insert p-section)
  (insert "\n-------------------------------------------------------------------------------\n\n"))

;; ----------------------------------------------------------------------------
;; Insert a line with a parameter and its value in config file.
;;
;; input  : p-parameter : config parameter.
;;          p-value : config value.
;;          p-possibilities : possible values.
;;          eide-config-target-buffer : temporary config buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-config-rebuild-insert-parameter (p-parameter p-value &optional p-possibilities)
  (set-buffer eide-config-target-buffer)
  (insert p-parameter)
  (if p-possibilities
    (progn
      (insert "(")
      (insert p-possibilities)
      (insert ")")))
  (insert ":")
  (insert p-value)
  (insert "\n"))

;; ----------------------------------------------------------------------------
;; Update a line with a parameter and its value (default if not found).
;;
;; input  : p-parameter : config parameter.
;;          p-default-value : config default value.
;;          p-possibilities : possible values.
;; ----------------------------------------------------------------------------
(defun eide-l-config-rebuild-update-value (p-parameter p-default-value &optional p-possibilities)
  (let ((l-value (eide-l-config-rebuild-get-current-value p-parameter)))
    ;; If the parameter is not present, or it is a color and the value is not
    ;; correct, we use default value.
    (if (not l-value)
      (setq l-value p-default-value))
    (if (and (string-match "color_theme_.*" p-parameter)
             (not (color-defined-p l-value)))
      (progn
        (eide-popup-message (concat "Warning : " p-parameter " value \"" l-value "\" is not correct,\nusing default value \"" p-default-value "\" instead."))
        (setq l-value p-default-value)))
    (eide-l-config-rebuild-insert-parameter p-parameter l-value p-possibilities)))

;; ----------------------------------------------------------------------------
;; Update a line with a parameter and its value (default from options if not
;; found).
;;
;; input  : p-parameter : config parameter.
;;          p-options-parameter : related parameter in options.
;; ----------------------------------------------------------------------------
(defun eide-l-config-rebuild-update-value-from-options (p-parameter p-options-parameter)
  (let ((l-value (eide-l-config-rebuild-get-current-value p-parameter)))
    (if (not l-value)
      (setq l-value (eide-l-config-get-option-value p-options-parameter)))
    (eide-l-config-rebuild-insert-parameter p-parameter l-value)))

;; ----------------------------------------------------------------------------
;; Apply color theme.
;; ----------------------------------------------------------------------------
(defun eide-l-config-apply-color-theme ()
  (let ((l-color-theme (eide-l-config-get-option-value "color_theme")))
    (if (string-equal l-color-theme "dark")
      (progn
        ;; "dark" theme
        (setq eide-config-background-color (eide-l-config-get-option-value "color_theme_dark_background"))
        (setq eide-config-foreground-color (eide-l-config-get-option-value "color_theme_dark_foreground"))

        ;; Code
        (set-face-foreground 'font-lock-keyword-face (eide-l-config-get-option-value "color_theme_dark_keyword_foreground"))
        (set-face-foreground 'font-lock-type-face (eide-l-config-get-option-value "color_theme_dark_type_foreground"))
        (set-face-foreground 'font-lock-function-name-face (eide-l-config-get-option-value "color_theme_dark_function_foreground"))
        (set-face-foreground 'font-lock-variable-name-face (eide-l-config-get-option-value "color_theme_dark_variable_foreground"))
        (set-face-background 'font-lock-constant-face (eide-l-config-get-option-value "color_theme_dark_constant_background"))
        (set-face-foreground 'font-lock-constant-face (eide-l-config-get-option-value "color_theme_dark_constant_foreground"))
        (set-face-background 'font-lock-builtin-face (eide-l-config-get-option-value "color_theme_dark_builtin_background"))
        (set-face-foreground 'font-lock-builtin-face (eide-l-config-get-option-value "color_theme_dark_builtin_foreground"))
        (set-face-background 'font-lock-string-face (eide-l-config-get-option-value "color_theme_dark_string_background")) 
        (set-face-foreground 'font-lock-string-face (eide-l-config-get-option-value "color_theme_dark_foreground")) 
        (set-face-foreground 'font-lock-comment-face (eide-l-config-get-option-value "color_theme_dark_comment_foreground"))
        (set-face-background 'region (eide-l-config-get-option-value "color_theme_dark_selection_background"))
        (set-face-foreground 'region (eide-l-config-get-option-value "color_theme_dark_foreground"))

        ;; Menu
        (setq eide-config-menu-background-color (eide-l-config-get-option-value "color_theme_dark_menu_background"))
        (setq eide-config-menu-foreground-color "white")
        (set-face-foreground 'eide-config-menu-project-header-face "deep sky blue")
        (set-face-foreground 'eide-config-menu-project-name-face "orange")

        ;; Menu : directories
        (set-face-background 'eide-config-menu-directory-face "dim gray")
        (set-face-foreground 'eide-config-menu-directory-face "white")
        (set-face-background 'eide-config-menu-directory-out-of-project-face "saddle brown")
        (set-face-foreground 'eide-config-menu-directory-out-of-project-face "peach puff")

        ;; Menu : files
        (set-face-foreground 'eide-config-menu-file-rw-face "gray95")
        (set-face-foreground 'eide-config-menu-file-face "gray65")
        (setq eide-config-menu-file-highlight-background-color "brown")

        ;; Menu : functions
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
        (set-face-foreground 'eide-config-config-parameter-face "salmon")
        (set-face-foreground 'eide-config-config-possibilities-face "medium sea green")
        (set-face-foreground 'eide-config-config-separator-face "orange red")
        (set-face-background 'eide-config-config-value-face "gray30")
        (set-face-foreground 'eide-config-config-value-face "white")

        ;; Information line
        (set-face-background 'mode-line "gray"))

      (progn
        ;; "light" theme
        (setq eide-config-background-color (eide-l-config-get-option-value "color_theme_light_background"))
        (setq eide-config-foreground-color (eide-l-config-get-option-value "color_theme_light_foreground"))

        ;; Code
        (set-face-foreground 'font-lock-keyword-face (eide-l-config-get-option-value "color_theme_light_keyword_foreground"))
        (set-face-foreground 'font-lock-type-face (eide-l-config-get-option-value "color_theme_light_type_foreground"))
        (set-face-foreground 'font-lock-function-name-face (eide-l-config-get-option-value "color_theme_light_function_foreground"))
        (set-face-foreground 'font-lock-variable-name-face (eide-l-config-get-option-value "color_theme_light_variable_foreground"))
        (set-face-background 'font-lock-constant-face (eide-l-config-get-option-value "color_theme_light_constant_background"))
        (set-face-foreground 'font-lock-constant-face (eide-l-config-get-option-value "color_theme_light_constant_foreground"))
        (set-face-background 'font-lock-builtin-face (eide-l-config-get-option-value "color_theme_light_builtin_background"))
        (set-face-foreground 'font-lock-builtin-face (eide-l-config-get-option-value "color_theme_light_builtin_foreground"))
        (set-face-background 'font-lock-string-face (eide-l-config-get-option-value "color_theme_light_string_background")) 
        (set-face-foreground 'font-lock-string-face (eide-l-config-get-option-value "color_theme_light_foreground")) 
        (set-face-foreground 'font-lock-comment-face (eide-l-config-get-option-value "color_theme_light_comment_foreground"))
        (set-face-background 'region (eide-l-config-get-option-value "color_theme_light_selection_background"))
        (set-face-foreground 'region (eide-l-config-get-option-value "color_theme_light_foreground"))

        ;; Menu
        (setq eide-config-menu-background-color (eide-l-config-get-option-value "color_theme_light_menu_background"))
        (setq eide-config-menu-foreground-color "black")
        (set-face-foreground 'eide-config-menu-project-header-face "blue")
        (set-face-foreground 'eide-config-menu-project-name-face "red")

        ;; Menu : directories
        (set-face-background 'eide-config-menu-directory-face "lavender blush")
        (set-face-foreground 'eide-config-menu-directory-face "dark violet")
        (set-face-background 'eide-config-menu-directory-out-of-project-face "bisque")
        (set-face-foreground 'eide-config-menu-directory-out-of-project-face "red")

        ;; Menu : files
        (set-face-foreground 'eide-config-menu-file-rw-face "black")
        (set-face-foreground 'eide-config-menu-file-face "gray55")
        (setq eide-config-menu-file-highlight-background-color "yellow")

        ;; Menu : functions
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
        (set-face-foreground 'eide-config-config-parameter-face "brown")
        (set-face-foreground 'eide-config-config-possibilities-face "sea green")
        (set-face-foreground 'eide-config-config-separator-face "red")
        (set-face-background 'eide-config-config-value-face "white")
        (set-face-foreground 'eide-config-config-value-face "black")

        ;; Information line
        (set-face-background 'mode-line "wheat")))

    ;; Normal text
    (set-foreground-color eide-config-foreground-color)

    ;; Left and right borders (same color as background)
    (set-face-background 'fringe eide-config-background-color)

    (set-face-background 'eide-config-menu-default-face eide-config-menu-background-color)
    (set-face-foreground 'eide-config-menu-default-face eide-config-menu-foreground-color)
    (set-face-background 'eide-config-menu-project-header-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-project-name-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-ref-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-new-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-file-rw-face eide-config-menu-background-color)

    ;; Menu : current file
    (copy-face 'eide-config-menu-file-face 'eide-config-menu-current-file-face)
    (copy-face 'eide-config-menu-file-ref-face 'eide-config-menu-current-file-ref-face)
    (copy-face 'eide-config-menu-file-new-face 'eide-config-menu-current-file-new-face)
    (copy-face 'eide-config-menu-file-rw-face 'eide-config-menu-current-file-rw-face)
    (set-face-background 'eide-config-menu-current-file-face eide-config-menu-file-highlight-background-color)
    (set-face-background 'eide-config-menu-current-file-ref-face eide-config-menu-file-highlight-background-color)
    (set-face-background 'eide-config-menu-current-file-new-face eide-config-menu-file-highlight-background-color)
    (set-face-background 'eide-config-menu-current-file-rw-face eide-config-menu-file-highlight-background-color)

    (set-face-background 'eide-config-menu-function-face eide-config-menu-background-color)
    (set-face-background 'eide-config-menu-empty-list-face eide-config-menu-background-color)
    (set-face-foreground 'eide-config-menu-empty-list-face eide-config-menu-foreground-color)

    (eide-config-set-colors-for-files)))

;; ----------------------------------------------------------------------------
;; Apply options.
;;
;; output : eide-config-menu-position : menu position (windows layout).
;;          eide-config-menu-height : menu height (windows layout).
;;          eide-c-indent-offset : indentation offset for C files.
;; ----------------------------------------------------------------------------
(defun eide-l-config-apply-options ()
  ;; Size of characters for X system
  (if window-system
    ;;(if (eq system-type 'windows-nt)
    ;;  (set-default-font (concat "-*-fixed-medium-r-*-*-" (eide-l-config-get-option-value "font_size_for_windows") "-*-*-*-c-*-iso8859-1"))
    (let ((l-string (concat "-*-fixed-medium-r-*-*-" (eide-l-config-get-option-value "font_size") "-*-*-*-c-*-iso8859-1")))
      (set-default-font l-string)))

  (eide-l-config-apply-color-theme)
  (if (string-equal (eide-l-config-get-option-value "show_trailing_spaces") "yes")
    (setq eide-config-show-trailing-spaces t)
    (setq eide-config-show-trailing-spaces nil))

  ;; Windows layout : menu position
  (setq eide-config-menu-position (eide-l-config-get-option-value "menu_position"))
  ;; If menu position is not correct, set default value
  (if (not (or (string-equal eide-config-menu-position "left")
               (string-equal eide-config-menu-position "right")))
    (setq eide-config-menu-position "right"))

  ;; Windows layout : menu height
  (setq eide-config-menu-height (eide-l-config-get-option-value "menu_height"))
  ;; If menu position is not correct, set default value
  (if (not (or (string-equal eide-config-menu-height "half")
               (string-equal eide-config-menu-height "full")))
    (setq eide-config-menu-height "half"))

  ;; Coding rules
  ;; TODO : appliquer la valeur sans avoir à recharger les fichiers manuellement (F5)
  (setq eide-c-indent-offset (string-to-number (eide-l-config-get-option-value "c_indent_offset"))))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Update options file.
;; ----------------------------------------------------------------------------
(defun eide-config-rebuild-options-file ()
  ;; Configuration values are read from source file (current config file) and
  ;; wrote into target file (which will replace config file in the end).
  ;; - If a parameter is not set in source file - a new parameter for example -
  ;;   it will be given default value in target file.
  ;; - If a parameter is set in source file but doesn't exist anymore - a
  ;;   deprecated parameter for example - it will not be present in target
  ;;   file.
  ;; Therefore, config file is always compliant with current version.

  (save-excursion
    (eide-l-config-rebuild-start "~/" eide-options-file)

    (eide-l-config-rebuild-insert-info "options")
    (eide-l-config-rebuild-insert-section "Display")
    (eide-l-config-rebuild-update-value "font_size" "18")
    (eide-l-config-rebuild-update-value "color_theme" "light" "dark/light")
    (eide-l-config-rebuild-update-value "show_trailing_spaces" "no" "yes/no")

    (eide-l-config-rebuild-insert-section "Customized colors for dark theme")
    (eide-l-config-rebuild-update-value "color_theme_dark_background" "gray15")
    (eide-l-config-rebuild-update-value "color_theme_dark_foreground" "gray90")
    (eide-l-config-rebuild-update-value "color_theme_dark_keyword_foreground" "salmon")
    (eide-l-config-rebuild-update-value "color_theme_dark_type_foreground" "medium sea green")
    (eide-l-config-rebuild-update-value "color_theme_dark_function_foreground" "orange")
    (eide-l-config-rebuild-update-value "color_theme_dark_variable_foreground" "dark orange")
    (eide-l-config-rebuild-update-value "color_theme_dark_constant_background" "maroon4")
    (eide-l-config-rebuild-update-value "color_theme_dark_constant_foreground" "misty rose")
    (eide-l-config-rebuild-update-value "color_theme_dark_builtin_background" "brown")
    (eide-l-config-rebuild-update-value "color_theme_dark_builtin_foreground" "yellow")
    (eide-l-config-rebuild-update-value "color_theme_dark_string_background" "gray30")
    (eide-l-config-rebuild-update-value "color_theme_dark_comment_foreground" "deep sky blue")
    (eide-l-config-rebuild-update-value "color_theme_dark_selection_background" "gray50")
    (eide-l-config-rebuild-update-value "color_theme_dark_menu_background" "black")

    (eide-l-config-rebuild-insert-section "Customized colors for light theme")
    (eide-l-config-rebuild-update-value "color_theme_light_background" "old lace")
    (eide-l-config-rebuild-update-value "color_theme_light_foreground" "black")
    (eide-l-config-rebuild-update-value "color_theme_light_keyword_foreground" "brown")
    (eide-l-config-rebuild-update-value "color_theme_light_type_foreground" "sea green")
    (eide-l-config-rebuild-update-value "color_theme_light_function_foreground" "red")
    (eide-l-config-rebuild-update-value "color_theme_light_variable_foreground" "orange red")
    (eide-l-config-rebuild-update-value "color_theme_light_constant_background" "misty rose")
    (eide-l-config-rebuild-update-value "color_theme_light_constant_foreground" "deep pink")
    (eide-l-config-rebuild-update-value "color_theme_light_builtin_background" "yellow")
    (eide-l-config-rebuild-update-value "color_theme_light_builtin_foreground" "red")
    (eide-l-config-rebuild-update-value "color_theme_light_string_background" "white")
    (eide-l-config-rebuild-update-value "color_theme_light_comment_foreground" "light slate blue")
    (eide-l-config-rebuild-update-value "color_theme_light_selection_background" "bisque")
    (eide-l-config-rebuild-update-value "color_theme_light_menu_background" "white")

    (eide-l-config-rebuild-insert-section "Windows layout")
    (eide-l-config-rebuild-update-value "menu_position" "right" "left/right")
    (eide-l-config-rebuild-update-value "menu_height" "half" "half/full")

    (eide-l-config-rebuild-insert-section "Coding rules")

    ;; Compatibility with Emacs-IDE v1.0
    (let ((l-old-value (eide-l-config-rebuild-get-current-value "indent_offset")))
      (if l-old-value
        (eide-l-config-rebuild-update-value "c_indent_offset" l-old-value)
        (eide-l-config-rebuild-update-value "c_indent_offset" "2")))

    (eide-l-config-rebuild-insert-section "Compilation / run / debug commands (default for new project)")
    (eide-l-config-rebuild-update-value "project_default_init_command"      "")
    (eide-l-config-rebuild-update-value "project_default_compile_command_1" "make")
    (eide-l-config-rebuild-update-value "project_default_compile_command_2" "make")
    (eide-l-config-rebuild-update-value "project_default_run_command_1"     "./program")
    (eide-l-config-rebuild-update-value "project_default_run_command_2"     "./program")
    (eide-l-config-rebuild-update-value "project_default_debug_command_1"   "gdb program")
    (eide-l-config-rebuild-update-value "project_default_debug_command_2"   "gdb program")

    (eide-l-config-rebuild-stop)
    (eide-l-config-apply-options)
    ;; Close options file
    (kill-buffer eide-options-file)))

;; ----------------------------------------------------------------------------
;; Update project file.
;;
;; input  : eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-config-rebuild-project-file ()
  (save-excursion
    (eide-l-config-rebuild-start eide-root-directory eide-project-file)
    ;; Temporarily open options file (to get default values for project)
    (find-file-noselect (concat "~/" eide-options-file))

    (eide-l-config-rebuild-insert-info "project configuration")

    (eide-l-config-rebuild-insert-section "Compilation / run /debug commands")
    (eide-l-config-rebuild-update-value-from-options "init_command"      "project_default_init_command")
    (eide-l-config-rebuild-update-value-from-options "compile_command_1" "project_default_compile_command_1")
    (eide-l-config-rebuild-update-value-from-options "compile_command_2" "project_default_compile_command_2")
    (eide-l-config-rebuild-update-value-from-options "run_command_1"     "project_default_run_command_1")
    (eide-l-config-rebuild-update-value-from-options "run_command_2"     "project_default_run_command_2")
    (eide-l-config-rebuild-update-value-from-options "debug_command_1"   "project_default_debug_command_1")
    (eide-l-config-rebuild-update-value-from-options "debug_command_2"   "project_default_debug_command_2")

    ;; Close options files
    (kill-buffer eide-options-file)
    (eide-l-config-rebuild-stop)))

;; ----------------------------------------------------------------------------
;; Get the value of a parameter in project config.
;;
;; input  : p-parameter : config parameter.
;;          eide-root-directory : project root directory.
;; return : config value.
;; ----------------------------------------------------------------------------
(defun eide-config-get-project-value (p-parameter)
  (save-excursion
    (if (not (get-buffer eide-project-file))
      (find-file-noselect (concat eide-root-directory eide-project-file)))
    (set-buffer eide-project-file)
    (eide-l-config-get-value p-parameter)))

;; ----------------------------------------------------------------------------
;; Display options file (full frame).
;; ----------------------------------------------------------------------------
(defun eide-config-open-options-file ()
  (eide-windows-layout-unbuild)
  (eide-config-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-menu-find-file-without-advice (concat "~/" eide-options-file)))

;; ----------------------------------------------------------------------------
;; Display project file (full frame).
;;
;; input  : eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-config-open-project-file ()
  (eide-windows-layout-unbuild)
  (eide-config-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-menu-find-file-without-advice (concat eide-root-directory eide-project-file))
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
  (eide-menu-find-file-without-advice (concat eide-root-directory eide-project-notes-file)))

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
  (set-background-color eide-config-background-color)
  (set-foreground-color eide-config-foreground-color)
  (set-face-background 'fringe eide-config-background-color))

;;; eide-config.el ends here
