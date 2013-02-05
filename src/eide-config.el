;;; eide-config.el --- Emacs-IDE, config

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

(provide 'eide-config)

(require 'eide-menu)
(require 'eide-vc)

(defvar eide-config-ready nil)
(defvar eide-config-show-svn-status-flag nil)
(defvar eide-config-show-git-status-flag nil)

(defvar eide-config-background-color nil)
(defvar eide-config-foreground-color nil)
(defvar eide-config-menu-background-color nil)
(defvar eide-config-menu-foreground-color nil)
(defvar eide-config-menu-file-highlight-background-color nil)
(defvar eide-config-config-background-color nil)
(defvar eide-config-config-foreground-color nil)
(defvar eide-config-menu-use-specific-background-color nil)

(defvar eide-config-user-menu-bar-mode nil)
(defvar eide-config-user-tool-bar-mode nil)
(defvar eide-config-user-scroll-bar-mode nil)
(defvar eide-config-user-font-height nil)
(defvar eide-config-user-indent-tabs-mode nil)
(defvar eide-config-user-tab-width nil)
(defvar eide-config-user-background-color nil)
(defvar eide-config-user-foreground-color nil)
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
(defvar eide-config-user-cscope-do-not-update-database nil)

(defvar eide-config-target-buffer nil)

;; ----------------------------------------------------------------------------
;; OPTIONS
;; ----------------------------------------------------------------------------

;; Exclude "_" from word delimiters (when selecting by double-click)
(defvar eide-option-select-whole-symbol-flag t)

;; When using a file (.ref or .new for example), update file date,
;; so that compilation takes it into account.
(defvar eide-option-touch-files-when-using-flag t)

(defvar eide-option-menu-buffer-popup-groups-flags nil)

;; ----------------------------------------------------------------------------
;; SETTINGS FOR MAJOR MODE "EMACS-IDE-CONFIG"
;; ----------------------------------------------------------------------------

(define-derived-mode emacs-ide-config-mode fundamental-mode "Emacs-IDE config"
  (setq font-lock-defaults '('(("\\(#.*\\)"      1 'eide-config-config-comment-face) ; comment
                               ("\\(.*\\) = "    1 'eide-config-config-parameter-face) ; parameter
                               (" = "            . 'eide-config-config-separator-face) ; " = "
                               (" = \\(.*\\)"    1 'eide-config-config-value-face))))) ; value

(setq auto-mode-alist (append '(("\\.emacs-ide-project.cfg\\'" . emacs-ide-config-mode)) auto-mode-alist))

;; ----------------------------------------------------------------------------
;; SYNTAX HIGHLIGHTING
;; ----------------------------------------------------------------------------

(require 'font-lock)

;; Enable syntax highlighting
(global-font-lock-mode t)

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
(make-face 'eide-config-menu-file-vc-modified-face)
(make-face 'eide-config-menu-function-face)
(make-face 'eide-config-menu-function-with-highlight-face)
(make-face 'eide-config-menu-empty-list-face)

;; Help page
(make-face 'eide-config-help-title-face)
(make-face 'eide-config-help-chapter1-face)
(make-face 'eide-config-help-chapter2-face)

;; Config files
(make-face 'eide-config-config-comment-face)
(make-face 'eide-config-config-parameter-face)
(make-face 'eide-config-config-possibilities-face)
(make-face 'eide-config-config-separator-face)
(make-face 'eide-config-config-value-face)
(make-face-bold 'eide-config-config-parameter-face)

;; Projects list
(make-face 'eide-config-project-name-face)
(make-face-bold 'eide-config-project-name-face)
(make-face 'eide-config-project-current-name-face)
(make-face-bold 'eide-config-project-current-name-face)
(make-face 'eide-config-project-comparison-name-face)
(make-face-bold 'eide-config-project-comparison-name-face)

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
(make-face-bold 'eide-config-menu-file-vc-modified-face)

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

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION VARIABLES
;; ----------------------------------------------------------------------------

(defgroup eide nil "Customization of Emacs-IDE."
  :tag "Emacs-IDE"
  :group 'emacs)
(defcustom eide-custom-override-emacs-settings t "Enable or disable \"Emacs settings\" group. If disabled, Emacs-IDE will not override any default or user setting. If enabled, Emacs-IDE will override some default or user settings, in order to provide a more user-friendly interface, and each setting can be enabled or disabled individually in \"Emacs settings\" group."
  :tag "Override Emacs settings"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :set '(lambda (param value) (set-default param value) (eide-i-config-apply-emacs-settings))
  :initialize 'custom-initialize-default
  :group 'eide)

(defgroup eide-display nil "Colors, menu, and windows layout."
  :tag "Display"
  :group 'eide)
(defcustom eide-custom-color-theme 'light "Color theme for menu. To extend the use of color theme to source code, see \"Emacs settings > Emacs display > Color theme for source code\" option."
  :tag "Menu color theme"
  :type '(choice (const dark) (const light))
  :set '(lambda (param value) (set-default param value) (eide-i-config-apply-color-theme) (eide-i-config-apply-extended-color-theme) (eide-i-config-update-menu-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-display)
(defcustom eide-custom-menu-window-position 'right "Menu window position."
  :tag "Menu window position"
  :type '(choice (const left) (const right))
  :group 'eide-display)
(defcustom eide-custom-menu-window-height 'half "Menu window height."
  :tag "Menu window height"
  :type '(choice (const half) (const full))
  :group 'eide-display)
(defcustom eide-custom-menu-use-specific-background-color t "Use a specific background color (depending on color theme) in menu."
  :tag "Use a specific background color in menu"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :set '(lambda (param value) (set-default param value) (eide-i-config-update-menu-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-display)
(defcustom eide-custom-menu-insert-blank-line-between-directories nil "Insert a blank line between directories in menu."
  :tag "Insert a blank line between directories in menu"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :set 'eide-i-config-update-menu
  :initialize 'custom-initialize-default
  :group 'eide-display)

(defgroup eide-version-control nil "Version control facilities in menu."
  :tag "Version control"
  :group 'eide)
(defcustom eide-custom-show-svn-status 'auto "Show svn status of files in menu."
  :tag "Show svn status"
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (const :tag "If root directory contains .svn directory" auto))
  :set 'eide-i-config-set-show-svn-status
  :initialize 'custom-initialize-default
  :group 'eide-version-control)
(defcustom eide-custom-show-git-status 'auto "Show git status of files in menu."
  :tag "Show git status"
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (const :tag "If root directory contains .git directory" auto))
  :set 'eide-i-config-set-show-git-status
  :initialize 'custom-initialize-default
  :group 'eide-version-control)
(defcustom eide-custom-vc-diff-command "" "Version control diff command (svn diff --diff-cmd=<command>, git difftool -y --extcmd=<command>). Use default (svn diff, git diff) if empty."
  :tag "Version control diff command"
  :type 'string
  :set 'eide-i-config-set-vc-diff-command
  :initialize 'custom-initialize-default
  :group 'eide-version-control)

(defgroup eide-project nil "Projects management and default commands that are set in project configuration when a project is created."
  :tag "Projects"
  :group 'eide)
(defcustom eide-custom-number-of-workspaces 2 "Number of workspaces (each workspace has got its own list of projects)."
  :tag "Number of workspaces"
  :type '(choice (const 1) (const 2) (const 3) (const 4) (const 5) (const 6) (const 7) (const 8))
  :set 'eide-i-config-set-number-of-workspaces
  :initialize 'custom-initialize-default
  :group 'eide-project)
(defcustom eide-custom-project-default-init-command "" "This command is called before all 'compile' and 'run' commands."
  :tag "Default init command"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-command-1 "" "Default compile command (1)."
  :tag "Default compile command (1)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-command-2 "" "Default compile command (2)."
  :tag "Default compile command (2)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-command-3 "" "Default compile command (3)."
  :tag "Default compile command (3)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-command-4 "" "Default compile command (4)."
  :tag "Default compile command (4)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-run-command-1 "" "Default run command (1)."
  :tag "Default run command (1)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-run-command-2 "" "Default run command (2)."
  :tag "Default run command (2)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-debug-command "" "Default debug command."
  :tag "Default debug command"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-debug-program-1 "" "Default debug program (1)."
  :tag "Default debug program (1)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-debug-program-2 "" "Default debug program (2)."
  :tag "Default debug program (2)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)

(defgroup eide-emacs-settings nil "Options that are not specific to Emacs-IDE, but can be set to override some default settings of Emacs, and provide a more user-friendly interface (requires 'Override Emacs settings' to be enabled)."
  :tag "Emacs settings"
  :group 'eide)

(defgroup eide-emacs-settings-display nil "Emacs display."
  :tag "Emacs display"
  :group 'eide-emacs-settings)
(defcustom eide-custom-start-maximized t "Start with maximized frame."
  :tag "Start with maximized frame"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-show-menu-bar nil "Show menu bar."
  :tag "Show menu bar"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Don't override" ignore))
  :set 'eide-i-config-set-menu-bar
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-show-tool-bar nil "Show tool bar."
  :tag "Show tool bar"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Don't override" ignore))
  :set 'eide-i-config-set-tool-bar
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-scroll-bar-position 'right "Scroll bar position."
  :tag "Scroll bar position"
  :type '(choice (const :tag "No scroll bar" nil)
                 (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Don't override" ignore))
  :set 'eide-i-config-set-scroll-bar-position
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-font-height 105 "Font height (an integer in units of 1/10 point)."
  :tag "Font height"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "An integer in units of 1/10 point"))
  :set 'eide-i-config-set-font-height
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-extend-color-theme-to-source-code t "Extend the use of color theme to source code. See \"Display > Menu color theme\" option."
  :tag "Color theme for source code"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :set '(lambda (param value) (set-default param value) (eide-i-config-apply-extended-color-theme) (eide-i-config-update-menu-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-show-trailing-spaces 'ignore "Show trailing spaces."
  :tag "Show trailing spaces"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Don't override buffer-local show-trailing-whitespace variable" ignore))
  :set '(lambda (param value) (set-default param value))
  :group 'eide-emacs-settings-display)

(defgroup eide-emacs-settings-coding-rules nil "Indentation for some languages."
  :tag "Coding rules"
  :group 'eide-emacs-settings)
(defcustom eide-custom-indent-mode 'spaces "Indentation mode (spaces or tabs)."
  :tag "Indentation mode"
  :type '(choice (const :tag "Spaces" spaces)
                 (const :tag "Tabs" tabs)
                 (const :tag "Don't override indent-tabs-mode variable" ignore))
  :set 'eide-i-config-set-indent-mode
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-coding-rules)
(defcustom eide-custom-default-tab-width 4 "Default tab width. For languages that are listed below, tab width is indentation offset."
  :tag "Default tab width"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "Number of spaces"))
  :set 'eide-i-config-set-default-tab-width
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-coding-rules)
(defcustom eide-custom-c-indent-offset 2 "Indentation offset for C/C++."
  :tag "Indentation offset for C/C++"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "Number of spaces"))
  :set '(lambda (param value) (set-default param value))
  :group 'eide-emacs-settings-coding-rules)
(defcustom eide-custom-sh-indent-offset 2 "Indentation offset for shell scripts."
  :tag "Indentation offset for shell scripts"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "Number of spaces"))
  :set '(lambda (param value) (set-default param value))
  :group 'eide-emacs-settings-coding-rules)
(defcustom eide-custom-lisp-indent-offset 2 "Indentation offset for Emacs Lisp."
  :tag "Indentation offset for Emacs Lisp"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "Number of spaces"))
  :set '(lambda (param value) (set-default param value))
  :group 'eide-emacs-settings-coding-rules)
(defcustom eide-custom-perl-indent-offset 2 "Indentation offset for Perl."
  :tag "Indentation offset for Perl"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "Number of spaces"))
  :set '(lambda (param value) (set-default param value))
  :group 'eide-emacs-settings-coding-rules)
(defcustom eide-custom-python-indent-offset 4 "Indentation offset for Python."
  :tag "Indentation offset for Python"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "Number of spaces"))
  :set '(lambda (param value) (set-default param value))
  :group 'eide-emacs-settings-coding-rules)
(defcustom eide-custom-sgml-indent-offset 2 "Indentation offset for SGML (HTML, XML...)."
  :tag "Indentation offset for SGML"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "Number of spaces"))
  :set '(lambda (param value) (set-default param value))
  :group 'eide-emacs-settings-coding-rules)

(defgroup eide-emacs-settings-dark-colors nil "Source code colors for dark color theme."
  :tag "Source code colors for dark color theme"
  :group 'eide-emacs-settings)
(defcustom eide-custom-dark-background "black" "Background color."
  :tag "Background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-background param value 'dark) (eide-i-config-update-menu-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-foreground "gray90" "Foreground color."
  :tag "Foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-foreground param value 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-keyword-foreground "salmon" "Keyword foreground color."
  :tag "Keyword foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-keyword-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-type-foreground "medium sea green" "Type foreground color."
  :tag "Type foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-type-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-function-foreground "orange" "Function foreground color."
  :tag "Function foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-function-name-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-variable-foreground "dark orange" "Variable foreground color."
  :tag "Variable foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-variable-name-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-constant-background "maroon4" "Constant background color."
  :tag "Constant background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-background param value 'font-lock-constant-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-constant-foreground "misty rose" "Constant foreground color."
  :tag "Constant foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-constant-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-builtin-background "brown" "Builtin background color."
  :tag "Builtin background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-background param value 'font-lock-builtin-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-builtin-foreground "yellow" "Builtin foreground color."
  :tag "Builtin foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-builtin-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-string-background "gray15" "String background color."
  :tag "String background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-background param value 'font-lock-string-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-comment-foreground "deep sky blue" "Comment foreground color."
  :tag "Comment foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-comment-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-selection-background "gray50" "Selection background color."
  :tag "Selection background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-background param value 'region 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)

(defgroup eide-emacs-settings-light-colors nil "Source code colors for light color theme."
  :tag "Source code colors for light color theme"
  :group 'eide-emacs-settings)
(defcustom eide-custom-light-background "old lace" "Background color."
  :tag "Background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-background param value 'light) (eide-i-config-update-menu-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-foreground "black" "Foreground color."
  :tag "Foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-foreground param value 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-keyword-foreground "brown" "Keyword foreground color."
  :tag "Keyword foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-keyword-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-type-foreground "sea green" "Type foreground color."
  :tag "Type foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-type-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-function-foreground "red" "Function foreground color."
  :tag "Function foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-function-name-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-variable-foreground "orange red" "Variable foreground color."
  :tag "Variable foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-variable-name-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-constant-background "misty rose" "Constant background color."
  :tag "Constant background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-background param value 'font-lock-constant-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-constant-foreground "deep pink" "Constant foreground color."
  :tag "Constant foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-constant-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-builtin-background "yellow" "Builtin background color."
  :tag "Builtin background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-background param value 'font-lock-builtin-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-builtin-foreground "red" "Builtin foreground color."
  :tag "Builtin foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-builtin-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-string-background "white" "String background color."
  :tag "String background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-background param value 'font-lock-string-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-comment-foreground "light slate blue" "Comment foreground color."
  :tag "Comment foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-foreground param value 'font-lock-comment-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-selection-background "bisque" "Selection background color."
  :tag "Selection background color"
  :type 'color
  :set '(lambda (param value) (eide-i-config-set-face-background param value 'region 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)

(defgroup eide-search nil "Cscope option."
  :tag "Search"
  :group 'eide-emacs-settings)
(defcustom eide-custom-update-cscope-database 'auto "Update of cscope database. Update is necessary when the code has changed. You can update on every search (cscope default behaviour), only on user request, or automatically when a buffer has been edited or refreshed."
  :tag "Update of cscope database"
  :type '(choice (const :tag "Don't override" ignore)
                 (const :tag "Always (on every search)" t)
                 (const :tag "Never (only on user request)" nil)
                 (const :tag "When a buffer has been edited or refreshed" auto))
  :set 'eide-i-config-set-cscope-update
  :initialize 'custom-initialize-default
  :group 'eide-search)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-config-apply-color-theme ()
  "Apply color theme (for menu)."
  (if eide-config-ready
    (progn
      (if (equal eide-custom-color-theme 'dark)
        ;; "Dark" color theme
        (progn
          ;; Menu
          (setq eide-config-menu-background-color "black")
          (setq eide-config-menu-foreground-color "gray95")
          (set-face-foreground 'eide-config-menu-project-header-face "deep sky blue")
          (set-face-foreground 'eide-config-menu-project-name-face "orange")
          ;; Menu: directories
          (set-face-background 'eide-config-menu-directory-face "#300030")
          (set-face-foreground 'eide-config-menu-directory-face "thistle")
          (set-face-background 'eide-config-menu-directory-out-of-project-face "saddle brown")
          (set-face-foreground 'eide-config-menu-directory-out-of-project-face "peach puff")
          ;; Menu: files
          (set-face-foreground 'eide-config-menu-file-rw-face "gray95")
          (set-face-foreground 'eide-config-menu-file-ro-face "gray65")
          (set-face-foreground 'eide-config-menu-file-nofile-face "gray95")
          (setq eide-config-menu-file-highlight-background-color "dark red")
          (set-face-foreground 'eide-config-menu-file-vc-modified-face "deep sky blue")
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
          (set-face-foreground 'eide-config-config-parameter-face "salmon")
          (set-face-foreground 'eide-config-config-possibilities-face "medium sea green")
          (set-face-foreground 'eide-config-config-separator-face "orange red")
          (set-face-background 'eide-config-config-value-face "gray30")
          (set-face-foreground 'eide-config-config-value-face "white")
          ;; Projects list
          (set-face-foreground 'eide-config-project-name-face "sandy brown")
          (set-face-background 'eide-config-project-current-name-face "dark red")
          (set-face-foreground 'eide-config-project-current-name-face "sandy brown")
          (set-face-background 'eide-config-project-comparison-name-face "dark green")
          (set-face-foreground 'eide-config-project-comparison-name-face "sandy brown")
          ;; Information line
          (set-face-background 'mode-line "gray"))
        ;; "Light" color theme
        (progn
          ;; Menu
          (setq eide-config-menu-background-color "white")
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
          (set-face-foreground 'eide-config-menu-file-vc-modified-face "blue")
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
          (set-face-foreground 'eide-config-config-parameter-face "brown")
          (set-face-foreground 'eide-config-config-possibilities-face "sea green")
          (set-face-foreground 'eide-config-config-separator-face "red")
          (set-face-background 'eide-config-config-value-face "white")
          (set-face-foreground 'eide-config-config-value-face "black")
          ;; Projects list
          (set-face-foreground 'eide-config-project-name-face "red")
          (set-face-background 'eide-config-project-current-name-face "yellow")
          (set-face-foreground 'eide-config-project-current-name-face "red")
          (set-face-background 'eide-config-project-comparison-name-face "light green")
          (set-face-foreground 'eide-config-project-comparison-name-face "red")
          ;; Information line
          (set-face-background 'mode-line "wheat"))))))

(defun eide-i-config-apply-extended-color-theme ()
  "Apply color theme (for source code)."
  (if eide-config-ready
    (progn
      (if (and eide-custom-override-emacs-settings
               eide-custom-extend-color-theme-to-source-code)
        (progn
          (if (equal eide-custom-color-theme 'dark)
            ;; "Dark" color theme
            (progn
              (setq eide-config-background-color eide-custom-dark-background)
              (setq eide-config-foreground-color eide-custom-dark-foreground)
              (set-background-color eide-custom-dark-background)
              (set-face-background 'fringe eide-custom-dark-background)
              (set-foreground-color eide-custom-dark-foreground)
              (set-face-foreground 'font-lock-string-face eide-custom-dark-foreground)
              (set-face-foreground 'region eide-custom-dark-foreground)
              (set-face-foreground 'font-lock-keyword-face eide-custom-dark-keyword-foreground)
              (set-face-foreground 'font-lock-type-face eide-custom-dark-type-foreground)
              (set-face-foreground 'font-lock-function-name-face eide-custom-dark-function-foreground)
              (set-face-foreground 'font-lock-variable-name-face eide-custom-dark-variable-foreground)
              (set-face-background 'font-lock-constant-face eide-custom-dark-constant-background)
              (set-face-foreground 'font-lock-constant-face eide-custom-dark-constant-foreground)
              (set-face-background 'font-lock-builtin-face eide-custom-dark-builtin-background)
              (set-face-foreground 'font-lock-builtin-face eide-custom-dark-builtin-foreground)
              (set-face-background 'font-lock-string-face eide-custom-dark-string-background)
              (set-face-foreground 'font-lock-comment-face eide-custom-dark-comment-foreground)
              (set-face-background 'region eide-custom-dark-selection-background))
            ;; "Light" color theme
            (progn
              (setq eide-config-background-color eide-custom-light-background)
              (setq eide-config-foreground-color eide-custom-light-foreground)
              (set-background-color eide-custom-light-background)
              (set-face-background 'fringe eide-custom-light-background)
              (set-foreground-color eide-custom-light-foreground)
              (set-face-foreground 'font-lock-string-face eide-custom-light-foreground)
              (set-face-foreground 'region eide-custom-light-foreground)
              (set-face-foreground 'font-lock-keyword-face eide-custom-light-keyword-foreground)
              (set-face-foreground 'font-lock-type-face eide-custom-light-type-foreground)
              (set-face-foreground 'font-lock-function-name-face eide-custom-light-function-foreground)
              (set-face-foreground 'font-lock-variable-name-face eide-custom-light-variable-foreground)
              (set-face-background 'font-lock-constant-face eide-custom-light-constant-background)
              (set-face-foreground 'font-lock-constant-face eide-custom-light-constant-foreground)
              (set-face-background 'font-lock-builtin-face eide-custom-light-builtin-background)
              (set-face-foreground 'font-lock-builtin-face eide-custom-light-builtin-foreground)
              (set-face-background 'font-lock-string-face eide-custom-light-string-background)
              (set-face-foreground 'font-lock-comment-face eide-custom-light-comment-foreground)
              (set-face-background 'region eide-custom-light-selection-background))))
        (progn
          ;; Restore user colors
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
          (set-face-foreground 'region eide-config-user-selection-foreground-color))))))

(defun eide-i-config-update-menu-background-color ()
  "Update menu background color."
  (if eide-config-ready
    (progn
      (let ((l-menu-background-color nil))
        (let ((l-background-color nil))
          (if (and eide-custom-override-emacs-settings
                   eide-custom-extend-color-theme-to-source-code)
            (if (equal eide-custom-color-theme 'dark)
              (setq l-background-color eide-custom-dark-background)
              (setq l-background-color eide-custom-light-background))
            (setq l-background-color (face-background 'default)))
          (if (or (not eide-custom-menu-use-specific-background-color)
                  (equal eide-config-menu-background-color l-background-color))
            (progn
              (setq eide-config-menu-use-specific-background-color nil)
              (setq l-menu-background-color l-background-color))
            (progn
              (setq eide-config-menu-use-specific-background-color t)
              (setq l-menu-background-color eide-config-menu-background-color))))

        (set-face-background 'eide-config-menu-default-face l-menu-background-color)
        (set-face-foreground 'eide-config-menu-default-face eide-config-menu-foreground-color)
        (set-face-background 'eide-config-menu-project-header-face l-menu-background-color)
        (set-face-background 'eide-config-menu-project-name-face l-menu-background-color)
        (set-face-background 'eide-config-menu-file-rw-face l-menu-background-color)
        (set-face-background 'eide-config-menu-file-ro-face l-menu-background-color)
        (set-face-background 'eide-config-menu-file-nofile-face l-menu-background-color)
        (set-face-background 'eide-config-menu-file-ref-face l-menu-background-color)
        (set-face-background 'eide-config-menu-file-new-face l-menu-background-color)
        (set-face-background 'eide-config-menu-file-vc-modified-face l-menu-background-color)

        ;; Menu: current file
        (copy-face 'eide-config-menu-file-rw-face 'eide-config-menu-current-file-rw-face)
        (copy-face 'eide-config-menu-file-ro-face 'eide-config-menu-current-file-ro-face)
        (copy-face 'eide-config-menu-file-nofile-face 'eide-config-menu-current-file-nofile-face)
        (copy-face 'eide-config-menu-file-ref-face 'eide-config-menu-current-file-ref-face)
        (copy-face 'eide-config-menu-file-new-face 'eide-config-menu-current-file-new-face)
        (copy-face 'eide-config-menu-file-vc-modified-face 'eide-config-menu-current-file-vc-modified-face)
        (set-face-background 'eide-config-menu-current-file-rw-face eide-config-menu-file-highlight-background-color)
        (set-face-background 'eide-config-menu-current-file-ro-face eide-config-menu-file-highlight-background-color)
        (set-face-background 'eide-config-menu-current-file-nofile-face eide-config-menu-file-highlight-background-color)
        (set-face-background 'eide-config-menu-current-file-ref-face eide-config-menu-file-highlight-background-color)
        (set-face-background 'eide-config-menu-current-file-new-face eide-config-menu-file-highlight-background-color)
        (set-face-background 'eide-config-menu-current-file-vc-modified-face eide-config-menu-file-highlight-background-color)

        (set-face-background 'eide-config-menu-function-face l-menu-background-color)
        (set-face-background 'eide-config-menu-empty-list-face l-menu-background-color)
        (set-face-foreground 'eide-config-menu-empty-list-face eide-config-menu-foreground-color))
      (eide-menu-update t))))

(defun eide-i-config-update-menu (param value)
  "Update menu.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (eide-menu-update t)))

(defun eide-i-config-set-show-svn-status (param value)
  "Set show svn status (eide-config-show-svn-status-flag).
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (eide-vc-update-show-svn-status)))

(defun eide-i-config-set-show-git-status (param value)
  "Set show git status (eide-config-show-git-status-flag).
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (eide-vc-update-show-git-status)))

(defun eide-i-config-set-vc-diff-command (param value)
  "Set vc diff command.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (eide-vc-set-diff-command value)))

(defun eide-i-config-set-number-of-workspaces (param value)
  "Set number of workspaces.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (eide-project-create-workspaces)))

(defun eide-i-config-set-menu-bar (param value)
  "Set menu bar mode.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if window-system
      (if (and eide-custom-override-emacs-settings
               (not (equal value 'ignore)))
        (if value
          (menu-bar-mode 1)
          (menu-bar-mode -1))
        (menu-bar-mode (if eide-config-user-menu-bar-mode 1 -1))))))

(defun eide-i-config-set-tool-bar (param value)
  "Set tool bar mode.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if window-system
      (if (and eide-custom-override-emacs-settings
               (not (equal value 'ignore)))
        (if value
          (tool-bar-mode 1)
          (tool-bar-mode -1))
        (tool-bar-mode (if eide-config-user-tool-bar-mode 1 -1))))))

(defun eide-i-config-set-scroll-bar-position (param value)
  "Set scroll bar position
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if window-system
      (if (and eide-custom-override-emacs-settings
               (not (equal value 'ignore)))
        (set-scroll-bar-mode value)
        (set-scroll-bar-mode eide-config-user-scroll-bar-mode)))))

(defun eide-i-config-set-font-height (param value)
  "Set font height.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if window-system
      (if (and eide-custom-override-emacs-settings value)
        (set-face-attribute 'default nil :height value)
        (set-face-attribute 'default nil :height eide-config-user-font-height)))))

(defun eide-i-config-set-indent-mode (param value)
  "Set indentation mode (spaces or tabs).
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings
             (not (equal value 'ignore)))
      (setq-default indent-tabs-mode (if (equal value 'spaces) nil t))
      (setq-default indent-tabs-mode eide-config-user-indent-tabs-mode))))

(defun eide-i-config-set-default-tab-width (param value)
  "Set default tab width.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings value)
      (setq-default tab-width value)
      (setq-default tab-width eide-config-user-tab-width))))

(defun eide-i-config-set-background (param value color-theme)
  "Set background color for a color theme.
Arguments:
- param: customization parameter.
- value: customization value.
- color-theme: color theme."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings
             eide-custom-extend-color-theme-to-source-code
             (equal eide-custom-color-theme color-theme))
      (progn
        (setq eide-config-background-color value)
        (set-background-color value)
        (set-face-background 'fringe value)))))

(defun eide-i-config-set-foreground (param value color-theme)
  "Set foreground color for a color theme.
Arguments:
- param: customization parameter.
- value: customization value.
- color-theme: color theme."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings
             eide-custom-extend-color-theme-to-source-code
             (equal eide-custom-color-theme color-theme))
      (progn
        (setq eide-config-foreground-color value)
        (set-foreground-color value)
        (set-face-foreground 'font-lock-string-face value)
        (set-face-foreground 'region value)))))

(defun eide-i-config-set-face-background (param value face color-theme)
  "Set background color of a face for a color theme.
Arguments:
- param: customization parameter.
- value: customization value.
- face: face.
- color-theme: color theme."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings
             eide-custom-extend-color-theme-to-source-code
             (equal eide-custom-color-theme color-theme))
      (set-face-background face value))))

(defun eide-i-config-set-face-foreground (param value face color-theme)
  "Set foreground color of a face for a color theme.
Arguments:
- param: customization parameter.
- value: customization value.
- face: face.
- color-theme: color theme."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings
             eide-custom-extend-color-theme-to-source-code
             (equal eide-custom-color-theme color-theme))
      (set-face-foreground face value))))

(defun eide-i-config-set-cscope-update (param value)
  "Set cscope update.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings
             (not (equal value 'ignore)))
      (if (equal value 'auto)
        ;; In "auto" mode, update database for the first search
        (setq eide-search-cscope-update-database-request-pending-flag t))
      (setq cscope-do-not-update-database eide-config-user-cscope-do-not-update-database))))

(defun eide-i-config-apply-emacs-settings ()
  "Apply \"Emacs settings\" options."
  (if eide-config-ready
    (progn
      (eide-i-config-apply-extended-color-theme)
      (eide-i-config-update-menu-background-color)
      (eide-i-config-set-show-svn-status 'eide-custom-show-svn-status eide-custom-show-svn-status)
      (eide-i-config-set-show-git-status 'eide-custom-show-git-status eide-custom-show-git-status)
      (eide-i-config-set-vc-diff-command 'eide-custom-vc-diff-command eide-custom-vc-diff-command)
      (eide-i-config-set-menu-bar 'eide-custom-show-menu-bar eide-custom-show-menu-bar)
      (eide-i-config-set-tool-bar 'eide-custom-show-tool-bar eide-custom-show-tool-bar)
      (eide-i-config-set-scroll-bar-position 'eide-custom-scroll-bar-position eide-custom-scroll-bar-position)
      (eide-i-config-set-font-height 'eide-custom-font-height eide-custom-font-height)
      (eide-i-config-set-indent-mode 'eide-custom-indent-mode eide-custom-indent-mode)
      (eide-i-config-set-default-tab-width 'eide-custom-default-tab-width eide-custom-default-tab-width)
      (if eide-option-use-cscope-flag
        (eide-i-config-set-cscope-update 'eide-custom-update-cscope-database eide-custom-update-cscope-database)))))

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-config-save-emacs-settings ()
  "Save emacs settings."
  (setq eide-config-user-menu-bar-mode menu-bar-mode)
  (setq eide-config-user-tool-bar-mode tool-bar-mode)
  (setq eide-config-user-scroll-bar-mode scroll-bar-mode)
  (setq eide-config-user-font-height (face-attribute 'default :height))
  (setq eide-config-user-indent-tabs-mode indent-tabs-mode)
  (setq eide-config-user-tab-width tab-width)
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
  (setq eide-config-user-selection-foreground-color (face-foreground 'region))
  (if eide-option-use-cscope-flag
    (setq eide-config-user-cscope-do-not-update-database cscope-do-not-update-database)))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-config-open-customization ()
  "Display customization (full frame)."
  (eide-windows-layout-unbuild)
  (eide-keys-configure-for-special-buffer)
  (customize-group 'eide))

(defun eide-config-init ()
  "Initialize config."
  (eide-i-config-save-emacs-settings))

(defun eide-config-apply ()
  "Apply config."
  ;; Custom values are initialized (and set if customized) by
  ;; custom-set-variables in ~/.emacs, which may be done before or after
  ;; eide-start call.
  ;; There are dependencies between parameters: we cannot set them until they
  ;; have all been defined.
  ;; Moreover, in order to avoid to set different values successively, values
  ;; are not set until eide-config-ready is set (below).
  (setq eide-config-ready t)
  (eide-project-create-workspaces)
  (eide-i-config-apply-color-theme)
  (eide-i-config-apply-emacs-settings))

;;; eide-config.el ends here
