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

(require 'eide-display)
(require 'eide-menu)
(require 'eide-vc)

(defvar eide-config-ready nil)

(defvar eide-config-user-indent-tabs-mode nil)
(defvar eide-config-user-tab-width nil)

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
;; SYNTAX HIGHLIGHTING
;; ----------------------------------------------------------------------------

(require 'font-lock)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Code
(make-face-bold 'font-lock-keyword-face)
(make-face-bold 'font-lock-function-name-face)

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

(defgroup eide-display nil "Display."
  :tag "Display"
  :group 'eide)

(defgroup eide-windows nil "Windows layout."
  :tag "Windows layout"
  :group 'eide)
(defcustom eide-custom-menu-window-position 'right "Menu window position."
  :tag "Menu window position"
  :type '(choice (const left) (const right))
  :group 'eide-windows)
(defcustom eide-custom-menu-window-height 'half "Menu window height."
  :tag "Menu window height"
  :type '(choice (const half) (const full))
  :group 'eide-windows)

(defgroup eide-menu nil "Menu colors and display."
  :tag "Menu colors and display"
  :group 'eide)

(defgroup eide-version-control nil "Version control facilities in menu."
  :tag "Version control"
  :group 'eide)

(defgroup eide-project nil "Projects management and default commands that are set in project configuration when a project is created."
  :tag "Projects"
  :group 'eide)

(defgroup eide-emacs-settings nil "Options that are not specific to Emacs-IDE, but can be set to override some default settings of Emacs, and provide a more user-friendly interface (requires 'Override Emacs settings' to be enabled)."
  :tag "Emacs settings"
  :group 'eide)

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

(defgroup eide-search nil "Cscope option."
  :tag "Search"
  :group 'eide-emacs-settings)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-config-update-menu (param value)
  "Update menu.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (eide-menu-update t)))

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

(defun eide-i-config-apply-emacs-settings ()
  "Apply \"Emacs settings\" options."
  (if eide-config-ready
    (progn
      (eide-display-apply-extended-color-theme)
      (eide-menu-update-background-color)
      (eide-display-apply-emacs-settings)
      (eide-i-config-set-indent-mode 'eide-custom-indent-mode eide-custom-indent-mode)
      (eide-i-config-set-default-tab-width 'eide-custom-default-tab-width eide-custom-default-tab-width)
      (eide-search-apply-customization))))

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-config-save-emacs-settings ()
  "Save Emacs settings."
  (setq eide-config-user-indent-tabs-mode indent-tabs-mode)
  (setq eide-config-user-tab-width tab-width)
  (eide-display-save-emacs-settings)
  (eide-search-save-emacs-settings))

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
  (eide-display-apply-color-theme)
  (eide-i-config-apply-emacs-settings)
  (eide-vc-apply-customization))

;;; eide-config.el ends here
