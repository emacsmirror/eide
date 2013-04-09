;;; eide-coding.el --- Emacs-IDE, coding

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

(provide 'eide-coding)

(require 'eide-config)
(require 'eide-display)
(require 'eide-menu)
(require 'eide-vc)

(defvar eide-coding-user-indent-tabs-mode nil)
(defvar eide-coding-user-tab-width nil)

;; ----------------------------------------------------------------------------
;; OPTIONS
;; ----------------------------------------------------------------------------

;; Exclude "_" from word delimiters (when selecting by double-click)
(defvar eide-option-select-whole-symbol-flag t)

;; ----------------------------------------------------------------------------
;; SYNTAX HIGHLIGHTING
;; ----------------------------------------------------------------------------

(require 'font-lock)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Code
(make-face-bold 'font-lock-keyword-face)
(make-face-bold 'font-lock-function-name-face)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION VARIABLES
;; ----------------------------------------------------------------------------

(defcustom eide-custom-indent-mode 'spaces "Indentation mode (spaces or tabs)."
  :tag "Indentation mode"
  :type '(choice (const :tag "Spaces" spaces)
                 (const :tag "Tabs" tabs)
                 (const :tag "Don't override indent-tabs-mode variable" ignore))
  :set 'eide-i-coding-set-indent-mode
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-coding-rules)
(defcustom eide-custom-default-tab-width 4 "Default tab width. For languages that are listed below, tab width is indentation offset."
  :tag "Default tab width"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "Number of spaces"))
  :set 'eide-i-coding-set-default-tab-width
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

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-coding-set-indent-mode (param value)
  "Set indentation mode (spaces or tabs).
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings
             (not (equal value 'ignore)))
      (setq-default indent-tabs-mode (if (equal value 'spaces) nil t))
      (setq-default indent-tabs-mode eide-coding-user-indent-tabs-mode))))

(defun eide-i-coding-set-default-tab-width (param value)
  "Set default tab width.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if (and eide-custom-override-emacs-settings value)
      (setq-default tab-width value)
      (setq-default tab-width eide-coding-user-tab-width))))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-coding-init ()
  "Add hooks for major modes."
  ;; C major mode
  (add-hook
   'c-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" c-mode-syntax-table))

      ;; Indentation
      (c-set-style "K&R") ; Indentation style
      (if (and eide-custom-override-emacs-settings eide-custom-c-indent-offset)
        (progn
          (setq tab-width eide-custom-c-indent-offset)
          (setq c-basic-offset eide-custom-c-indent-offset)))
      (c-set-offset 'case-label '+) ; Case/default in a switch (default value: 0)

      ;; Turn hide/show mode on
      (if (not hs-minor-mode)
        (hs-minor-mode))
      ;; Do not hide comments when hidding all
      (setq hs-hide-comments-when-hiding-all nil)

      ;; Turn ifdef mode on (does not work very well with ^M turned into empty lines)
      (hide-ifdef-mode 1)

      ;; Pour savoir si du texte est sélectionné ou non
      (setq mark-even-if-inactive nil)))

  ;; C++ major mode
  (add-hook
   'c++-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" c-mode-syntax-table))

      ;; Indentation
      (c-set-style "K&R") ; Indentation style
      (if (and eide-custom-override-emacs-settings eide-custom-c-indent-offset)
        (progn
          (setq tab-width eide-custom-c-indent-offset)
          (setq c-basic-offset eide-custom-c-indent-offset)))
      (c-set-offset 'case-label '+) ; Case/default in a switch (default value: 0)

      ;; Turn hide/show mode on
      (if (not hs-minor-mode)
        (hs-minor-mode))
      ;; Do not hide comments when hidding all
      (setq hs-hide-comments-when-hiding-all nil)

      ;; Turn ifdef mode on (does not work very well with ^M turned into empty lines)
      (hide-ifdef-mode 1)

      ;; Pour savoir si du texte est sélectionné ou non
      (setq mark-even-if-inactive nil)))

  ;; Shell Script major mode

  ;; Enable colors
  ;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  ;; Shell color mode is disabled because it disturbs shell-command (run
  ;; command), and I have no solution for that!...
  ;; - ansi-term: Does not work correctly ("error in process filter").
  ;; - eshell: Uses specific aliases.
  ;; - ansi-color-for-comint-mode-on: Does not apply to shell-command and
  ;;   disturb it ("Marker does not point anywhere"). Moreover, it is not
  ;;   buffer local (this would partly solve the issue).
  ;; - Using shell for shell-command: previous run command is not killed, even
  ;;   if process and buffer are killed.

  (add-hook
   'sh-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" sh-mode-syntax-table))
      ;; Indentation
      (if (and eide-custom-override-emacs-settings eide-custom-sh-indent-offset)
        (progn
          (setq tab-width eide-custom-sh-indent-offset)
          (setq sh-basic-offset eide-custom-sh-indent-offset)))))

  ;; Emacs Lisp major mode
  (add-hook
   'emacs-lisp-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "-" should not be a word delimiter
        (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))

      ;; Indentation
      (if (and eide-custom-override-emacs-settings eide-custom-lisp-indent-offset)
        (progn
          (setq tab-width eide-custom-lisp-indent-offset)
          (setq lisp-body-indent eide-custom-lisp-indent-offset)
          ;; Indentation after "if" (with default behaviour, the "then" statement is
          ;; more indented than the "else" statement)
          (put 'if 'lisp-indent-function 1)))))

  ;; Perl major mode
  (add-hook
   'perl-mode-hook
   '(lambda()
      ;; Indentation
      (if (and eide-custom-override-emacs-settings eide-custom-perl-indent-offset)
        (progn
          (setq tab-width eide-custom-perl-indent-offset)
          (setq perl-indent-level eide-custom-perl-indent-offset)))))

  ;; Python major mode
  (add-hook
   'python-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" python-mode-syntax-table))

      ;; Indentation
      (if (and eide-custom-override-emacs-settings eide-custom-python-indent-offset)
        (progn
          (setq tab-width eide-custom-python-indent-offset)
          (setq python-indent eide-custom-python-indent-offset)))))

  ;; SGML (HTML, XML...) major mode
  (add-hook
   'sgml-mode-hook
   '(lambda()
      ;; Indentation
      (if (and eide-custom-override-emacs-settings eide-custom-sgml-indent-offset)
        (progn
          (setq tab-width eide-custom-sgml-indent-offset)
          (setq sgml-basic-offset eide-custom-sgml-indent-offset))))))

(defun eide-coding-save-emacs-settings ()
  "Save Emacs settings (for coding)."
  (setq eide-coding-user-indent-tabs-mode indent-tabs-mode)
  (setq eide-coding-user-tab-width tab-width))

(defun eide-coding-apply-emacs-settings ()
  "Apply Emacs settings (for coding)."
  (eide-i-coding-set-indent-mode 'eide-custom-indent-mode eide-custom-indent-mode)
  (eide-i-coding-set-default-tab-width 'eide-custom-default-tab-width eide-custom-default-tab-width))

;;; eide-coding.el ends here
