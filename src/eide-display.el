;;; eide-display.el --- Emacs-IDE, display

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

(provide 'eide-display)

(require 'eide-help)
(require 'eide-menu)
(require 'eide-project)

(defvar eide-display-background-color nil)
(defvar eide-display-foreground-color nil)

(defvar eide-display-user-menu-bar-mode nil)
(defvar eide-display-user-tool-bar-mode nil)
(defvar eide-display-user-scroll-bar-mode nil)
(defvar eide-display-user-font-height nil)
(defvar eide-display-user-background-color nil)
(defvar eide-display-user-foreground-color nil)
(defvar eide-display-user-keyword-foreground-color nil)
(defvar eide-display-user-type-foreground-color nil)
(defvar eide-display-user-function-foreground-color nil)
(defvar eide-display-user-variable-foreground-color nil)
(defvar eide-display-user-constant-background-color nil)
(defvar eide-display-user-constant-foreground-color nil)
(defvar eide-display-user-builtin-background-color nil)
(defvar eide-display-user-builtin-foreground-color nil)
(defvar eide-display-user-string-background-color nil)
(defvar eide-display-user-string-foreground-color nil)
(defvar eide-display-user-comment-foreground-color nil)
(defvar eide-display-user-selection-background-color nil)
(defvar eide-display-user-selection-foreground-color nil)

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

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION VARIABLES
;; ----------------------------------------------------------------------------

(defcustom eide-custom-color-theme 'light "Color theme for menu. To extend the use of color theme to source code, see \"Emacs settings > Emacs display > Color theme for source code\" option."
  :tag "Menu color theme"
  :type '(choice (const dark) (const light))
  :set '(lambda (param value) (set-default param value) (eide-display-apply-color-theme) (eide-display-apply-extended-color-theme) (eide-menu-update-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-display)

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
  :set 'eide-i-display-set-menu-bar
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-show-tool-bar nil "Show tool bar."
  :tag "Show tool bar"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Don't override" ignore))
  :set 'eide-i-display-set-tool-bar
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-scroll-bar-position 'right "Scroll bar position."
  :tag "Scroll bar position"
  :type '(choice (const :tag "No scroll bar" nil)
                 (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Don't override" ignore))
  :set 'eide-i-display-set-scroll-bar-position
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-font-height 105 "Font height (an integer in units of 1/10 point)."
  :tag "Font height"
  :type '(choice (const :tag "Don't override" nil)
                 (integer :tag "An integer in units of 1/10 point"))
  :set 'eide-i-display-set-font-height
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-extend-color-theme-to-source-code t "Extend the use of color theme to source code. See \"Display > Menu color theme\" option."
  :tag "Color theme for source code"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :set '(lambda (param value) (set-default param value) (eide-display-apply-extended-color-theme) (eide-menu-update-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-display)
(defcustom eide-custom-show-trailing-spaces 'ignore "Show trailing spaces."
  :tag "Show trailing spaces"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Don't override buffer-local show-trailing-whitespace variable" ignore))
  :set '(lambda (param value) (set-default param value))
  :group 'eide-emacs-settings-display)

(defgroup eide-emacs-settings-dark-colors nil "Source code colors for dark color theme."
  :tag "Source code colors for dark color theme"
  :group 'eide-emacs-settings)
(defcustom eide-custom-dark-background "black" "Background color."
  :tag "Background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-background param value 'dark) (eide-menu-update-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-foreground "gray90" "Foreground color."
  :tag "Foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-foreground param value 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-keyword-foreground "salmon" "Keyword foreground color."
  :tag "Keyword foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-keyword-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-type-foreground "medium sea green" "Type foreground color."
  :tag "Type foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-type-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-function-foreground "orange" "Function foreground color."
  :tag "Function foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-function-name-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-variable-foreground "dark orange" "Variable foreground color."
  :tag "Variable foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-variable-name-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-constant-background "maroon4" "Constant background color."
  :tag "Constant background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-background param value 'font-lock-constant-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-constant-foreground "misty rose" "Constant foreground color."
  :tag "Constant foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-constant-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-builtin-background "brown" "Builtin background color."
  :tag "Builtin background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-background param value 'font-lock-builtin-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-builtin-foreground "yellow" "Builtin foreground color."
  :tag "Builtin foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-builtin-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-string-background "gray15" "String background color."
  :tag "String background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-background param value 'font-lock-string-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-comment-foreground "deep sky blue" "Comment foreground color."
  :tag "Comment foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-comment-face 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)
(defcustom eide-custom-dark-selection-background "gray50" "Selection background color."
  :tag "Selection background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-background param value 'region 'dark))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-dark-colors)

(defgroup eide-emacs-settings-light-colors nil "Source code colors for light color theme."
  :tag "Source code colors for light color theme"
  :group 'eide-emacs-settings)
(defcustom eide-custom-light-background "old lace" "Background color."
  :tag "Background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-background param value 'light) (eide-menu-update-background-color))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-foreground "black" "Foreground color."
  :tag "Foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-foreground param value 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-keyword-foreground "brown" "Keyword foreground color."
  :tag "Keyword foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-keyword-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-type-foreground "sea green" "Type foreground color."
  :tag "Type foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-type-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-function-foreground "red" "Function foreground color."
  :tag "Function foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-function-name-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-variable-foreground "orange red" "Variable foreground color."
  :tag "Variable foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-variable-name-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-constant-background "misty rose" "Constant background color."
  :tag "Constant background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-background param value 'font-lock-constant-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-constant-foreground "deep pink" "Constant foreground color."
  :tag "Constant foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-constant-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-builtin-background "yellow" "Builtin background color."
  :tag "Builtin background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-background param value 'font-lock-builtin-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-builtin-foreground "red" "Builtin foreground color."
  :tag "Builtin foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-builtin-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-string-background "white" "String background color."
  :tag "String background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-background param value 'font-lock-string-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-comment-foreground "light slate blue" "Comment foreground color."
  :tag "Comment foreground color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-foreground param value 'font-lock-comment-face 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)
(defcustom eide-custom-light-selection-background "bisque" "Selection background color."
  :tag "Selection background color"
  :type 'color
  :set '(lambda (param value) (eide-i-display-set-face-background param value 'region 'light))
  :initialize 'custom-initialize-default
  :group 'eide-emacs-settings-light-colors)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-display-set-menu-bar (param value)
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
        (menu-bar-mode (if eide-display-user-menu-bar-mode 1 -1))))))

(defun eide-i-display-set-tool-bar (param value)
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
        (tool-bar-mode (if eide-display-user-tool-bar-mode 1 -1))))))

(defun eide-i-display-set-scroll-bar-position (param value)
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
        (set-scroll-bar-mode eide-display-user-scroll-bar-mode)))))

(defun eide-i-display-set-font-height (param value)
  "Set font height.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (if eide-config-ready
    (if window-system
      (if (and eide-custom-override-emacs-settings value)
        (set-face-attribute 'default nil :height value)
        (set-face-attribute 'default nil :height eide-display-user-font-height)))))

(defun eide-i-display-set-background (param value color-theme)
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
        (set-background-color value)
        (set-face-background 'fringe value)))))

(defun eide-i-display-set-foreground (param value color-theme)
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
        (set-foreground-color value)
        (set-face-foreground 'font-lock-string-face value)
        (set-face-foreground 'region value)))))

(defun eide-i-display-set-face-background (param value face color-theme)
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

(defun eide-i-display-set-face-foreground (param value face color-theme)
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

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-display-save-emacs-settings ()
  "Save Emacs settings (for display)."
  (setq eide-display-user-menu-bar-mode menu-bar-mode)
  (setq eide-display-user-tool-bar-mode tool-bar-mode)
  (setq eide-display-user-scroll-bar-mode scroll-bar-mode)
  (setq eide-display-user-font-height (face-attribute 'default :height))
  (setq eide-display-user-background-color (face-background 'default))
  (setq eide-display-user-foreground-color (face-foreground 'default))

  (setq eide-display-user-keyword-foreground-color (face-foreground 'font-lock-keyword-face))
  (setq eide-display-user-type-foreground-color (face-foreground 'font-lock-type-face))
  (setq eide-display-user-function-foreground-color (face-foreground 'font-lock-function-name-face))
  (setq eide-display-user-variable-foreground-color (face-foreground 'font-lock-variable-name-face))
  (setq eide-display-user-constant-background-color (face-background 'font-lock-constant-face))
  (setq eide-display-user-constant-foreground-color (face-foreground 'font-lock-constant-face))
  (setq eide-display-user-builtin-background-color (face-background 'font-lock-builtin-face))
  (setq eide-display-user-builtin-foreground-color (face-foreground 'font-lock-builtin-face))
  (setq eide-display-user-string-background-color (face-background 'font-lock-string-face))
  (setq eide-display-user-string-foreground-color (face-foreground 'font-lock-string-face))
  (setq eide-display-user-comment-foreground-color (face-foreground 'font-lock-comment-face))
  (setq eide-display-user-selection-background-color (face-background 'region))
  (setq eide-display-user-selection-foreground-color (face-foreground 'region)))

(defun eide-display-apply-color-theme ()
  "Apply color theme."
  (if eide-config-ready
    (progn
      (eide-menu-apply-color-theme)
      (eide-project-apply-color-theme)
      (eide-help-apply-color-theme))))

(defun eide-display-apply-extended-color-theme ()
  "Apply color theme (for source code)."
  (if eide-config-ready
    (progn
      (if (and eide-custom-override-emacs-settings
               eide-custom-extend-color-theme-to-source-code)
        (progn
          (if (equal eide-custom-color-theme 'dark)
            ;; "Dark" color theme
            (progn
              (setq eide-display-background-color eide-custom-dark-background)
              (setq eide-display-foreground-color eide-custom-dark-foreground)
              (set-background-color eide-custom-dark-background)
              (set-foreground-color eide-custom-dark-foreground)
              (set-face-background 'fringe eide-custom-dark-background)
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
              (set-face-background 'region eide-custom-dark-selection-background)
              ;; Information line
              (set-face-background 'mode-line "gray"))
            ;; "Light" color theme
            (progn
              (setq eide-display-background-color eide-custom-light-background)
              (setq eide-display-foreground-color eide-custom-light-foreground)
              (set-background-color eide-custom-light-background)
              (set-foreground-color eide-custom-light-foreground)
              (set-face-background 'fringe eide-custom-light-background)
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
              (set-face-background 'region eide-custom-light-selection-background)
              ;; Information line
              (set-face-background 'mode-line "wheat"))))
        (progn
          ;; Restore user colors
          (setq eide-display-background-color eide-display-user-background-color)
          (setq eide-display-foreground-color eide-display-user-foreground-color)
          (set-background-color eide-display-user-background-color)
          (set-foreground-color eide-display-user-foreground-color)
          (set-face-background 'fringe eide-display-user-background-color)
          (set-face-foreground 'font-lock-keyword-face eide-display-user-keyword-foreground-color)
          (set-face-foreground 'font-lock-type-face eide-display-user-type-foreground-color)
          (set-face-foreground 'font-lock-function-name-face eide-display-user-function-foreground-color)
          (set-face-foreground 'font-lock-variable-name-face eide-display-user-variable-foreground-color)
          (set-face-background 'font-lock-constant-face eide-display-user-constant-background-color)
          (set-face-foreground 'font-lock-constant-face eide-display-user-constant-foreground-color)
          (set-face-background 'font-lock-builtin-face eide-display-user-builtin-background-color)
          (set-face-foreground 'font-lock-builtin-face eide-display-user-builtin-foreground-color)
          (set-face-background 'font-lock-string-face eide-display-user-string-background-color)
          (set-face-foreground 'font-lock-string-face eide-display-user-string-foreground-color)
          (set-face-foreground 'font-lock-comment-face eide-display-user-comment-foreground-color)
          (set-face-background 'region eide-display-user-selection-background-color)
          (set-face-foreground 'region eide-display-user-selection-foreground-color))))))

(defun eide-display-apply-emacs-settings ()
  "Apply Emacs settings (for display)."
  (eide-i-display-set-menu-bar 'eide-custom-show-menu-bar eide-custom-show-menu-bar)
  (eide-i-display-set-tool-bar 'eide-custom-show-tool-bar eide-custom-show-tool-bar)
  (eide-i-display-set-scroll-bar-position 'eide-custom-scroll-bar-position eide-custom-scroll-bar-position)
  (eide-i-display-set-font-height 'eide-custom-font-height eide-custom-font-height))

(defun eide-display-set-colors-for-files ()
  "Set colors for edition mode."
  (set-background-color eide-display-background-color)
  (set-foreground-color eide-display-foreground-color)
  (set-face-background 'fringe eide-display-background-color))
