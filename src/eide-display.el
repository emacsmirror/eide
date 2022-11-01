;;; eide-display.el --- Emacs-IDE: Display (color themes)

;; Copyright © 2013-2022 Cédric Marie

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

(require 'eide-menu)
(require 'eide-project)

(defvar eide-display-background-color nil)
(defvar eide-display-foreground-color nil)

(defvar eide-display-color-theme nil)

(defvar eide-display-user-ring-bell-function nil)

(defvar eide-display-user-menu-bar-mode nil)
(defvar eide-display-user-tool-bar-mode nil)

(defvar eide-display-user-line-number-mode nil)
(defvar eide-display-user-column-number-mode nil)
(defvar eide-display-user-which-function-mode nil)
(defvar eide-display-user-show-paren-mode nil)

(defvar eide-display-user-show-trailing-whitespace nil)

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

(defcustom eide-custom-color-theme nil
  "Color theme for Emacs-IDE specific faces (menu, help, and list of projects). If
'auto', it will be set according to Emacs-IDE color theme for source code
(light if none is enabled)."
  :tag "Color theme for Emacs-IDE specific faces (menu, help, and list of projects)"
  :type '(choice (const :tag "auto" nil) (const dark) (const light))
  :set '(lambda (param value) (set-default param value) (eide-display-apply-color-theme))
  :initialize 'custom-initialize-default
  :group 'eide-display)
(defcustom eide-custom-start-maximized t
  "Start with maximized frame."
  :tag "Start with maximized frame"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'eide-display)

(defgroup eide-override-display nil "Sound/display settings."
  :tag "Sound/display"
  :group 'eide-emacs-settings)
(defcustom eide-custom-disable-beeping t
  "Disable beeping (or screen flashing with \"visible bell\") (ring-bell-function
'ignore)."
  :tag "Disable beeping / screen flashing"
  :type '(choice (const :tag "Don't override" nil)
                 (const :tag "Disable" t))
  :set '(lambda (param value) (set-default param value) (eide-i-config-apply-emacs-settings))
  :initialize 'custom-initialize-default
  :group 'eide-override-display)
(defcustom eide-custom-disable-bars t
  "Disable the menu-bar and the tool-bar (menu-bar-mode nil, tool-bar-mode nil)."
  :tag "Disable the menu-bar and the tool-bar"
  :type '(choice (const :tag "Don't override" nil)
                 (const :tag "Disable" t))
  :set '(lambda (param value) (set-default param value) (eide-i-config-apply-emacs-settings))
  :initialize 'custom-initialize-default
  :group 'eide-override-display)
(defcustom eide-custom-add-cursor-position-info t
  "Add information about the cursor position (line-number-mode t,
column-number-mode t, which-function-mode t) and highlight matching parentheses
(show-paren-mode t)."
  :tag "Add information about the cursor position"
  :type '(choice (const :tag "Don't override" nil)
                 (const :tag "Enable" t))
  :set '(lambda (param value) (set-default param value) (eide-i-config-apply-emacs-settings))
  :initialize 'custom-initialize-default
  :group 'eide-override-display)
(defcustom eide-custom-show-trailing-whitespace t
  "Show trailing whitespace (show-trailing-whitespace t)."
  :tag "Show trailing whitespace"
  :type '(choice (const :tag "Don't override" nil)
                 (const :tag "Enable" t))
  :set '(lambda (param value) (set-default param value) (eide-i-config-apply-emacs-settings))
  :initialize 'custom-initialize-default
  :group 'eide-override-display)

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-display-init ()
  "Initialize display."
  ;; Do not display startup screen
  (setq inhibit-startup-screen t))

(defun eide-display-save-emacs-settings ()
  "Save Emacs settings (for display)."
  (setq eide-display-user-ring-bell-function ring-bell-function)

  (setq eide-display-user-menu-bar-mode menu-bar-mode)
  (setq eide-display-user-tool-bar-mode tool-bar-mode)

  (setq eide-display-user-line-number-mode line-number-mode)
  (setq eide-display-user-column-number-mode column-number-mode)
  (setq eide-display-user-which-function-mode which-function-mode)
  (setq eide-display-user-show-paren-mode show-paren-mode)

  (setq eide-display-user-show-trailing-whitespace show-trailing-whitespace))

(defun eide-display-apply-emacs-settings ()
  "Apply Emacs settings (for display)."
  (if (and eide-custom-override-emacs-settings eide-custom-disable-beeping)
      (setq ring-bell-function 'ignore)
    (setq ring-bell-function eide-display-user-ring-bell-function))
  (if (and eide-custom-override-emacs-settings eide-custom-disable-bars)
      (progn
        ;; Hide menu-bar and tool-bar (eide menu is enough)
        (menu-bar-mode 0)
        (tool-bar-mode 0))
    (progn
      (if eide-display-user-menu-bar-mode
          (menu-bar-mode 1)
        (menu-bar-mode 0))
      (if eide-display-user-tool-bar-mode
          (tool-bar-mode 1)
        (tool-bar-mode 0))))
  (if (and eide-custom-override-emacs-settings eide-custom-add-cursor-position-info)
      (progn
        ;; Display line and column numbers
        (line-number-mode 1)
        (column-number-mode 1)
        ;; Display current function (at cursor position)
        (which-function-mode 1)
        ;; Highlight matching parentheses (when cursor on "(" or just after ")")
        (show-paren-mode 1))
    (progn
      (if eide-display-user-line-number-mode
          (line-number-mode 1)
        (line-number-mode 0))
      (if eide-display-user-column-number-mode
          (column-number-mode 1)
        (column-number-mode 0))
      (if eide-display-user-which-function-mode
          (which-function-mode 1)
        (which-function-mode 0))
      (if eide-display-user-show-paren-mode
          (show-paren-mode 1)
        (show-paren-mode 0))))
  (if (and eide-custom-override-emacs-settings eide-custom-show-trailing-whitespace)
      ;; Show trailing whitespaces
      (setq-default show-trailing-whitespace t)
    (setq-default show-trailing-whitespace eide-display-user-show-trailing-whitespace)))

(defun eide-display-apply-color-theme ()
  "Apply color theme."
  (when eide-config-ready
    (if eide-custom-color-theme
        ;; Color theme for Emacs-IDE specific faces is forced
        (setq eide-display-color-theme eide-custom-color-theme)
      ;; Color theme for Emacs-IDE specific faces is not forced
      ;; and depends on which Emacs-IDE color theme is enabled
      (if (custom-theme-enabled-p 'eide-dark)
          ;; If eide-dark theme is enabled, use dark color theme for Emacs-IDE
          ;; specific faces
          (setq eide-display-color-theme 'dark)
        ;; If eide-light theme is enabled, or neither eide-dark nor eide-light
        ;; theme is enabled, use light color theme for Emacs-IDE specific faces
        (setq eide-display-color-theme 'light)))
    ;; Save current colors
    (setq eide-display-background-color (face-background 'default))
    (setq eide-display-foreground-color (face-foreground 'default))
    (eide-menu-apply-color-theme)
    (eide-project-apply-color-theme)))

(defun eide-display-set-colors-for-files ()
  "Set colors for edition mode."
  (set-background-color eide-display-background-color)
  (set-foreground-color eide-display-foreground-color)
  (set-face-background 'fringe eide-display-background-color))

;;; eide-display.el ends here
