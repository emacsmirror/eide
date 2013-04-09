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

(require 'eide-coding)
(require 'eide-display)
(require 'eide-menu)
(require 'eide-search)
(require 'eide-vc)

(defvar eide-config-ready nil)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION GROUPS AND VARIABLES
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

(defgroup eide-search nil "Cscope option."
  :tag "Search"
  :group 'eide-emacs-settings)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-config-apply-emacs-settings ()
  "Apply \"Emacs settings\" options."
  (if eide-config-ready
    (progn
      (eide-display-apply-extended-color-theme)
      (eide-menu-update-background-color)
      (eide-display-apply-emacs-settings)
      (eide-coding-apply-emacs-settings)
      (eide-search-apply-customization))))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-config-init ()
  "Config initialization: save Emacs settings."
  (eide-coding-save-emacs-settings)
  (eide-display-save-emacs-settings)
  (eide-search-save-emacs-settings))

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

(defun eide-config-open-customization ()
  "Display customization (full frame)."
  (eide-windows-layout-unbuild)
  (eide-keys-configure-for-special-buffer)
  (customize-group 'eide))

;;; eide-config.el ends here
