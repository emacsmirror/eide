;;; eide.el --- Emacs-IDE: Main file

;; Copyright (C) 2008-2018 Cédric Marie

;; Author: Cédric Marie <cedric@hjuvi.lautre.net>
;; Version: 2.1.3
;; URL: https://eide.frama.io/

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; Emacs-IDE (eide) is a package for Emacs that provides IDE features
;; (Integrated Development Environment).

;; Although most of these features are already available in Emacs,
;; the purpose of this package is to integrate them into a user-friendly
;; interface, with dedicated windows (source files, menu, and ouput),
;; convenient keyboard shortcuts, and project management.

;; It is suitable for almost all languages (as long as they are supported by
;; Ctags). Cscope provides additional browsing facility for C/C++ files.

;; Homepage: https://eide.frama.io/

;;; Code:

(provide 'eide)

;; Emacs modules
(require 'desktop)
(require 'hideshow)
(require 'imenu)
(require 'mwheel)
(require 'ediff)

;; Emacs-IDE modules
(require 'eide-compare)
(require 'eide-config)
(require 'eide-edit)
(require 'eide-help)
(require 'eide-keys)
(require 'eide-menu)
(require 'eide-popup)
(require 'eide-project)
(require 'eide-search)
(require 'eide-vc)
(require 'eide-windows)

;; Create a C style based on bsd, with:
;; - 4 spaces (instead of 8)
;; - no tabulations
(c-add-style "bsd-4-spaces"
             '("bsd"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)))

;; Create a C style based on linux, with explicit tabulations
;; (in case indent-tabs-mode is customized nil)
(c-add-style "linux-tabs"
             '("linux"
               (indent-tabs-mode . t)))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-shell-open ()
  "Open a shell."
  (interactive)
  ;; Force to open a new shell (in current directory)
  (when eide-shell-buffer
    (kill-buffer eide-shell-buffer))
  (eide-windows-select-source-window t)
  (shell))

;;;###autoload
(defun eide-start ()
  "Start Emacs-IDE."
  (if (>= emacs-major-version 24)
      (progn
        (unless (file-directory-p "~/.emacs.d/eide")
          (if (file-directory-p "~/.emacs-ide")
              ;; Upgrade from version 2.0.0:
              ;; Store the environment in ~/.emacs.d/eide instead of ~/.emacs-ide
              (rename-file "~/.emacs-ide" "~/.emacs.d/eide")
            ;; Create ~/.emacs.d/eide if it does not exist
            (make-directory "~/.emacs.d/eide")))
        ;; Emacs settings must be saved before the desktop is loaded, because it
        ;; reads some variables that might be overridden by local values in buffers.
        (eide-config-init)
        (eide-project-init)
        (eide-menu-init)
        (eide-windows-init)
        ;; Start with "editor" mode
        (eide-keys-configure-for-editor))
    (message "Failed to start Emacs-IDE (requires Emacs version >= 24)")))

;;; eide.el ends here
