;;; eide.el --- Emacs-IDE

;; Copyright (C) 2008-2014 Cédric Marie

;; Author: Cédric Marie <cedric.marie@openmailbox.org>
;; Version: 1.12.1

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
  ;; Shell buffer name will be updated in eide-i-windows-display-buffer-function
  (setq eide-windows-update-output-buffer-id "s")
  (shell))

;;;###autoload
(defun eide-start ()
  "Start Emacs-IDE."
  (if (>= emacs-major-version 24)
    (progn
      (unless (file-directory-p "~/.emacs-ide")
        (make-directory "~/.emacs-ide"))
      ;; Emacs settings must be saved before the desktop is loaded, because it
      ;; reads some variables that might be overridden by local values in buffers.
      (eide-config-init)
      (eide-project-init)
      (eide-project-load-root-directory-content t)
      (eide-menu-init)
      (eide-windows-init))
    (message "Failed to start Emacs-IDE (requires Emacs version >= 24)")))

;;; eide.el ends here
