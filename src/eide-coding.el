;;; eide-coding.el --- Emacs-IDE, coding

;; Copyright (C) 2008-2014 Cédric Marie

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

;; ----------------------------------------------------------------------------
;; OPTIONS
;; ----------------------------------------------------------------------------

;; Exclude "_" from word delimiters (when selecting by double-click)
(defvar eide-option-select-whole-symbol-flag t)

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-coding-init ()
  "Add hooks for major modes."

  ;; C major mode
  (add-hook
   'c-mode-hook
   '(lambda()
      (when eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" c-mode-syntax-table))))

  ;; C++ major mode
  (add-hook
   'c++-mode-hook
   '(lambda()
      (when eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" c-mode-syntax-table))))

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
      (when eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" sh-mode-syntax-table))))

  ;; Emacs Lisp major mode
  (add-hook
   'emacs-lisp-mode-hook
   '(lambda()
      (when eide-option-select-whole-symbol-flag
        ;; "-" should not be a word delimiter
        (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))
      (when eide-custom-override-emacs-settings
        ;; Indentation after "if" (with default behaviour, the "then" statement is
        ;; more indented than the "else" statement)
        (put 'if 'lisp-indent-function 1))))

  ;; Python major mode
  (add-hook
   'python-mode-hook
   '(lambda()
      (when eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" python-mode-syntax-table)))))

;;; eide-coding.el ends here
