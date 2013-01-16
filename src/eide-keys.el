;;; eide-keys.el --- Emacs-IDE, keys

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

(provide 'eide-keys)

(defvar eide-keys-is-editor-configuration-active-flag nil)

;; ----------------------------------------------------------------------------
;; FUNCTIONS FOR MOVING
;; ----------------------------------------------------------------------------

(defun eide-i-keys-scroll-right-one-step ()
  "Scroll right in current buffer."
  (interactive)
  (let ((index 0))
    (while (and (not (eolp)) (< index 4))
      (progn (scroll-left) (forward-char) (setq index (1+ index))))))

(defun eide-i-keys-scroll-left-one-step ()
  "Scroll left in current buffer."
  (interactive)
  (let ((index 0))
    (while (and (not (bolp)) (< index 4))
      (progn (scroll-right) (backward-char) (setq index (1+ index))))))

;; ----------------------------------------------------------------------------
;; KEYS BINDINGS
;; ----------------------------------------------------------------------------

;;(defun eide-local-insert-user-defined-lines-1 ()
;;  (interactive)
;;  (beginning-of-line)
;;  (insert "#ifdef FLAG\n#else\n")
;;  (forward-line)
;;  (beginning-of-line)
;;  (insert "#endif\n"))

;;(defun eide-local-insert-user-defined-lines-2 ()
;;  (interactive)
;;  (beginning-of-line)
;;  (insert "#ifndef FLAG\n")
;;  (forward-line)
;;  (beginning-of-line)
;;  (insert "#endif\n"))

;; To delete selected text
(if (fboundp 'pc-selection-mode)
  ;; emacs24: pc-selection-mode is deprecated (this is now the default behaviour)
  (pc-selection-mode))

;; Cut-copy-paste
;; (impossible to use Windows shortcuts, because Control-c and Control-x have
;; other meanings)
;; Alt-left:  Cut   (Control-x)
;; Alt-down:  Copy  (Control-c)
;; Alt-right: Paste (Control-v)
(global-set-key [M-left]  'kill-region)
(global-set-key [M-down]  'kill-ring-save)
(global-set-key [M-right] 'yank)

;; Cut-copy-paste with mouse
;; Control-mouse-1: Cut   (Control-x)
;; Control-mouse-2: Copy  (Control-c)
;; Control-mouse-3: Paste (Control-v)
(global-unset-key [C-mouse-1])
(global-unset-key [C-mouse-2])
(global-unset-key [C-mouse-3])
(global-set-key [C-down-mouse-1] 'kill-region)
(global-set-key [C-down-mouse-2] 'kill-ring-save)
(global-set-key [C-down-mouse-3] 'yank)

;; Symbol completion
;;(global-set-key [C-tab] 'complete-symbol)

;; Display possible symbols one after the other
(global-set-key [C-tab] 'dabbrev-expand)
;; Display the list of possible symbols (in another window)
(global-set-key [S-tab] 'dabbrev-completion)

;; Override find-file, to get default directory from "source" window
(global-set-key "\C-x\C-f" 'eide-windows-find-file)

(global-set-key [f11] 'eide-windows-toggle-frame-fullscreen-mode)

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-keys-enable-keys-for-project ()
  "Set key bindings for project."
  (global-set-key [f1] 'eide-search-back-from-tag)
  (global-set-key [f2] 'eide-search-find-tag-without-prompt)
  (global-set-key [S-f2] 'eide-search-find-tag-with-prompt)
  (global-set-key [S-f1] 'eide-search-find-alternate-tag)

  (if eide-option-use-cscope-flag
    (progn
      (global-set-key [f3] 'eide-search-find-symbol-without-prompt)
      (global-set-key [S-f3] 'eide-search-find-symbol-with-prompt))
    (progn
      (global-unset-key [f3])
      (global-unset-key [S-f3])))

  (global-set-key [f9] 'eide-project-compile-1)
  (global-set-key [S-f9] 'eide-project-compile-2)

  (global-set-key [f10] 'eide-project-run-1)
  (global-set-key [S-f10] 'eide-project-run-2))

(defun eide-i-keys-disable-keys-for-project ()
  "Unset key bindings for project."
  (global-unset-key [f1])
  (global-unset-key [f2])
  (global-unset-key [S-f2])
  (global-unset-key [S-f1])

  (global-unset-key [f3])
  (global-unset-key [S-f3])

  (global-unset-key [f9])
  (global-unset-key [S-f9])

  (global-unset-key [f10])
  (global-unset-key [S-f10]))

(defun eide-i-keys-enable-keys-for-grep ()
  "Set key bindings that can be used without project."
  (global-set-key [f4] 'eide-search-grep-global-without-prompt)
  (global-set-key [S-f4] 'eide-search-grep-global-with-prompt)

  (global-set-key [f6] 'eide-search-grep-local-without-prompt)
  (global-set-key [S-f6] 'eide-search-grep-local-with-prompt)

  (global-set-key [f7] 'eide-search-grep-go-to-previous)
  (global-set-key [f8] 'eide-search-grep-go-to-next))

(defun eide-i-keys-disable-keys-for-grep ()
  "Unset key bindings that can be used without project."
  (global-unset-key [f4])
  (global-unset-key [S-f4])
  (global-unset-key [f6])
  (global-unset-key [S-f6])
  (global-unset-key [f7])
  (global-unset-key [f8]))

(defun eide-i-keys-enable-keys-misc ()
  "Set key bindings for misc features."
  ;; Block hiding
  ;;(global-set-key [C-f1] 'hs-hide-block)
  ;;(global-set-key [C-f2] 'hs-show-block)
  ;;(global-set-key [C-f3] 'hs-hide-all)
  ;;(global-set-key [C-f4] 'hs-show-all)

  ;; Display
  (global-set-key [f5] 'eide-menu-revert-buffers)
  (global-set-key [S-f5] 'eide-menu-kill-buffer)

  ;; Unix Shell commands
  (global-set-key [f12] 'eide-shell-open)

  (global-set-key [mouse-3] 'eide-windows-handle-mouse-3)

  (global-set-key [S-down-mouse-3] 'eide-windows-handle-shift-mouse-3)

  ;; Shift + Wheel up (horizontal scrolling)
  (global-set-key [S-mouse-4] 'eide-i-keys-scroll-right-one-step)

  ;; Shift + Wheel down (horizontal scrolling)
  (global-set-key [S-mouse-5] 'eide-i-keys-scroll-left-one-step))

(defun eide-i-keys-disable-keys-misc ()
  "Unset key bindings for misc features."
  ;; Block hiding
  ;;(global-unset-key [C-f1])
  ;;(global-unset-key [C-f2])
  ;;(global-unset-key [C-f3])
  ;;(global-unset-key [C-f4])

  ;; Display
  (global-unset-key [f5])
  (global-unset-key [S-f5])

  ;; Unix Shell commands
  (global-unset-key [f12])

  (global-unset-key [mouse-3])

  (global-unset-key [S-down-mouse-3])

  ;; Control + Wheel up (resize windows layout)
  (global-unset-key [C-mouse-4])

  ;; Control + Wheel down (resize windows layout)
  (global-unset-key [C-mouse-5])

  ;; Shift + Wheel up (horizontal scrolling)
  (global-unset-key [S-mouse-4])

  ;; Shift + Wheel down (horizontal scrolling)
  (global-unset-key [S-mouse-5]))

(defun eide-i-keys-enable-keys-for-ediff ()
  "Set key bindings for ediff session."
  (global-set-key [mouse-3] 'eide-compare-quit)
  (global-set-key [f1] 'eide-compare-copy-a-to-b)
  (global-set-key [f2] 'eide-compare-copy-b-to-a)
  (global-set-key [f5] 'eide-compare-update)
  (global-set-key [f7] 'eide-compare-go-to-previous-diff)
  (global-set-key [f8] 'eide-compare-go-to-next-diff))

(defun eide-i-keys-enable-keys-for-gdb ()
  "Set key bindings for gdb session."
  (global-set-key [mouse-3] 'eide-windows-handle-mouse-3))

(defun eide-i-keys-enable-keys-for-special-buffer ()
  "Set key bindings for configuration editing."
  (global-set-key [mouse-3] 'eide-windows-handle-mouse-3)
  (global-set-key [S-down-mouse-3] 'eide-windows-handle-shift-mouse-3))


(global-set-key [mouse-2] 'eide-windows-handle-mouse-2)

;; Pour classer les buffers par catégories (C/C++ files, etc...)
;;(msb-mode)
;;(setq msb-display-most-recently-used nil)

;;(if (not (eq system-type 'windows-nt))
;;  (global-set-key [vertical-scroll-bar mouse-1] 'scroll-bar-drag))

;;(global-set-key [C-S-f1] 'eide-local-insert-user-defined-lines-1)
;;(global-set-key [C-S-f2] 'eide-local-insert-user-defined-lines-2)

;; Disable some mode-line default key bindings (mouse-delete-window and mouse-delete-other-windows)
(global-set-key [mode-line mouse-2] nil)
(global-set-key [mode-line mouse-3] nil)

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-keys-configure-for-editor ()
  "Configure keys for edition mode."
  (setq eide-keys-is-editor-configuration-active-flag t)
  (if eide-project-name
    (eide-i-keys-enable-keys-for-project)
    (eide-i-keys-disable-keys-for-project))
  (eide-i-keys-enable-keys-for-grep)
  (eide-i-keys-enable-keys-misc))

(defun eide-keys-configure-for-ediff ()
  "Configure keys for ediff session."
  (setq eide-keys-is-editor-configuration-active-flag nil)
  (eide-i-keys-disable-keys-for-project)
  (eide-i-keys-disable-keys-for-grep)
  (eide-i-keys-disable-keys-misc)
  (eide-i-keys-enable-keys-for-ediff))

(defun eide-keys-configure-for-gdb ()
  "Configure keys for gdb session."
  (setq eide-keys-is-editor-configuration-active-flag nil)
  (eide-i-keys-disable-keys-for-project)
  (eide-i-keys-disable-keys-for-grep)
  (eide-i-keys-disable-keys-misc)
  (eide-i-keys-enable-keys-for-gdb))

(defun eide-keys-configure-for-special-buffer ()
  "Configure keys for configuration editing."
  (setq eide-keys-is-editor-configuration-active-flag nil)
  (eide-i-keys-disable-keys-for-project)
  (eide-i-keys-disable-keys-for-grep)
  (eide-i-keys-disable-keys-misc)
  (eide-i-keys-enable-keys-for-special-buffer))

;;; eide-keys.el ends here
