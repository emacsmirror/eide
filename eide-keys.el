;;; eide-key-bindings.el --- Emacs-IDE, key bindings

;; Copyright (C) 2005-2008 Cédric Marie

;; This program is free software ; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation ; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY ; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program ; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code:

(provide 'eide-key-bindings)


;;;; ==========================================================================
;;;; FUNCTIONS FOR MOVING
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-scroll-right-one-step ()
  (interactive)
  (let ((index 0))
    (while (and (not (eolp)) (< index 4))
      (progn (scroll-left) (forward-char) (setq index (1+ index))))))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-scroll-left-one-step ()
  (interactive)
  (let ((index 0))
    (while (and (not (bolp)) (< index 4))
      (progn (scroll-right) (backward-char) (setq index (1+ index))))))


;;;; ==========================================================================
;;;; KEYS BINDINGS
;;;; ==========================================================================

;(defun eide-local-insert-user-defined-lines-1 ()
;  (interactive)
;  (beginning-of-line)
;  (insert "#ifdef FLAG\n#else\n")
;  (forward-line)
;  (beginning-of-line)
;  (insert "#endif\n"))

;(defun eide-local-insert-user-defined-lines-2 ()
;  (interactive)
;  (beginning-of-line)
;  (insert "#ifndef FLAG\n")
;  (forward-line)
;  (beginning-of-line)
 ; (insert "#endif\n"))

;; To select text with <shift>
(if (not (featurep 'xemacs))
  (pc-selection-mode))

;; To delete selected text when typing
(if (featurep 'xemacs)
  (cond
    ((fboundp 'turn-on-pending-delete)
     (turn-on-pending-delete))
    ((fboundp 'pending-delete-on)
     (pending-delete-on t))))

;; Cut-copy-paste
;; (impossible to use Windows shortcuts, because Control-c and Control-x have
;; other meanings)
;; Alt-left  : Cut   (Control-x)
;; Alt-down  : Copy  (Control-c)
;; Alt-right : Paste (Control-v)
(if (featurep 'xemacs)
  (progn
    (global-set-key [(meta left)]  'kill-region)
    (global-set-key [(meta down)]  'kill-ring-save)
    (global-set-key [(meta right)] 'yank))
  (progn
    (global-set-key [M-left]  'kill-region)
    (global-set-key [M-down]  'kill-ring-save)
    (global-set-key [M-right] 'yank)))

;; Cut-copy-paste with mouse
;; Control-mouse-1 : Cut   (Control-x)
;; Control-mouse-2 : Copy  (Control-c)
;; Control-mouse-3 : Paste (Control-v)
(if (featurep 'xemacs)
  (progn
    (global-set-key [(control button1)] 'kill-region)
    (global-set-key [(control button2)] 'kill-ring-save)
    (global-set-key [(control button3)] 'yank))
  (progn
    (global-unset-key [C-mouse-1])
    (global-unset-key [C-mouse-2])
    (global-unset-key [C-mouse-3])
    (global-set-key [C-down-mouse-1] 'kill-region)
    (global-set-key [C-down-mouse-2] 'kill-ring-save)
    (global-set-key [C-down-mouse-3] 'yank)))

;; Symbol completion
;;(global-set-key [C-tab] 'complete-symbol)

(if (featurep 'xemacs)
  (progn
    ;; Display possible symbols one after the other
    (global-set-key [(control tab)] 'dabbrev-expand)
    ;; Display the list of possible symbols (in another window)
    (global-set-key [(shift tab)] 'dabbrev-completion))
  (progn
    ;; Display possible symbols one after the other
    (global-set-key [C-tab] 'dabbrev-expand)
    ;; Display the list of possible symbols (in another window)
    (global-set-key [S-tab] 'dabbrev-completion)))

;; Override find-file, to get default directory from window "file"
(global-set-key "\C-x\C-f" 'eide-windows-find-file)


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Set key bindings for project
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-enable-keys-for-project ()
  (if (and eide-option-use-cscope-flag (not eide-option-use-cscope-and-tags-flag))
    (progn
      (global-set-key [f1]   'eide-search-back-from-symbol-definition)
      (global-set-key [f2]   'eide-search-find-symbol-definition-without-prompt)
      (if (featurep 'xemacs)
        (global-set-key [(shift f2)] 'eide-search-find-symbol-definition-with-prompt)
        (global-set-key [S-f2] 'eide-search-find-symbol-definition-with-prompt)))
    (progn
      (global-set-key [f1]   'eide-search-back-from-tag)
      (global-set-key [f2]   'eide-search-find-tag-without-prompt)
      (if (featurep 'xemacs)
        (global-set-key [(shift f2)] 'eide-search-find-tag-with-prompt)
        (global-set-key [S-f2] 'eide-search-find-tag-with-prompt))
      (if (featurep 'xemacs)
        (global-set-key [(shift f1)] 'eide-search-find-alternate-tag)
        (global-set-key [S-f1] 'eide-search-find-alternate-tag))))

  (if (and eide-option-use-cscope-flag (string-equal eide-project-type "C"))
    (progn
      (global-set-key [f3] 'eide-search-find-symbol-without-prompt)
      (if (featurep 'xemacs)
        (global-set-key [(shift f3)] 'eide-search-find-symbol-with-prompt)
        (global-set-key [S-f3] 'eide-search-find-symbol-with-prompt)))
    (progn
      (global-unset-key [f3])
      (if (featurep 'xemacs)
        (global-unset-key [(shift f3)])
        (global-unset-key [S-f3]))))

  (global-set-key [f4]   'eide-search-grep-find-without-prompt)
  (if (featurep 'xemacs)
    (global-set-key [(shift f4)] 'eide-search-grep-find-with-prompt)
    (global-set-key [S-f4] 'eide-search-grep-find-with-prompt))

  (global-set-key [f9] 'eide-project-compile-1)
  (if (featurep 'xemacs)
    (global-set-key [(shift f9)] 'eide-project-compile-2)
    (global-set-key [S-f9] 'eide-project-compile-2))

  (global-set-key [f10] 'eide-project-run-1)
  (if (featurep 'xemacs)
    (global-set-key [(shift f10)] 'eide-project-run-2)
    (global-set-key [S-f10] 'eide-project-run-2))

  (global-set-key [f11] 'eide-project-debug-1)
  (if (featurep 'xemacs)
    (global-set-key [(shift f11)] 'eide-project-debug-2)
    (global-set-key [S-f11] 'eide-project-debug-2)))

;; ----------------------------------------------------------------------------
;; Unset key bindings
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-disable-keys-for-project ()
  (global-unset-key [f1])
  (global-unset-key [f2])
  (if (featurep 'xemacs)
    (global-unset-key [(shift f2)])
    (global-unset-key [S-f2]))
  (if (featurep 'xemacs)
    (global-unset-key [(shift f1)])
    (global-unset-key [S-f1]))

  (global-unset-key [f3])
  (global-unset-key [f4])
  (if (featurep 'xemacs)
    (progn
      (global-unset-key [(shift f3)])
      (global-unset-key [(shift f4)]))
    (progn
      (global-unset-key [S-f3])
      (global-unset-key [S-f4])))
  (global-unset-key [f9])
  (if (featurep 'xemacs)
    (global-unset-key [(shift f9)])
    (global-unset-key [S-f9]))

  (global-unset-key [f10])
  (if (featurep 'xemacs)
    (global-unset-key [(shift f10)])
    (global-unset-key [S-f10]))

  (global-unset-key [f11])
  (if (featurep 'xemacs)
    (global-unset-key [(shift f11)])
    (global-unset-key [S-f11])))

;; ----------------------------------------------------------------------------
;; Set key bindings that can be used without project
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-enable-keys-for-grep ()
  (global-set-key [f6] 'eide-search-grep-without-prompt)
  (if (featurep 'xemacs)
    (global-set-key [(shift f6)] 'eide-search-grep-with-prompt)
    (global-set-key [S-f6] 'eide-search-grep-with-prompt))

  (global-set-key [f7] 'eide-search-grep-go-to-previous)
  (global-set-key [f8] 'eide-search-grep-go-to-next))

;; ----------------------------------------------------------------------------
;; Unset key bindings that can be used without project
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-disable-keys-for-grep ()
  (global-unset-key [f6])
  (if (featurep 'xemacs)
    (global-unset-key [(shift f6)])
    (global-unset-key [S-f6]))
  (global-unset-key [f7])
  (global-unset-key [f8]))

;; ----------------------------------------------------------------------------
;; Set key bindings (misc)
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-enable-keys-misc ()
  ;; Block hiding
  (if (featurep 'xemacs)
    (progn
      (global-set-key [(control f1)] 'hs-hide-block)
      (global-set-key [(control f2)] 'hs-show-block)
      (global-set-key [(control f3)] 'hs-hide-all)
      (global-set-key [(control f4)] 'hs-show-all))
    (progn
      (global-set-key [C-f1] 'hs-hide-block)
      (global-set-key [C-f2] 'hs-show-block)
      (global-set-key [C-f3] 'hs-hide-all)
      (global-set-key [C-f4] 'hs-show-all)))

  ;; Display
  (global-set-key [f5] 'eide-menu-revert-buffer)
  (if (featurep 'xemacs)
    (global-set-key [(shift f5)] 'eide-menu-kill-buffer)
    (global-set-key [S-f5] 'eide-menu-kill-buffer))

  ;; Unix Shell commands
  (global-set-key [f12] 'eide-shell-open)

  ;; Menu pop up "liste des fonctions" : click droit dans la fenêtre
  ;(global-set-key [down-mouse-3] 'imenu)

  (if (featurep 'xemacs)
    (global-set-key [button3] 'eide-windows-handle-mouse-3)
    (global-set-key [mouse-3] 'eide-windows-handle-mouse-3))

  (if (not (featurep 'xemacs))
    (global-set-key [S-down-mouse-3] 'eide-windows-handle-shift-mouse-3))

  ;; Control + Wheel up (resize windows layout)
  (if (not (featurep 'xemacs))
    (global-set-key [C-mouse-4] 'eide-windows-handle-control-mouse-4))

  ;; Control + Wheel down (resize windows layout)
  (if (not (featurep 'xemacs))
    (global-set-key [C-mouse-5] 'eide-windows-handle-control-mouse-5))

  ;; Shift + Wheel up (horizontal scrolling)
  (if (not (featurep 'xemacs))
    (global-set-key [S-mouse-4] 'eide-key-bindings-internal-scroll-right-one-step))

  ;; Shift + Wheel down (horizontal scrolling)
  (if (not (featurep 'xemacs))
    (global-set-key [S-mouse-5] 'eide-key-bindings-internal-scroll-left-one-step)))


;; ----------------------------------------------------------------------------
;; Unset key bindings (misc)
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-disable-keys-misc ()
  ;; Block hiding
  (if (featurep 'xemacs)
    (progn
      (global-unset-key [(control f1)])
      (global-unset-key [(control f2)])
      (global-unset-key [(control f3)])
      (global-unset-key [(control f4)]))
    (progn
      (global-unset-key [C-f1])
      (global-unset-key [C-f2])
      (global-unset-key [C-f3])
      (global-unset-key [C-f4])))

  ;; Display
  (global-unset-key [f5])
  (if (featurep 'xemacs)
    (global-unset-key [(shift f5)])
    (global-unset-key [S-f5]))

  ;; Unix Shell commands
  (global-unset-key [f12])

  (if (featurep 'xemacs)
    (global-unset-key [button3])
    (global-unset-key [mouse-3]))

  (if (not (featurep 'xemacs))
    (global-unset-key [S-down-mouse-3]))

  ;; Control + Wheel up (resize windows layout)
  (if (not (featurep 'xemacs))
    (global-unset-key [C-mouse-4]))

  ;; Control + Wheel down (resize windows layout)
  (if (not (featurep 'xemacs))
    (global-unset-key [C-mouse-5]))

  ;; Shift + Wheel up (horizontal scrolling)
  (if (not (featurep 'xemacs))
    (global-unset-key [S-mouse-4]))

  ;; Shift + Wheel down (horizontal scrolling)
  (if (not (featurep 'xemacs))
    (global-unset-key [S-mouse-5])))


;; ----------------------------------------------------------------------------
;; Set key bindings for ediff session
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-enable-keys-for-ediff ()
  (if (featurep 'xemacs)
    (global-set-key [button3] 'eide-compare-quit)
    (global-set-key [mouse-3] 'eide-compare-quit))
  (global-set-key [f1] 'eide-compare-copy-a-to-b)
  (global-set-key [f2] 'eide-compare-copy-b-to-a)
  (global-set-key [f5] 'eide-compare-update)
  (global-set-key [f7] 'eide-compare-go-to-previous-diff)
  (global-set-key [f8] 'eide-compare-go-to-next-diff))

;; ----------------------------------------------------------------------------
;; Set key bindings for configuration editing
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-internal-enable-keys-for-special-buffer ()
  (if (featurep 'xemacs)
    (global-set-key [button3] 'eide-windows-handle-mouse-3)
    (global-set-key [mouse-3] 'eide-windows-handle-mouse-3)))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(if (featurep 'xemacs)
  (global-set-key [button2] 'eide-windows-handle-mouse-2)
  (global-set-key [mouse-2] 'eide-windows-handle-mouse-2))

;; Pour classer les buffers par catégories (C/C++ files, etc...)
;(msb-mode)
;(setq msb-display-most-recently-used nil)

;(if (not (eq system-type 'windows-nt))
;  (global-set-key [vertical-scroll-bar mouse-1] 'scroll-bar-drag))

;(if (featurep 'xemacs)
;  (global-set-key [(f5)] 'eide-local-insert-user-defined-lines-1)
;  (global-set-key [C-S-f1] 'eide-local-insert-user-defined-lines-1))

;(if (featurep 'xemacs)
;  (global-set-key [(control-f5)] 'eide-local-insert-user-defined-lines-2)
;  (global-set-key [C-S-f2] 'eide-local-insert-user-defined-lines-2))

; Disable some mode-line default key bindings (mouse-delete-window and mouse-delete-other-windows)
(global-set-key [mode-line mouse-2] nil)
(global-set-key [mode-line mouse-3] nil)

;; Override speedbar default key-binding
(if (featurep 'xemacs)
  (define-key speedbar-key-map 'button1 'speedbar-click)
  (define-key speedbar-key-map [mouse-1] 'speedbar-click))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-configure-for-editor ()
  (if eide-project-name
    (eide-key-bindings-internal-enable-keys-for-project)
    (eide-key-bindings-internal-disable-keys-for-project))
  (eide-key-bindings-internal-enable-keys-for-grep)
  (eide-key-bindings-internal-enable-keys-misc))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-configure-for-ediff ()
  (eide-key-bindings-internal-disable-keys-for-project)
  (eide-key-bindings-internal-disable-keys-for-grep)
  (eide-key-bindings-internal-disable-keys-misc)
  (eide-key-bindings-internal-enable-keys-for-ediff))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-key-bindings-configure-for-special-buffer ()
  (eide-key-bindings-internal-disable-keys-for-project)
  (eide-key-bindings-internal-disable-keys-for-grep)
  (eide-key-bindings-internal-disable-keys-misc)
  (eide-key-bindings-internal-enable-keys-for-special-buffer))

;;; eide-key-bindings.el ends here
