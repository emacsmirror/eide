;;; eide-toolbar.el --- Emacs-IDE, toolbar

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

(provide 'eide-toolbar)


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Insert text in "toolbar" buffer (with white background)
;;
;; input  :   string : string to insert
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-insert-text (string)
  (if eide-option-buffer-menu-white-background-flag
    (put-text-property (point) (progn (insert string) (point)) 'face 'eide-menu-white-background-face)
    (insert string)))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-view-previous-file ()
  (interactive)
  (eide-windows-select-window-file t)
  (eide-display-skip-unwanted-buffers t nil))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-view-next-file ()
  (interactive)
  (eide-windows-select-window-file t)
  (eide-display-skip-unwanted-buffers t t))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-view-buffer-in-window-results (my-buffer)
  (if my-buffer
    (progn
      (eide-windows-select-window-results t)
      (switch-to-buffer my-buffer)
      (eide-toolbar-update))
    (message "This buffer has not been created yet...")))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-view-compile-buffer ()
  (interactive)
  (eide-toolbar-internal-view-buffer-in-window-results eide-buffer-compile))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-view-run-buffer ()
  (interactive)
  (eide-toolbar-internal-view-buffer-in-window-results eide-buffer-run))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-view-debug-buffer ()
  (interactive)
  (eide-toolbar-internal-view-buffer-in-window-results eide-buffer-debug))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-view-shell-buffer ()
  (interactive)
  (eide-toolbar-internal-view-buffer-in-window-results eide-buffer-shell))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-send-string-to-debug-buffer (my-string)
  (eide-windows-select-window-results t)
  (process-send-string nil (concat "echo " my-string "\n"))
;  (process-send-string nil (concat "info source\n"))
  (process-send-string nil (concat my-string "\n")))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-next ()
  (interactive)
;  (gud-next "next"))
  (eide-toolbar-internal-send-string-to-debug-buffer "n"))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-step ()
  (interactive)
;  (gud-step "step 1"))
  (eide-toolbar-internal-send-string-to-debug-buffer "s"))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-continue ()
  (interactive)
;  (gud-cont "cont"))
  (eide-toolbar-internal-send-string-to-debug-buffer "c"))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-break ()
  (interactive)
  ;; Get full file name (the same file name may be used in different
  ;; directories, and gdb chooses the first one !)
  (eide-windows-select-window-file t)
;  (gud-break (concat "\"" (buffer-file-name) ":" (number-to-string (count-lines (point-min) (point))) "\"")))
  (eide-toolbar-internal-send-string-to-debug-buffer (concat "break " (buffer-file-name) ":" (number-to-string (count-lines (point-min) (point))))))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-info-break ()
  (interactive)
  (eide-toolbar-internal-send-string-to-debug-buffer "info break"))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-bt ()
  (interactive)
  (eide-toolbar-internal-send-string-to-debug-buffer "bt"))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-up ()
  (interactive)
  (eide-toolbar-internal-send-string-to-debug-buffer "up"))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-down ()
  (interactive)
  (eide-toolbar-internal-send-string-to-debug-buffer "down"))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-debug-execute-info-threads ()
  (interactive)
  (eide-toolbar-internal-send-string-to-debug-buffer "info threads"))

;; ----------------------------------------------------------------------------
;; Insert tab name in "toolbar" buffer
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-insert-tab-in-toolbar-buffer (my-buffer-name my-current-buffer-name my-map my-real-buffer)
  (put-text-property (setq from-point (point)) (progn (insert my-buffer-name) (point)) 'keymap my-map)
  (if my-real-buffer
    (if (string-equal my-buffer-name my-current-buffer-name)
      (put-text-property from-point (point) 'face 'eide-menu-current-tab-face)
      (put-text-property from-point (point) 'face 'eide-menu-other-tab-face))
    (put-text-property from-point (point) 'face 'eide-menu-disabled-tab-face))
  (put-text-property from-point (point) 'mouse-face 'highlight))

;; ----------------------------------------------------------------------------
;; Insert action in "menu" or "toolbar" buffer
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-insert-action-text (string my-map)
  (put-text-property (setq from-point (point)) (progn (insert string) (point)) 'keymap my-map)
  (put-text-property from-point (point) 'face 'eide-menu-action-face)
  (put-text-property from-point (point) 'mouse-face 'highlight))

;; ----------------------------------------------------------------------------
;; Insert common actions in "toolbar" buffer
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-insert-common-items-in-toolbar-buffer (my-current-buffer-name)
  (eide-toolbar-internal-insert-text " ")

  (eide-toolbar-internal-insert-action-text " <<" toolbar-file-previous-map)
  (eide-toolbar-internal-insert-text " ")
  (eide-toolbar-internal-insert-action-text ">> " toolbar-file-next-map)
  (eide-toolbar-internal-insert-text " - ")

  (if eide-session-project
    (progn
      (eide-toolbar-internal-insert-tab-in-toolbar-buffer "compile" my-current-buffer-name toolbar-compile-map eide-buffer-compile)
      (eide-toolbar-internal-insert-text " / ")
      (eide-toolbar-internal-insert-tab-in-toolbar-buffer "run" my-current-buffer-name toolbar-run-map eide-buffer-run)
      (eide-toolbar-internal-insert-text " / ")
      (eide-toolbar-internal-insert-tab-in-toolbar-buffer "debug" my-current-buffer-name toolbar-debug-map eide-buffer-debug)
      (eide-toolbar-internal-insert-text " / ")))
  (eide-toolbar-internal-insert-tab-in-toolbar-buffer "shell" my-current-buffer-name toolbar-shell-map eide-buffer-shell)
  (if (and (string-equal eide-custom-toolbar-position "middle")
           (string-equal eide-custom-menu-height "full")
           (string-equal eide-toolbar-current-tab "debug"))
    (eide-toolbar-internal-insert-text "\n           ")
    (if (string-equal eide-toolbar-current-tab "debug")
      (eide-toolbar-internal-insert-text " - ")
      (eide-toolbar-internal-insert-text "           "))))

;; ----------------------------------------------------------------------------
;; Insert specific actions for "debug" in "toolbar" buffer
;; ----------------------------------------------------------------------------
(defun eide-toolbar-internal-insert-items-for-debug-in-toolbar-buffer ()
  (eide-toolbar-internal-insert-action-text "next" toolbar-gdb-next-map)
  (eide-toolbar-internal-insert-text " | ")
  (eide-toolbar-internal-insert-action-text "step" toolbar-gdb-step-map)
  (eide-toolbar-internal-insert-text " | ")
  (eide-toolbar-internal-insert-action-text "cont" toolbar-gdb-continue-map)
  (eide-toolbar-internal-insert-text " | ")
  (eide-toolbar-internal-insert-action-text "break" toolbar-gdb-setbp-map)
  (eide-toolbar-internal-insert-text " | ")
  (eide-toolbar-internal-insert-action-text "stack" toolbar-gdb-stack-map)
  (eide-toolbar-internal-insert-text " | ")
  (eide-toolbar-internal-insert-action-text "up" toolbar-gdb-stack-up-map)
  (eide-toolbar-internal-insert-text " | ")
  (eide-toolbar-internal-insert-action-text "down" toolbar-gdb-stack-down-map)
  (eide-toolbar-internal-insert-text " | ")
  (eide-toolbar-internal-insert-action-text "threads" toolbar-gdb-threads-map)
  (eide-toolbar-internal-insert-text " | ")
  (eide-toolbar-internal-insert-action-text "breaks" toolbar-gdb-breakpoints-map)
  (eide-toolbar-internal-insert-text "           "))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Init "toolbar" buffer
;; ----------------------------------------------------------------------------
(defun eide-toolbar-init ()
  ;; Toolbar buffer is created empty (content will be built by
  ;; eide-toolbar-update)
  (setq eide-toolbar-buffer (buffer-name (get-buffer-create "* Toolbar *")))
  (save-excursion
    (set-buffer eide-toolbar-buffer)
    (setq buffer-read-only t))
  (setq eide-toolbar-current-tab "edit"))

;; ----------------------------------------------------------------------------
;; Update window "toolbar"
;; ----------------------------------------------------------------------------
(defun eide-toolbar-update ()
  (if eide-custom-use-toolbar
    (let ((my-window (selected-window)))
      ;; Get name of current buffer in window "results"
      (eide-windows-select-window-results t)
      (setq eide-windows-buffer-in-window-results (buffer-name))
      (eide-windows-select-window-toolbar t)
      ;; Set current tab according to this buffer name
      (if (string-equal eide-windows-buffer-in-window-results eide-buffer-compile)
        (setq eide-toolbar-current-tab "compile")
        (if (string-equal eide-windows-buffer-in-window-results eide-buffer-run)
          (setq eide-toolbar-current-tab "run")
          (if (string-equal eide-windows-buffer-in-window-results eide-buffer-debug)
            (setq eide-toolbar-current-tab "debug")
            (if (string-equal eide-windows-buffer-in-window-results eide-buffer-shell)
              (setq eide-toolbar-current-tab "shell")
              ;; Default tab (no active tab)
              (setq eide-toolbar-current-tab "edit")))))
      ;; Update toolbar window size
      (let ((window-size-fixed nil))
        (if (and (string-equal eide-custom-toolbar-position "middle")
                 (string-equal eide-custom-menu-height "full")
                 (string-equal eide-toolbar-current-tab "debug"))
          (enlarge-window (- 3 (window-height)))
          (enlarge-window (- 2 (window-height)))))
      ;; Update toolbar buffer
      (set-buffer eide-toolbar-buffer)
      (let ((buffer-read-only nil))
        (delete-region (point-min) (point-max))
        (eide-toolbar-internal-insert-common-items-in-toolbar-buffer eide-toolbar-current-tab)
        (if (string-equal eide-toolbar-current-tab "debug")
          (eide-toolbar-internal-insert-items-for-debug-in-toolbar-buffer)))
      ;; Move to end of line, so that the cursor doesn't disturb...
      (end-of-line)
      ;; Restore active window
      (select-window my-window))))


;;;; ==========================================================================
;;;; KEYMAPS
;;;; ==========================================================================

(setq toolbar-file-previous-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-file-previous-map [button1] 'eide-toolbar-internal-view-previous-file)
  (define-key toolbar-file-previous-map [mouse-1] 'eide-toolbar-internal-view-previous-file))

(setq toolbar-file-next-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-file-next-map [button1] 'eide-toolbar-internal-view-next-file)
  (define-key toolbar-file-next-map [mouse-1] 'eide-toolbar-internal-view-next-file))

(setq toolbar-compile-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-compile-map [button1] 'eide-toolbar-internal-view-compile-buffer)
  (define-key toolbar-compile-map [mouse-1] 'eide-toolbar-internal-view-compile-buffer)
  (define-key toolbar-compile-map [button3] 'eide-popup-open-menu-for-compile)
  (define-key toolbar-compile-map [mouse-3] 'eide-popup-open-menu-for-compile))

(setq toolbar-run-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-run-map [button1] 'eide-toolbar-internal-view-run-buffer)
  (define-key toolbar-run-map [mouse-1] 'eide-toolbar-internal-view-run-buffer)
  (define-key toolbar-run-map [button3] 'eide-popup-open-menu-for-run)
  (define-key toolbar-run-map [mouse-3] 'eide-popup-open-menu-for-run))

(setq toolbar-debug-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-debug-map [button1] 'eide-toolbar-internal-view-debug-buffer)
  (define-key toolbar-debug-map [mouse-1] 'eide-toolbar-internal-view-debug-buffer)
  (define-key toolbar-debug-map [button3] 'eide-popup-open-menu-for-debug)
  (define-key toolbar-debug-map [mouse-3] 'eide-popup-open-menu-for-debug))

(setq toolbar-shell-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-shell-map [button1] 'eide-toolbar-internal-view-shell-buffer)
  (define-key toolbar-shell-map [mouse-1] 'eide-toolbar-internal-view-shell-buffer)
  (define-key toolbar-shell-map [button3] 'eide-popup-open-menu-for-shell)
  (define-key toolbar-shell-map [mouse-3] 'eide-popup-open-menu-for-shell))

(setq toolbar-gdb-next-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-next-map [button1] 'eide-toolbar-internal-debug-execute-next)
  (define-key toolbar-gdb-next-map [mouse-1] 'eide-toolbar-internal-debug-execute-next))

(setq toolbar-gdb-step-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-step-map [button1] 'eide-toolbar-internal-debug-execute-step)
  (define-key toolbar-gdb-step-map [mouse-1] 'eide-toolbar-internal-debug-execute-step))

(setq toolbar-gdb-continue-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-continue-map [button1] 'eide-toolbar-internal-debug-execute-continue)
  (define-key toolbar-gdb-continue-map [mouse-1] 'eide-toolbar-internal-debug-execute-continue))

(setq toolbar-gdb-setbp-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-setbp-map [button1] 'eide-toolbar-internal-debug-execute-break)
  (define-key toolbar-gdb-setbp-map [mouse-1] 'eide-toolbar-internal-debug-execute-break))

(setq toolbar-gdb-breakpoints-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-breakpoints-map [button1] 'eide-toolbar-internal-debug-execute-info-break)
  (define-key toolbar-gdb-breakpoints-map [mouse-1] 'eide-toolbar-internal-debug-execute-info-break))

(setq toolbar-gdb-stack-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-stack-map [button1] 'eide-toolbar-internal-debug-execute-bt)
  (define-key toolbar-gdb-stack-map [mouse-1] 'eide-toolbar-internal-debug-execute-bt))

(setq toolbar-gdb-stack-up-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-stack-up-map [button1] 'eide-toolbar-internal-debug-execute-up)
  (define-key toolbar-gdb-stack-up-map [mouse-1] 'eide-toolbar-internal-debug-execute-up))

(setq toolbar-gdb-stack-down-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-stack-down-map [button1] 'eide-toolbar-internal-debug-execute-down)
  (define-key toolbar-gdb-stack-down-map [mouse-1] 'eide-toolbar-internal-debug-execute-down))

(setq toolbar-gdb-threads-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key toolbar-gdb-threads-map [button1] 'eide-toolbar-internal-debug-execute-info-threads)
  (define-key toolbar-gdb-threads-map [mouse-1] 'eide-toolbar-internal-debug-execute-info-threads))

;;; eide-toolbar.el ends here
