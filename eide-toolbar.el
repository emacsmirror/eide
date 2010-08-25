;;; eide-toolbar.el --- Emacs-IDE, toolbar

;; Copyright (C) 2005-2009 Cédric Marie

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

(defvar eide-toolbar-buffer-name nil)
(defvar eide-toolbar-current-tab nil)


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Insert text in "toolbar" buffer (with specific background).
;;
;; input  : p-string : string to insert.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-insert-text (p-string)
  (put-text-property (point) (progn (insert p-string) (point)) 'face 'eide-config-toolbar-separator-face))

;; ----------------------------------------------------------------------------
;; Display previous file (in window "file").
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-view-previous-file ()
  (interactive)
  (eide-windows-skip-unwanted-buffers-in-window-file t nil))

;; ----------------------------------------------------------------------------
;; Display next file (in window "file").
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-view-next-file ()
  (interactive)
  (eide-windows-skip-unwanted-buffers-in-window-file t t))

;; ----------------------------------------------------------------------------
;; Display a buffer in window "results".
;;
;; input  : p-buffer-name : buffer name.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-view-buffer-in-window-results (p-buffer-name)
  (if p-buffer-name
    (progn
      (eide-windows-select-window-results)
      (switch-to-buffer p-buffer-name))
    (message "This buffer has not been created yet...")))

;; ----------------------------------------------------------------------------
;; Display compile buffer in window "results".
;;
;; input  : eide-compile-buffer : compile buffer name.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-view-compile-buffer ()
  (interactive)
  (eide-l-toolbar-view-buffer-in-window-results eide-compile-buffer))

;; ----------------------------------------------------------------------------
;; Display run buffer in window "results".
;;
;; input  : eide-run-buffer : run buffer name.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-view-run-buffer ()
  (interactive)
  (eide-l-toolbar-view-buffer-in-window-results eide-run-buffer))

;; ----------------------------------------------------------------------------
;; Display debug buffer in window "results".
;;
;; input  : eide-debug-buffer : debug buffer name.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-view-debug-buffer ()
  (interactive)
  (eide-l-toolbar-view-buffer-in-window-results eide-debug-buffer))

;; ----------------------------------------------------------------------------
;; Display shell buffer in window "results".
;;
;; input  : eide-shell-buffer : shell buffer name.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-view-shell-buffer ()
  (interactive)
  (eide-l-toolbar-view-buffer-in-window-results eide-shell-buffer))

;; ----------------------------------------------------------------------------
;; Send a command string to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-send-string-to-debug-buffer (p-string)
  (eide-windows-select-window-results)
  (process-send-string nil (concat "echo " p-string "\n"))
  ;;(process-send-string nil (concat "info source\n"))
  (process-send-string nil (concat p-string "\n")))

;; ----------------------------------------------------------------------------
;; Send "next" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-next ()
  (interactive)
  ;;(gud-next "next"))
  (eide-l-toolbar-send-string-to-debug-buffer "n"))

;; ----------------------------------------------------------------------------
;; Send "step" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-step ()
  (interactive)
  ;;(gud-step "step 1"))
  (eide-l-toolbar-send-string-to-debug-buffer "s"))

;; ----------------------------------------------------------------------------
;; Send "continue" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-continue ()
  (interactive)
  ;;(gud-cont "cont"))
  (eide-l-toolbar-send-string-to-debug-buffer "c"))

;; ----------------------------------------------------------------------------
;; Send "break" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-break ()
  (interactive)
  ;; Get full file name (the same file name may be used in different
  ;; directories, and gdb chooses the first one !)
  (eide-windows-select-window-file t)
  ;;(gud-break (concat "\"" (buffer-file-name) ":" (number-to-string (count-lines (point-min) (point))) "\"")))
  (eide-l-toolbar-send-string-to-debug-buffer (concat "break " (buffer-file-name) ":" (number-to-string (count-lines (point-min) (point))))))

;; ----------------------------------------------------------------------------
;; Send "info break" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-info-break ()
  (interactive)
  (eide-l-toolbar-send-string-to-debug-buffer "info break"))

;; ----------------------------------------------------------------------------
;; Send "bt" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-bt ()
  (interactive)
  (eide-l-toolbar-send-string-to-debug-buffer "bt"))

;; ----------------------------------------------------------------------------
;; Send "up" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-up ()
  (interactive)
  (eide-l-toolbar-send-string-to-debug-buffer "up"))

;; ----------------------------------------------------------------------------
;; Send "down" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-down ()
  (interactive)
  (eide-l-toolbar-send-string-to-debug-buffer "down"))

;; ----------------------------------------------------------------------------
;; Send "info threads" command to debug buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-debug-execute-info-threads ()
  (interactive)
  (eide-l-toolbar-send-string-to-debug-buffer "info threads"))

;; ----------------------------------------------------------------------------
;; Insert tab name in "toolbar" buffer.
;;
;; input  : p-tab-name : tab name.
;;          p-map : keymap property for tab.
;;          p-real-buffer : related buffer (nil if not yet created).
;;          eide-toolbar-current-tab : current tab name (to highlight).
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-insert-tab-in-toolbar-buffer (p-tab-name p-map p-real-buffer)
  (put-text-property (setq l-begin-point (point)) (progn (insert p-tab-name) (point)) 'keymap p-map)
  (if p-real-buffer
    (if (string-equal p-tab-name eide-toolbar-current-tab)
      (put-text-property l-begin-point (point) 'face 'eide-config-toolbar-current-tab-face)
      (put-text-property l-begin-point (point) 'face 'eide-config-toolbar-enabled-tab-face))
    (put-text-property l-begin-point (point) 'face 'eide-config-toolbar-disabled-tab-face))
  (put-text-property l-begin-point (point) 'mouse-face 'highlight))

;; ----------------------------------------------------------------------------
;; Insert action in "toolbar" buffer.
;;
;; input  : p-string : action string.
;;          p-map : keymap property for action.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-insert-action-text (p-string p-map)
  (put-text-property (setq l-begin-point (point)) (progn (insert p-string) (point)) 'keymap p-map)
  (put-text-property l-begin-point (point) 'face 'eide-config-toolbar-action-face)
  (put-text-property l-begin-point (point) 'mouse-face 'highlight))

;; ----------------------------------------------------------------------------
;; Insert common actions in "toolbar" buffer.
;;
;; input  : eide-project-name : project name.
;;          eide-compile-buffer : compile buffer.
;;          eide-run-buffer : run buffer.
;;          eide-debug-buffer : debug buffer.
;;          eide-shell-buffer : shell buffer.
;;          eide-config-toolbar-position : toolbar position (windows layout).
;;          eide-config-menu-height : menu height (windows layout).
;;          eide-toolbar-current-tab : current tab name.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-insert-common-items-in-toolbar-buffer ()
  (eide-l-toolbar-insert-text " ")

  (eide-l-toolbar-insert-action-text " <<" toolbar-file-previous-map)
  (eide-l-toolbar-insert-text " ")
  (eide-l-toolbar-insert-action-text ">> " toolbar-file-next-map)
  (eide-l-toolbar-insert-text " - ")

  (if eide-project-name
    (progn
      (eide-l-toolbar-insert-tab-in-toolbar-buffer "compile" toolbar-compile-map eide-compile-buffer)
      (eide-l-toolbar-insert-text " / ")
      (eide-l-toolbar-insert-tab-in-toolbar-buffer "run" toolbar-run-map eide-run-buffer)
      (eide-l-toolbar-insert-text " / ")
      (eide-l-toolbar-insert-tab-in-toolbar-buffer "debug" toolbar-debug-map eide-debug-buffer)
      (eide-l-toolbar-insert-text " / ")))
  (eide-l-toolbar-insert-tab-in-toolbar-buffer "shell" toolbar-shell-map eide-shell-buffer)
  (if (and (string-equal eide-config-toolbar-position "middle")
           (string-equal eide-config-menu-height "full")
           (string-equal eide-toolbar-current-tab "debug"))
    ;; Debug commands on 2nd line
    (eide-l-toolbar-insert-text "\n           ")
    (if (string-equal eide-toolbar-current-tab "debug")
      (eide-l-toolbar-insert-text " - ")
      (eide-l-toolbar-insert-text "   "))))

;; ----------------------------------------------------------------------------
;; Insert "debug" actions in "toolbar" buffer.
;; ----------------------------------------------------------------------------
(defun eide-l-toolbar-insert-items-for-debug-in-toolbar-buffer ()
  (eide-l-toolbar-insert-action-text "next" toolbar-gdb-next-map)
  (eide-l-toolbar-insert-text " | ")
  (eide-l-toolbar-insert-action-text "step" toolbar-gdb-step-map)
  (eide-l-toolbar-insert-text " | ")
  (eide-l-toolbar-insert-action-text "cont" toolbar-gdb-continue-map)
  (eide-l-toolbar-insert-text " | ")
  (eide-l-toolbar-insert-action-text "break" toolbar-gdb-setbp-map)
  (eide-l-toolbar-insert-text " | ")
  (eide-l-toolbar-insert-action-text "stack" toolbar-gdb-stack-map)
  (eide-l-toolbar-insert-text " | ")
  (eide-l-toolbar-insert-action-text "up" toolbar-gdb-stack-up-map)
  (eide-l-toolbar-insert-text " | ")
  (eide-l-toolbar-insert-action-text "down" toolbar-gdb-stack-down-map)
  (eide-l-toolbar-insert-text " | ")
  (eide-l-toolbar-insert-action-text "threads" toolbar-gdb-threads-map)
  (eide-l-toolbar-insert-text " | ")
  (eide-l-toolbar-insert-action-text "breaks" toolbar-gdb-breakpoints-map)
  (eide-l-toolbar-insert-text "   "))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Initialize "toolbar" buffer.
;; ----------------------------------------------------------------------------
(defun eide-toolbar-init ()
  ;; Toolbar buffer is created empty (content will be built by
  ;; eide-toolbar-update)
  (setq eide-toolbar-buffer-name (buffer-name (get-buffer-create "* Toolbar *")))
  (save-excursion
    (set-buffer eide-toolbar-buffer-name)
    ;; Truncate lines to keep toolbar on a single line
    (setq truncate-lines t)
    (setq buffer-read-only t))
  (setq eide-toolbar-current-tab "edit"))

;; ----------------------------------------------------------------------------
;; Update "toolbar" buffer.
;;
;; input  : eide-config-use-toolbar-flag : toolbar activation (windows layout).
;;          eide-compile-buffer : compile buffer.
;;          eide-run-buffer : run buffer.
;;          eide-debug-buffer : debug buffer.
;;          eide-shell-buffer : shell buffer.
;;          eide-config-toolbar-position : toolbar position (windows layout).
;;          eide-config-menu-height : menu height (windows layout).
;; output : eide-toolbar-current-tab : current tab name.
;; ----------------------------------------------------------------------------
(defun eide-toolbar-update ()
  (if (and eide-windows-is-layout-visible-flag eide-config-use-toolbar-flag)
    (let ((l-window (selected-window)))
      ;; Get name of current buffer in window "results"
      (eide-windows-select-window-results)
      (setq eide-windows-buffer-in-window-results (buffer-name))
      (eide-windows-select-window-toolbar)
      ;; Set current tab according to this buffer name
      (if (string-equal eide-windows-buffer-in-window-results eide-compile-buffer)
        (setq eide-toolbar-current-tab "compile")
        (if (string-equal eide-windows-buffer-in-window-results eide-run-buffer)
          (setq eide-toolbar-current-tab "run")
          (if (string-equal eide-windows-buffer-in-window-results eide-debug-buffer)
            (setq eide-toolbar-current-tab "debug")
            (if (string-equal eide-windows-buffer-in-window-results eide-shell-buffer)
              (setq eide-toolbar-current-tab "shell")
              ;; Default tab (no active tab)
              (setq eide-toolbar-current-tab "edit")))))
      ;; Update toolbar window size
      (let ((window-size-fixed nil))
        (if (and (string-equal eide-config-toolbar-position "middle")
                 (string-equal eide-config-menu-height "full")
                 (string-equal eide-toolbar-current-tab "debug"))
          (enlarge-window (- 3 (window-height)))
          (enlarge-window (- 2 (window-height)))))
      ;; Update toolbar buffer
      (set-buffer eide-toolbar-buffer-name)
      (let ((buffer-read-only nil))
        (delete-region (point-min) (point-max))
        (eide-l-toolbar-insert-common-items-in-toolbar-buffer)
        (if (string-equal eide-toolbar-current-tab "debug")
          (eide-l-toolbar-insert-items-for-debug-in-toolbar-buffer))
        ;; 500 spaces, so that window "toolbar" seems to have specific background
        (setq l-loop-count 0)
        (save-excursion
          (while (< l-loop-count 500)
            (eide-l-toolbar-insert-text " ")
            (setq l-loop-count (+ l-loop-count 1)))))
      ;; Restore active window
      (select-window l-window))))


;;;; ==========================================================================
;;;; KEYMAPS
;;;; ==========================================================================

(setq toolbar-file-previous-map (make-sparse-keymap))
(define-key toolbar-file-previous-map [mouse-1] 'eide-l-toolbar-view-previous-file)

(setq toolbar-file-next-map (make-sparse-keymap))
(define-key toolbar-file-next-map [mouse-1] 'eide-l-toolbar-view-next-file)

(setq toolbar-compile-map (make-sparse-keymap))
(define-key toolbar-compile-map [mouse-1] 'eide-l-toolbar-view-compile-buffer)
(define-key toolbar-compile-map [mouse-3] 'eide-popup-open-menu-for-compile)

(setq toolbar-run-map (make-sparse-keymap))
(define-key toolbar-run-map [mouse-1] 'eide-l-toolbar-view-run-buffer)
(define-key toolbar-run-map [mouse-3] 'eide-popup-open-menu-for-run)

(setq toolbar-debug-map (make-sparse-keymap))
(define-key toolbar-debug-map [mouse-1] 'eide-l-toolbar-view-debug-buffer)
(define-key toolbar-debug-map [mouse-3] 'eide-popup-open-menu-for-debug)

(setq toolbar-shell-map (make-sparse-keymap))
(define-key toolbar-shell-map [mouse-1] 'eide-l-toolbar-view-shell-buffer)
(define-key toolbar-shell-map [mouse-3] 'eide-popup-open-menu-for-shell)

(setq toolbar-gdb-next-map (make-sparse-keymap))
(define-key toolbar-gdb-next-map [mouse-1] 'eide-l-toolbar-debug-execute-next)

(setq toolbar-gdb-step-map (make-sparse-keymap))
(define-key toolbar-gdb-step-map [mouse-1] 'eide-l-toolbar-debug-execute-step)

(setq toolbar-gdb-continue-map (make-sparse-keymap))
(define-key toolbar-gdb-continue-map [mouse-1] 'eide-l-toolbar-debug-execute-continue)

(setq toolbar-gdb-setbp-map (make-sparse-keymap))
(define-key toolbar-gdb-setbp-map [mouse-1] 'eide-l-toolbar-debug-execute-break)

(setq toolbar-gdb-breakpoints-map (make-sparse-keymap))
(define-key toolbar-gdb-breakpoints-map [mouse-1] 'eide-l-toolbar-debug-execute-info-break)

(setq toolbar-gdb-stack-map (make-sparse-keymap))
(define-key toolbar-gdb-stack-map [mouse-1] 'eide-l-toolbar-debug-execute-bt)

(setq toolbar-gdb-stack-up-map (make-sparse-keymap))
(define-key toolbar-gdb-stack-up-map [mouse-1] 'eide-l-toolbar-debug-execute-up)

(setq toolbar-gdb-stack-down-map (make-sparse-keymap))
(define-key toolbar-gdb-stack-down-map [mouse-1] 'eide-l-toolbar-debug-execute-down)

(setq toolbar-gdb-threads-map (make-sparse-keymap))
(define-key toolbar-gdb-threads-map [mouse-1] 'eide-l-toolbar-debug-execute-info-threads)

;;; eide-toolbar.el ends here
