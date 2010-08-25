;;; eide-windows.el --- Emacs-IDE, windows

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

(provide 'eide-windows)

(defvar eide-windows-is-layout-visible-flag nil)
(defvar eide-windows-menu-update-request-pending-flag nil)

(defvar eide-windows-buffer-in-window-results nil)
(defvar eide-compile-buffer nil)
(defvar eide-run-buffer nil)
(defvar eide-debug-buffer nil)
(defvar eide-shell-buffer nil)

(defvar eide-windows-update-result-buffer-id nil)


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Test if a buffer should be displayed in window "file".
;;
;; input  : p-buffer-name : buffer name.
;; return : t = yes, nil = no.
;; ----------------------------------------------------------------------------
(defun eide-l-windows-should-buffer-be-displayed-in-window-file-p (p-buffer-name)
  (if (or (string-equal p-buffer-name "TAGS") (string-equal p-buffer-name eide-options-file) (string-equal p-buffer-name eide-project-file) (string-equal p-buffer-name eide-project-notes-file) (string-match "^\*.*" p-buffer-name))
    nil
    t))

;; ----------------------------------------------------------------------------
;; Get the window in which a buffer should be displayed.
;;
;; input  : p-buffer-name : buffer name.
;; return : buffer window (nil if not found).
;; ----------------------------------------------------------------------------
(defun eide-l-windows-get-window-for-buffer (p-buffer-name)
  (if eide-windows-is-layout-visible-flag
    (if (string-match "^\*.*" p-buffer-name)
      (if (string-equal eide-toolbar-buffer-name p-buffer-name)
        eide-windows-window-toolbar
        (if (string-equal eide-menu-buffer-name p-buffer-name)
          eide-windows-window-menu
          eide-windows-window-results))
      (if (string-equal " SPEEDBAR" p-buffer-name)
        nil
        eide-windows-window-file))
      nil))

;; ----------------------------------------------------------------------------
;; Update menu or toolbar if necessary.
;;
;; input  : p-window : window in which a buffer has been displayed.
;; ----------------------------------------------------------------------------
(defun eide-l-windows-update-display (p-window)
  (if (equal p-window eide-windows-window-file)
    (eide-menu-update nil)
    (if (equal p-window eide-windows-window-results)
      (eide-toolbar-update))))

;; ----------------------------------------------------------------------------
;; Display a buffer in appropriate window.
;; Called :
;; - when launching compile, run, debug or shell.
;;
;; input  : p-buffer : buffer.
;;          eide-windows-update-result-buffer-id : ID of result buffer to be
;;              displayed (or nil).
;; output : eide-windows-update-result-buffer-id : nil.
;; return : buffer window.
;; ----------------------------------------------------------------------------
(defun eide-l-windows-display-buffer-function (p-buffer &optional p-not-this-window p-frame)
  (let ((l-buffer-name) (l-window)
        (l-selected-window (selected-window)))
    (if (bufferp p-buffer)
      (setq l-buffer-name (buffer-name p-buffer))
      (setq l-buffer-name p-buffer))
    (eide-debug-print-trace (concat "eide-l-windows-display-buffer-function : " l-buffer-name))
    (setq l-window (eide-l-windows-get-window-for-buffer l-buffer-name))
    (if (not l-window)
      (setq l-window l-selected-window))
    (set-window-buffer l-window p-buffer)
    ;; Result buffer name is updated asynchronously
    (if eide-windows-update-result-buffer-id
      (progn
        (if (string-equal eide-windows-update-result-buffer-id "c")
          (setq eide-compile-buffer l-buffer-name)
          (if (string-equal eide-windows-update-result-buffer-id "r")
            (setq eide-run-buffer l-buffer-name)
            (if (string-equal eide-windows-update-result-buffer-id "d")
              (setq eide-debug-buffer l-buffer-name)
              (if (string-equal eide-windows-update-result-buffer-id "s")
                (setq eide-shell-buffer l-buffer-name)))))
        (setq eide-windows-update-result-buffer-id nil)))
    ;; Update menu or toolbar if necessary
    (eide-l-windows-update-display l-window)
    (if (string-equal l-buffer-name "*Completions*")
      (progn
        (select-window l-window)
        ;; Window "results" temporarily expands to half or 2/3 of the frame to
        ;; display completions
        (let ((l-completion-height (max (+ (count-lines (point-min) (point-max)) 2) (/ (frame-height) 2))))
          (if (> l-completion-height (/ (frame-height) 2))
            (setq l-completion-height (/ (* (frame-height) 2) 3)))
          (enlarge-window (- l-completion-height (window-height))))))
    ;; Restore selected window
    (select-window l-selected-window)
    ;; Return buffer window
    l-window))

;; ----------------------------------------------------------------------------
;; Override switch-to-buffer function (advice), to display buffer in
;; appropriate window.
;; Called :
;; - when switching to compile, run, debug or shell buffer.
;;
;; input  : p-buffer : buffer.
;;          eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-search-find-symbol-definition-flag : t = display update is
;;              necessary after symbol search.
;;          eide-root-directory : project root directory.
;; output : eide-search-find-symbol-definition-flag : display update pending
;;              (nil).
;; return : p-buffer (or current buffer if it didn't switch to p-buffer).
;; ----------------------------------------------------------------------------
(defadvice switch-to-buffer (around eide-switch-to-buffer-advice-around (p-buffer))
  (let ((l-buffer-name) (l-do-it-flag) (l-window))
    (if (bufferp p-buffer)
      (setq l-buffer-name (buffer-name p-buffer))
      (setq l-buffer-name p-buffer))
    (eide-debug-print-trace (concat "switch-to-buffer : " l-buffer-name))
    (save-excursion
      (set-buffer l-buffer-name)
      ;; Do not display "dired mode", TAGS file, and configuration files
      (if (or (equal major-mode 'dired-mode) (string-equal l-buffer-name "TAGS") (string-equal l-buffer-name eide-options-file) (string-equal l-buffer-name eide-project-file) (string-equal l-buffer-name eide-project-notes-file))
        (setq l-do-it-flag nil)
        (setq l-do-it-flag t)))
    (if l-do-it-flag
      (progn
        ;;(eide-debug-print-trace (concat "switch-to-buffer : " l-buffer-name))
        (setq l-window (eide-l-windows-get-window-for-buffer l-buffer-name))
        (if l-window
          (select-window l-window)
          (setq l-window (selected-window)))
        ad-do-it
        (if eide-search-find-symbol-definition-flag
          (progn
            (recenter)
            (setq eide-search-find-symbol-definition-flag nil)))
        ;; Update menu or toolbar if necessary
        (eide-l-windows-update-display l-window)
        ;; Desktop is saved to avoid loss of opened buffers in case of crash of emacs
        (if (file-exists-p (concat eide-root-directory ".emacs.desktop"))
          (desktop-save eide-root-directory))
        ;; Select buffer window
        (select-window l-window)
        ;; Return the buffer that it switched to
        p-buffer)
      (progn
        ;; Close unwanted files (except TAGS and project configuration)
        (if (or (equal major-mode 'dired-mode) (string-equal l-buffer-name eide-options-file) (string-equal l-buffer-name eide-project-notes-file))
          (progn
            ;;(eide-debug-print-trace (concat "switch-to-buffer : discard dired-mode buffer " l-buffer-name))
            (kill-buffer l-buffer-name)
            ;; Return the current buffer
            (current-buffer)))))))

;; ----------------------------------------------------------------------------
;; Override C-x C-f find-file, to get default directory from buffer in window
;; "file".
;; ----------------------------------------------------------------------------
(defun eide-windows-find-file ()
  (interactive)
  (eide-windows-select-window-file nil)
  (call-interactively 'find-file))

;; ----------------------------------------------------------------------------
;; Override save-buffer function (advice), to save buffer in window "file".
;; ----------------------------------------------------------------------------
(defadvice save-buffer (around eide-save-buffer-advice-around)
  (let ((l-window (selected-window)))
    (eide-windows-select-window-file nil)
    ad-do-it
    (eide-menu-update-current-buffer-modified-status)
    (select-window l-window)))

;; ----------------------------------------------------------------------------
;; Hook to be called once the frame has been resized.
;;
;; output : eide-windows-results-window-height : height of window "results".
;;          eide-windows-menu-window-width : width of window "menu".
;; ----------------------------------------------------------------------------
(defun eide-l-windows-window-setup-hook ()
  ;;(setq eide-windows-results-window-height (/ (frame-height) 5))
  ;;(setq eide-windows-menu-window-width (/ (frame-width) 4))

  ;; Close buffer "*Buffer List*" (created when emacs is launched with files as
  ;; parameters)
  (if (string-equal (buffer-name) "*Buffer List*")
    (kill-this-buffer))

  (setq eide-windows-results-window-height 9)
  (setq eide-windows-menu-window-width 40)
  (eide-windows-layout-build)
  (ad-activate 'switch-to-buffer)
  (ad-activate 'save-buffer)
  (eide-windows-skip-unwanted-buffers-in-window-file nil nil)
  ;; Create menu content
  (eide-menu-update t)
  (eide-toolbar-update))

;; ----------------------------------------------------------------------------
;; Select window at mouse position.
;; ----------------------------------------------------------------------------
(defun eide-l-windows-select-window-at-mouse-position ()
  ;; Select the window where the mouse is
  (let ((l-position (last (mouse-position))))
    (select-window (window-at (car l-position) (cdr l-position)))))

;; ----------------------------------------------------------------------------
;; Test if selected window is window "file".
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-l-windows-is-window-file-selected-p ()
  (equal (selected-window) eide-windows-window-file))

;; ----------------------------------------------------------------------------
;; Test if selected window is window "menu".
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-l-windows-is-window-menu-selected-p ()
  (equal (selected-window) eide-windows-window-menu))

;; ----------------------------------------------------------------------------
;; Test if selected window is window "results".
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-l-windows-is-window-results-selected-p ()
  (equal (selected-window) eide-windows-window-results))

;; ----------------------------------------------------------------------------
;; Test if selected window is window "toolbar".
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-l-windows-is-window-toolbar-selected-p ()
  (equal (selected-window) eide-windows-window-toolbar))

;; ----------------------------------------------------------------------------
;; Resize layout : enlarge window "results".
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-windows-results-window-height : height of window "results".
;; output : eide-windows-results-window-height : updated height of window
;;              "results".
;; ----------------------------------------------------------------------------
(defun eide-l-windows-layout-resize-enlarge-window-results ()
  (if (and eide-windows-is-layout-visible-flag
           (< eide-windows-results-window-height (- (frame-height) 8)))
    (progn
      (eide-windows-layout-unbuild)
      (setq eide-windows-results-window-height (1+ eide-windows-results-window-height))
      (eide-windows-layout-build))))

;; ----------------------------------------------------------------------------
;; Resize layout : shrink window "results".
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-windows-results-window-height : height of window "results".
;; output : eide-windows-results-window-height : updated height of window
;;              "results".
;; ----------------------------------------------------------------------------
(defun eide-l-windows-layout-resize-shrink-window-results ()
  (if (and eide-windows-is-layout-visible-flag
           (> eide-windows-results-window-height 3))
    (progn
      (eide-windows-layout-unbuild)
      (setq eide-windows-results-window-height (1- eide-windows-results-window-height))
      (eide-windows-layout-build))))

;; ----------------------------------------------------------------------------
;; Resize layout : enlarge window "menu".
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-windows-menu-window-width : width of window "menu".
;; output : eide-windows-menu-window-width : updated width of window "menu".
;; ----------------------------------------------------------------------------
(defun eide-l-windows-layout-resize-enlarge-window-menu ()
  (if (and eide-windows-is-layout-visible-flag
           (< eide-windows-menu-window-width (- (frame-width) 16)))
    (progn
      (eide-windows-layout-unbuild)
      (setq eide-windows-menu-window-width (1+ eide-windows-menu-window-width))
      (eide-windows-layout-build))))

;; ----------------------------------------------------------------------------
;; Resize layout : shrink window "menu".
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-windows-menu-window-width : width of window "menu".
;; output : eide-windows-menu-window-width : updated width of window "menu".
;; ----------------------------------------------------------------------------
(defun eide-l-windows-layout-resize-shrink-window-menu ()
  (if (and eide-windows-is-layout-visible-flag
           (> eide-windows-menu-window-width 16))
    (progn
      (eide-windows-layout-unbuild)
      (setq eide-windows-menu-window-width (1- eide-windows-menu-window-width))
      (eide-windows-layout-build))))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================


;; ----------------------------------------------------------------------------
;; Initialize windows.
;; ----------------------------------------------------------------------------
(defun eide-windows-init ()
  (add-hook 'window-setup-hook 'eide-l-windows-window-setup-hook))

;; ----------------------------------------------------------------------------
;; Build windows layout.
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-config-use-toolbar-flag : toolbar activation (windows layout).
;;          eide-config-toolbar-position : toolbar position (windows layout).
;;          eide-config-menu-position : menu position (windows layout).
;;          eide-config-menu-height : menu height (windows layout).
;;          eide-windows-menu-window-width : width of window "menu".
;;          eide-windows-results-window-height : height of window "results".
;;          eide-windows-buffer-in-window-results : buffer in window "results".
;;          eide-toolbar-current-tab : current tab name in window "toolbar".
;; output : eide-windows-window-file : window "file".
;;          eide-windows-window-menu : window "menu".
;;          eide-windows-window-results : window "results".
;;          eide-windows-window-toolbar : window "toolbar".
;;          eide-windows-is-layout-visible-flag : t (windows layout is shown).
;; ----------------------------------------------------------------------------
(defun eide-windows-layout-build ()
  (if (not eide-windows-is-layout-visible-flag)
    (progn
      (delete-other-windows)
      ;; Create window "toolbar" now if position is "top" or "bottom"
      (if (and eide-config-use-toolbar-flag (or (string-equal eide-config-toolbar-position "top")
                                                (string-equal eide-config-toolbar-position "bottom")))
        (progn
          (split-window-vertically)
          (if (string-equal eide-config-toolbar-position "bottom")
            (select-window (next-window)))
          (setq eide-windows-window-toolbar (selected-window))
          (select-window (next-window))))

      ;; Split into 3 windows ("file", "menu", "results")
      (if (string-equal eide-config-menu-height "full")
        (progn
          (split-window-horizontally)
          (if (string-equal eide-config-menu-position "left")
            ;; Menu on left side
            (progn
              (setq eide-windows-window-menu (selected-window))
              (select-window (next-window))
              (split-window-vertically)
              (setq eide-windows-window-file (selected-window))
              (select-window (next-window))
              (setq eide-windows-window-results (selected-window)))
            ;; Menu on right side
            (progn
              (split-window-vertically)
              (setq eide-windows-window-file (selected-window))
              (select-window (next-window))
              (setq eide-windows-window-results (selected-window))
              (select-window (next-window))
              (setq eide-windows-window-menu (selected-window)))))
        (progn
          (split-window-vertically)
          (split-window-horizontally)
          (if (string-equal eide-config-menu-position "left")
            ;; Menu on left side
            (progn
              (setq eide-windows-window-menu (selected-window))
              (select-window (next-window))
              (setq eide-windows-window-file (selected-window))
              (select-window (next-window))
              (setq eide-windows-window-results (selected-window)))
            ;; Menu on right side
            (progn
              (setq eide-windows-window-file (selected-window))
              (select-window (next-window))
              (setq eide-windows-window-menu (selected-window))
              (select-window (next-window))
              (setq eide-windows-window-results (selected-window))))))

      ;; Create window "toolbar" now if position is "middle"
      ;; (Split window "results" into 2 windows ("toolbar" and "results"))
      (if (and eide-config-use-toolbar-flag (string-equal eide-config-toolbar-position "middle"))
        (progn
          (select-window eide-windows-window-results)
          (setq eide-windows-window-toolbar (selected-window))
          ;; +2 for window "toolbar"
          (enlarge-window (+ 2 (- eide-windows-results-window-height (window-height))))
          ;; split window "results" to create window "toolbar"
          (split-window-vertically)
          (select-window (next-window))
          (setq eide-windows-window-results (selected-window))))

      ;; Window "menu"
      (select-window eide-windows-window-menu)
      (switch-to-buffer eide-menu-buffer-name)
      ;; This window should be used for this buffer only
      (set-window-dedicated-p eide-windows-window-menu t)
      ;;(setq window-min-width 1) ; TODO : sans effet ?
      (enlarge-window-horizontally (- eide-windows-menu-window-width (window-width)))

      ;; Window "toolbar"
      (if eide-config-use-toolbar-flag
        (progn
          (select-window eide-windows-window-toolbar)
          (switch-to-buffer eide-toolbar-buffer-name)
          ;; This window should be used for this buffer only
          (set-window-dedicated-p eide-windows-window-toolbar t)
          ;; Resize window "toolbar"
          (setq window-min-height 1)
          (if (and (string-equal eide-config-toolbar-position "middle")
                   (string-equal eide-config-menu-height "full")
                   (string-equal eide-toolbar-current-tab "debug"))
            (enlarge-window (- 3 (window-height)))
            (enlarge-window (- 2 (window-height))))
          ;; Do not enable resizing of this window
          (setq window-size-fixed t)))

      ;; Window "results" (may be split into "toolbar" and "results")
      (select-window eide-windows-window-results)
      (setq window-min-height 2)
      (enlarge-window (- eide-windows-results-window-height (window-height)))
      (switch-to-buffer (get-buffer-create "*results*"))
      (if eide-windows-buffer-in-window-results
        (switch-to-buffer eide-windows-buffer-in-window-results)
        (setq eide-windows-buffer-in-window-results "*results*"))

      (select-window eide-windows-window-file)

      (setq eide-windows-is-layout-visible-flag t)
      (setq display-buffer-function 'eide-l-windows-display-buffer-function))))

;; ----------------------------------------------------------------------------
;; Unbuild windows layout (keep only window "file").
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-config-use-toolbar-flag : toolbar activation (windows layout).
;; output : eide-windows-menu-window-width : width of window "menu".
;;          eide-windows-results-window-height : height of window "results".
;;          eide-windows-buffer-in-window-results : buffer in window "results".
;;          eide-windows-is-layout-visible-flag : nil (windows layout is
;;              hidden).
;; ----------------------------------------------------------------------------
(defun eide-windows-layout-unbuild ()
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Remember window "menu" width
      (eide-windows-select-window-menu)
      (setq eide-windows-menu-window-width (window-width))
      ;; Remember window "results" height
      (eide-windows-select-window-results)
      (setq eide-windows-results-window-height (window-height))
      ;; Remember which result buffer is displayed in window "results"
      (setq eide-windows-buffer-in-window-results (buffer-name))
      (if eide-config-use-toolbar-flag
        (progn
          ;; Enable resizing of window "toolbar", otherwise it can't be deleted
          (eide-windows-select-window-toolbar)
          (setq window-size-fixed nil)))
      ;; Keep only window "file"
      (eide-windows-select-window-file t)
      (delete-other-windows)

      (setq eide-windows-is-layout-visible-flag nil)
      (setq display-buffer-function nil))))

;; ----------------------------------------------------------------------------
;; Select window "file".
;;
;; input  : p-force-build-flag : t = build windows layout if not visible.
;;          eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-select-window-file (p-force-build-flag)
  (if (or eide-windows-is-layout-visible-flag p-force-build-flag)
    (progn
      (if (not eide-windows-is-layout-visible-flag)
        (eide-windows-layout-build))
      (select-window eide-windows-window-file))))

;; ----------------------------------------------------------------------------
;; Select window "menu" (build windows layout if necessary).
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-select-window-menu ()
  (if (not eide-windows-is-layout-visible-flag)
    (eide-windows-layout-build))
  (select-window eide-windows-window-menu))

;; ----------------------------------------------------------------------------
;; Select window "results" (build windows layout if necessary).
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-select-window-results ()
  (if (not eide-windows-is-layout-visible-flag)
    (eide-windows-layout-build))
  (select-window eide-windows-window-results))

;; ----------------------------------------------------------------------------
;; Select window "toolbar" (build windows layout if necessary).
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-select-window-toolbar ()
  (if (not eide-windows-is-layout-visible-flag)
    (eide-windows-layout-build))
  (select-window eide-windows-window-toolbar))

;; ----------------------------------------------------------------------------
;; Parse buffers list until an appropriate buffer is found, that can be
;; displayed.
;;
;; input  : p-skip-current : t = force to skip current buffer, even if correct.
;;          p-go-forward : t = go forward, nil = go backward.
;; ----------------------------------------------------------------------------
(defun eide-windows-skip-unwanted-buffers-in-window-file (p-skip-current p-go-forward)
  (eide-windows-select-window-file nil)
  (let ((l-should-we-continue t) (l-current-buffer-name (buffer-name)) (l-iteration 0))
    ;; Temporarily disable switch-to-buffer advice : buffers must be displayed
    ;; in window "file", until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    ;;(eide-debug-print-trace (concat "l-current-buffer-name = " l-current-buffer-name))
    (if p-skip-current
      (progn
        (if p-go-forward
          (bs-cycle-previous)
          (bs-cycle-next))
        ;; To avoid list of buffers in minibuffer
        (message "")
        ;;(eide-debug-print-trace (concat "bs-cycle-xxx => " (buffer-name)))
        (setq l-first-found-buffer-name (buffer-name)))
      (setq l-first-found-buffer-name nil))
    (while (and (not (eide-l-windows-should-buffer-be-displayed-in-window-file-p (buffer-name)))
                l-should-we-continue
                (< l-iteration 10))
      (progn
        (if p-go-forward
          (bs-cycle-previous)
          (bs-cycle-next))
        ;; To avoid list of buffers in minibuffer
        (message "")
        ;;(eide-debug-print-trace (concat "bs-cycle-xxx => " (buffer-name)))
        (if (and (= l-iteration 0) (not l-first-found-buffer-name))
          (setq l-first-found-buffer-name (buffer-name))
          (if (string-equal (buffer-name) l-first-found-buffer-name)
            ;; We have parsed the whole buffer list without finding any other
            ;; buffer that fits. Moreover, current buffer cannot be found again
            ;; (because bs-cycle-xxx ignores temporary buffers), which means
            ;; that it is not valid either. Let's display "*scratch*".
            (progn
              ;;(eide-debug-print-trace (concat "back to first found buffer = " (buffer-name)))
              (switch-to-buffer "*scratch*")
              ;;(eide-debug-print-trace (concat "force buffer *scratch* => " (buffer-name)))
              )))
        (if (string-equal (buffer-name) l-current-buffer-name)
          (progn
            ;; We have parsed the whole buffer list without finding any other
            ;; buffer that fits. If this buffer is valid, let's keep it
            ;; current. Otherwise, let's display "*scratch*".
            ;;(eide-debug-print-trace (concat "back to current buffer = " (buffer-name)))
            (setq l-should-we-continue nil)
            (if (not (eide-l-windows-should-buffer-be-displayed-in-window-file-p (buffer-name)))
              (progn
                (switch-to-buffer "*scratch*")
                ;;(eide-debug-print-trace (concat "force buffer *scratch* => " (buffer-name)))
                ))))
        (setq l-iteration (1+ l-iteration))))
    (ad-activate 'switch-to-buffer)
    ;; Update menu (switch-to-buffer advice was disabled)
    (eide-menu-update nil)))

;; ----------------------------------------------------------------------------
;; Handle mouse-3 (right click) action.
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-windows-menu-update-request-pending-flag : t = menu buffer
;;              update is necessary.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-mouse-3 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-l-windows-select-window-at-mouse-position)

  (if (string-equal (buffer-name) "*Colors*")
    (progn
      ;; Close colors buffer and window
      (kill-this-buffer)
      (select-window (next-window))
      (if (string-equal (buffer-name) eide-options-file)
        (delete-other-windows))))
  (if (string-equal (buffer-name) "* Help *")
    ;; Close "help"
    (progn
      (kill-buffer "* Help *")
      (eide-config-set-colors-for-files)
      (eide-keys-configure-for-editor)
      (eide-windows-layout-build))
    (if (string-equal (buffer-name) eide-options-file)
      ;; Close ".emacs-ide.options"
      (progn
        (save-buffer)
        (eide-config-rebuild-options-file)
        (eide-config-set-colors-for-files)
        (eide-keys-configure-for-editor)
        (eide-windows-layout-build)
        ;; Toolbar might have been modified in options
        (eide-toolbar-update)
        ;; Close colors buffer if opened
        (if (get-buffer "*Colors*")
          (kill-buffer "*Colors*")))
      (if (string-equal (buffer-name) eide-project-file)
        ;; Display another buffer (other than ".emacs-ide.project")
        (progn
          (save-buffer)
          (eide-config-rebuild-project-file)
          ;; This buffer must not be closed
          (switch-to-buffer eide-current-buffer)
          (eide-config-set-colors-for-files)
          (eide-keys-configure-for-editor)
          (eide-windows-layout-build))
        (if (string-equal (buffer-name) eide-project-notes-file)
          ;; Close ".emacs-ide.project_notes"
          (progn
            (save-buffer)
            (kill-buffer eide-project-notes-file)
            (eide-config-set-colors-for-files)
            (eide-keys-configure-for-editor)
            (eide-windows-layout-build))
          (progn
            (if (eq mark-active t)
              ;; Text is selected
              (if (= (count-screen-lines (region-beginning) (region-end) t) 1)
                ;; Text is selected on a single line
                (eide-popup-open-menu-for-search)
                ;; Text is selected on several lines
                (eide-popup-open-menu-for-cleaning))
              ;; No text selected
              (progn
                (if (eide-l-windows-is-window-results-selected-p)
                  ;; Window "results" : open grep results popup menu
                  (eide-popup-open-menu-for-search-results)
                  (if (eide-l-windows-is-window-menu-selected-p)
                    ;; Window "menu" : open project popup menu
                    (eide-popup-open-menu)
                    ;; Window "file"
                    (if eide-windows-is-layout-visible-flag
                      ;; Hide
                      (eide-windows-layout-unbuild)
                      ;; Show
                      (progn
                        ;; Build windows layout
                        (eide-windows-layout-build)
                        ;; Update menu if necessary
                        (if eide-windows-menu-update-request-pending-flag
                          (eide-menu-update t))
                        (eide-windows-select-window-file t)))))))))))))

;; ----------------------------------------------------------------------------
;; Handle mouse-2 (middle click) action.
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-mouse-2 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-l-windows-select-window-at-mouse-position)

  (if (and eide-windows-is-layout-visible-flag (eide-l-windows-is-window-menu-selected-p))
    (eide-menu-speedbar-open)
    (yank)))

;; ----------------------------------------------------------------------------
;; Handle control + mouse-4 (wheel up) action.
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-control-mouse-4 ()
  (interactive)
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Select the window where the mouse is
      (eide-l-windows-select-window-at-mouse-position)
      (if (eide-l-windows-is-window-menu-selected-p)
        (eide-l-windows-layout-resize-enlarge-window-menu)
        (eide-l-windows-layout-resize-enlarge-window-results)))))

;; ----------------------------------------------------------------------------
;; Handle control + mouse-5 (wheel down) action.
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-control-mouse-5 ()
  (interactive)
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Select the window where the mouse is
      (eide-l-windows-select-window-at-mouse-position)
      (if (eide-l-windows-is-window-menu-selected-p)
        (eide-l-windows-layout-resize-shrink-window-menu)
        (eide-l-windows-layout-resize-shrink-window-results)))))

;; ----------------------------------------------------------------------------
;; Handle shift + mouse-3 (right click) action.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-shift-mouse-3 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-l-windows-select-window-at-mouse-position)

  (if (eide-l-windows-is-window-results-selected-p)
    ;; In window "results", open popup menu to delete search results
    (eide-popup-open-menu-for-search-results-delete)
    ;; In options, show/hide list of colors
    (if (string-equal (buffer-name) eide-options-file)
      (if (get-buffer "*Colors*")
        ;; Close colors buffer and window
        (progn
          (delete-other-windows)
          (kill-buffer "*Colors*"))
        ;; Display colors in another window
        (list-colors-display))
      (if (string-equal (buffer-name) "*Colors*")
        ;; Close colors buffer and window
        (progn
          (kill-this-buffer)
          (select-window (next-window))
          (if (string-equal (buffer-name) eide-options-file)
            (delete-other-windows)))))))

;;; eide-windows.el ends here
