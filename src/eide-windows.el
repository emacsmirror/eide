;;; eide-windows.el --- Emacs-IDE, windows

;; Copyright (C) 2008-2011 Cédric Marie

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

(provide 'eide-windows)

(require 'eide-menu) ; for eide-menu-buffer-name, eide-menu-browsing-mode-flag, eide-menu-browsing-mode-start, eide-menu-browsing-mode-stop, eide-menu-update, eide-menu-build-files-lists, eide-menu-update-current-buffer-modified-status, and eide-menu-dired-open

(defvar eide-windows-source-window nil)
(defvar eide-windows-menu-window nil)
(defvar eide-windows-output-window nil)
(defvar eide-windows-window-completion nil)

(defvar eide-windows-is-layout-visible-flag nil)
(defvar eide-windows-menu-update-request-pending-flag nil)
(defvar eide-windows-menu-update-request-pending-force-rebuild-flag nil)
(defvar eide-windows-menu-update-request-pending-force-update-status-flag nil)

(defvar eide-windows-output-window-buffer nil)
(defvar eide-compilation-buffer nil)
(defvar eide-execution-buffer nil)
(defvar eide-shell-buffer nil)

(defvar eide-windows-output-window-height nil)
(defvar eide-windows-menu-window-width nil)

(defvar eide-windows-update-output-buffer-id nil)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Get the window in which a buffer should be displayed.
;;
;; input  : p-buffer-name : buffer name.
;; return : buffer window (nil if not found).
;; ----------------------------------------------------------------------------
(defun eide-i-windows-get-window-for-buffer (p-buffer-name)
  (if eide-keys-is-editor-configuration-active-flag
    (if (string-match "^\*.*" p-buffer-name)
      (if eide-windows-is-layout-visible-flag
        (if (string-equal eide-menu-buffer-name p-buffer-name)
          eide-windows-menu-window
          ;; Let WoMan display in a frame
          (if (string-match "^\*WoMan.*" p-buffer-name)
            nil
            eide-windows-output-window))
        ;; Layout is not built => "menu" and "output" windows don't exist
        nil)
      (save-excursion
        (set-buffer p-buffer-name)
        (if (or (equal major-mode 'dired-mode)
                (equal major-mode 'Buffer-menu-mode))
          nil
          (if (or (string-equal "TAGS" p-buffer-name)
                  (string-equal eide-config-file p-buffer-name)
                  (string-equal eide-project-config-file p-buffer-name)
                  (string-equal eide-project-notes-file p-buffer-name))
            nil
            eide-windows-source-window))))
    nil))

;; ----------------------------------------------------------------------------
;; Display a buffer in appropriate window.
;; Called for:
;; - compile, run, and shell buffers
;; - man pages
;;
;; input  : p-buffer : buffer.
;;          eide-windows-update-output-buffer-id : ID of result buffer to be
;;              displayed (or nil).
;; output : eide-windows-update-output-buffer-id : nil.
;; return : buffer window.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-display-buffer-function (p-buffer &optional p-not-this-window p-frame)
  (let ((l-buffer-name) (l-window)
        (l-selected-window (selected-window))
        (l-browsing-mode-flag nil))
    (if (bufferp p-buffer)
      (setq l-buffer-name (buffer-name p-buffer))
      (setq l-buffer-name p-buffer))
    ;;(message (concat "eide-i-windows-display-buffer-function: " l-buffer-name))
    (save-excursion
      (set-buffer l-buffer-name)
      (if (or (equal major-mode 'dired-mode)
              (equal major-mode 'Buffer-menu-mode))
        (setq l-browsing-mode-flag t)))
    (if l-browsing-mode-flag
      (progn
        (if (not eide-menu-browsing-mode-flag)
          (eide-menu-browsing-mode-start))
        (setq l-window l-selected-window)
        (set-window-buffer l-window p-buffer))
      (progn
        (if (and (not eide-windows-is-layout-visible-flag)
                 (string-equal l-buffer-name "*Completions*"))
          (progn
            (setq l-window (get-buffer-window l-buffer-name))
            (if (not l-window)
              ;; When clicking on directories, completion buffer is closed,
              ;; but its window is not closed: we must use it
              (if (window-live-p eide-windows-window-completion)
                (setq l-window eide-windows-window-completion)
                (progn
                  (select-window eide-windows-source-window)
                  (split-window-vertically)
                  (setq l-window (next-window))
                  (setq eide-windows-window-completion l-window)))))
          (progn
            (setq l-window (eide-i-windows-get-window-for-buffer l-buffer-name))
            (if (not l-window)
              (setq l-window l-selected-window))))
        (set-window-buffer l-window p-buffer)
        ;; Result buffer name is updated asynchronously
        (if eide-windows-update-output-buffer-id
          (progn
            (if (string-equal eide-windows-update-output-buffer-id "c")
              (setq eide-compilation-buffer l-buffer-name)
              (if (string-equal eide-windows-update-output-buffer-id "r")
                (setq eide-execution-buffer l-buffer-name)
                (if (string-equal eide-windows-update-output-buffer-id "s")
                  (setq eide-shell-buffer l-buffer-name))))
            (setq eide-windows-update-output-buffer-id nil)))
        (if (equal l-window eide-windows-source-window)
          (progn
            (if (and eide-menu-browsing-mode-flag
                     (not (equal major-mode 'dired-mode))
                     (not (equal major-mode 'Buffer-menu-mode)))
              (eide-menu-browsing-mode-stop))
            ;; Update menu if necessary
            (eide-menu-update nil)))
        (if (string-equal l-buffer-name "*Completions*")
          (progn
            (select-window l-window)
            ;; "Output" window temporarily expands to half or 2/3 of the frame to
            ;; display completions
            (let ((l-completion-height (max (+ (count-lines (point-min) (point-max)) 2) (/ (frame-height) 2))))
              (if (> l-completion-height (/ (frame-height) 2))
                (setq l-completion-height (/ (* (frame-height) 2) 3)))
              (enlarge-window (- l-completion-height (window-height))))))
        (if (string-match "^\*Man .*" l-buffer-name)
          (eide-menu-build-files-lists))))
    ;; Restore selected window
    (select-window l-selected-window)
    ;; Return buffer window
    l-window))

;; ----------------------------------------------------------------------------
;; Override select-window function (advice), to know which window is the active
;; "source" window.
;;
;; input  : p-window : window.
;;          p-norecord : norecord flag
;; output : eide-windows-source-window : updated "source" window.
;; ----------------------------------------------------------------------------
(defadvice select-window (after eide-select-window-advice-around (p-window &optional p-norecord))
  (if (not (or (equal p-window eide-windows-source-window)
               (equal p-window eide-windows-menu-window)
               (equal p-window eide-windows-output-window)
               ;; Exclude minibuffer
               (window-minibuffer-p p-window)
               ;; Exclude any temporary buffer ("*...")
               (string-match "^\*.*" (buffer-name (window-buffer p-window)))))
    (progn
      (ad-deactivate 'select-window)
      (setq eide-windows-source-window p-window)
      (eide-menu-update nil)
      (ad-activate 'select-window))))

;; ----------------------------------------------------------------------------
;; Override switch-to-buffer function (advice), to display buffer in
;; appropriate window.
;; Called:
;; - when switching to compile, run or shell buffer.
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
  (let ((l-buffer-name) (l-do-it-flag t) (l-browsing-mode-flag nil) (l-window))
    (if (bufferp p-buffer)
      ;; Get buffer name from buffer
      (setq l-buffer-name (buffer-name p-buffer))
      ;; p-buffer is already a buffer name
      (setq l-buffer-name p-buffer))
    (if l-buffer-name
      ;; l-buffer-name = nil if p-buffer has been killed
      ;; I have to find out how this is possible...
      ;; It happens when opening multiple files with *
      (progn
        ;;(message (concat "switch-to-buffer: " l-buffer-name))
        (save-excursion
          (set-buffer l-buffer-name)
          (if (or (equal major-mode 'dired-mode)
                  (equal major-mode 'Buffer-menu-mode))
            (setq l-browsing-mode-flag t)))
        (if l-browsing-mode-flag
          (progn
            (if (not eide-menu-browsing-mode-flag)
              (eide-menu-browsing-mode-start))
            ad-do-it
            p-buffer)
          (progn
            ;; Do not display TAGS file, and configuration files
            (if (or (string-equal l-buffer-name "TAGS") (string-equal l-buffer-name eide-config-file) (string-equal l-buffer-name eide-project-config-file) (string-equal l-buffer-name eide-project-notes-file))
              (setq l-do-it-flag nil))
            (if l-do-it-flag
              (progn
                (setq l-window (eide-i-windows-get-window-for-buffer l-buffer-name))
                (if l-window
                  (select-window l-window)
                  (setq l-window (selected-window)))
                ad-do-it
                (set-buffer l-buffer-name)
                (if eide-project-is-gdb-session-visible-flag
                  (eide-menu-update nil)
                  (progn
                    (if eide-search-find-symbol-definition-flag
                      (progn
                        (recenter)
                        (setq eide-search-find-symbol-definition-flag nil)))
                    (if (equal l-window eide-windows-source-window)
                      (progn
                        (if (and eide-menu-browsing-mode-flag
                                 (not (equal major-mode 'dired-mode))
                                 (not (equal major-mode 'Buffer-menu-mode)))
                          (eide-menu-browsing-mode-stop))
                        ;; Update menu if necessary
                        (eide-menu-update nil)))))
                ;; Select buffer window
                (select-window l-window)
                ;; Return the buffer that it switched to
                p-buffer)
              (progn
                ;; Close unwanted files (except TAGS and project configuration)
                (if (or (string-equal l-buffer-name eide-config-file) (string-equal l-buffer-name eide-project-notes-file))
                  (progn
                    (kill-buffer l-buffer-name)
                    ;; Return the current buffer
                    (current-buffer)))))))))))

;; ----------------------------------------------------------------------------
;; Override C-x C-f find-file, to get default directory from buffer in "source"
;; window.
;; ----------------------------------------------------------------------------
(defun eide-windows-find-file ()
  (interactive)
  (if eide-keys-is-editor-configuration-active-flag
    (progn
      (eide-windows-select-source-window nil)
      (call-interactively 'find-file))))

;; ----------------------------------------------------------------------------
;; Override save-buffer function (advice), to save buffer in "source" window.
;; ----------------------------------------------------------------------------
(defadvice save-buffer (around eide-save-buffer-advice-around)
  (let ((l-window (selected-window)))
    (eide-windows-select-source-window nil)
    ad-do-it
    (eide-menu-update-current-buffer-modified-status)
    (select-window l-window)))

;; ----------------------------------------------------------------------------
;; Override mode-line-unbury-buffer (previous buffer) function (advice), to
;; select appropriate buffer according to selected window (for Emacs 21 only).
;; ----------------------------------------------------------------------------
(defadvice mode-line-unbury-buffer (around eide-previous-buffer-advice-around (p-event))
  (interactive "e")
  ;; Temporarily select event's window (code taken from mode-line-bury-buffer)
  (save-selected-window
    (select-window (posn-window (event-start p-event)))
    (let ((l-window (selected-window)) (l-starting-from-buffer-name (buffer-name)) (l-do-it-flag t))
      ;; Temporarily disable switch-to-buffer advice: buffers must be displayed
      ;; in "source" window, until a correct one is found
      (ad-deactivate 'switch-to-buffer)
      (while l-do-it-flag
        ad-do-it
        (if (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
                (string-equal (buffer-name) l-starting-from-buffer-name))
          (setq l-do-it-flag nil)))
      (ad-activate 'switch-to-buffer)))
  (eide-menu-update nil))

;; ----------------------------------------------------------------------------
;; Override mode-line-bury-buffer (next buffer) function (advice), to select
;; appropriate buffer according to selected window (for Emacs 21 only).
;; ----------------------------------------------------------------------------
(defadvice mode-line-bury-buffer (around eide-previous-buffer-advice-around (p-event))
  (interactive "e")
  ;; Temporarily select event's window (code taken from mode-line-bury-buffer)
  (save-selected-window
    (select-window (posn-window (event-start p-event)))
    (let ((l-window (selected-window)) (l-starting-from-buffer-name (buffer-name)) (l-do-it-flag t))
      ;; Temporarily disable switch-to-buffer advice: buffers must be displayed
      ;; in "source" window, until a correct one is found
      (ad-deactivate 'switch-to-buffer)
      (while l-do-it-flag
        ad-do-it
        (if (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
                (string-equal (buffer-name) l-starting-from-buffer-name))
          (setq l-do-it-flag nil)))
      (ad-activate 'switch-to-buffer)))
  (eide-menu-update nil))

;; ----------------------------------------------------------------------------
;; Override previous-buffer function (advice), to select appropriate buffer
;; according to selected window.
;; ----------------------------------------------------------------------------
(defadvice previous-buffer (around eide-previous-buffer-advice-around)
  (let ((l-window (selected-window)) (l-starting-from-buffer-name (buffer-name)) (l-do-it-flag t))
    ;; Temporarily disable switch-to-buffer advice: buffers must be displayed
    ;; in "source" window, until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (while l-do-it-flag
      ad-do-it
      (if (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
              (string-equal (buffer-name) l-starting-from-buffer-name))
        (setq l-do-it-flag nil)))
    (ad-activate 'switch-to-buffer))
  (eide-menu-update nil))

;; ----------------------------------------------------------------------------
;; Override next-buffer function (advice), to select appropriate buffer
;; according to selected window.
;; ----------------------------------------------------------------------------
(defadvice next-buffer (around eide-next-buffer-advice-around)
  (let ((l-window (selected-window)) (l-starting-from-buffer-name (buffer-name)) (l-do-it-flag t))
    ;; Temporarily disable switch-to-buffer advice: buffers must be displayed
    ;; in "source" window, until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (while l-do-it-flag
      ad-do-it
      (if (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
              (string-equal (buffer-name) l-starting-from-buffer-name))
        (setq l-do-it-flag nil)))
    (ad-activate 'switch-to-buffer))
  (eide-menu-update nil))

;; ----------------------------------------------------------------------------
;; Override gdb-setup-windows function (advice), to unbuild windows layout
;; before gdb builds its own.
;; ----------------------------------------------------------------------------
(defadvice gdb-setup-windows (before eide-gdb-setup-windows-advice-before)
  (eide-project-debug-mode-start))

;; ----------------------------------------------------------------------------
;; Override gdb-restore-windows function (advice), to unbuild windows layout
;; before gdb builds its own.
;; ----------------------------------------------------------------------------
(defadvice gdb-restore-windows (before eide-gdb-setup-windows-advice-before)
  (eide-project-debug-mode-start))

;; ----------------------------------------------------------------------------
;; Hook to be called once the frame has been resized.
;;
;; output : eide-windows-output-window-height : height of "output" window.
;;          eide-windows-menu-window-width : width of "menu" window.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-window-setup-hook ()
  ;;(setq eide-windows-output-window-height (/ (frame-height) 5))
  ;;(setq eide-windows-menu-window-width (/ (frame-width) 4))

  ;; Close buffer "*Buffer List*" (created when emacs is launched with files as
  ;; parameters)
  (if (string-equal (buffer-name) "*Buffer List*")
    (kill-this-buffer))

  (setq eide-windows-output-window-height 9)
  (setq eide-windows-menu-window-width 40)
  (eide-windows-layout-build)
  (ad-activate 'select-window)
  (ad-activate 'switch-to-buffer)
  (ad-activate 'save-buffer)
  (if (fboundp 'previous-buffer)
    (progn
      ;; New API (Emacs 22)
      (ad-activate 'previous-buffer)
      (ad-activate 'next-buffer))
    (progn
      ;; Old API (Emacs 21)
      ;; mode-line-bury-buffer calls bury-buffer, but mode-line-unbury-buffer
      ;; calls switch-to-buffer => we need to override mode-line functions
      (ad-activate 'mode-line-unbury-buffer)
      (ad-activate 'mode-line-bury-buffer)))
  (ad-activate 'gdb-setup-windows)
  (ad-activate 'gdb-restore-windows)
  (setq display-buffer-function 'eide-i-windows-display-buffer-function)
  (eide-windows-skip-unwanted-buffers-in-source-window)
  ;; Create menu content (force to build and to retrieve files status)
  (eide-menu-update t t))

;; ----------------------------------------------------------------------------
;; Select window at mouse position.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-select-window-at-mouse-position ()
  ;; Select the window where the mouse is
  (let ((l-position (last (mouse-position))))
    (select-window (window-at (car l-position) (cdr l-position)))))

;; ----------------------------------------------------------------------------
;; Test if selected window is "source" window.
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-is-source-window-selected-p ()
  (equal (selected-window) eide-windows-source-window))

;; ----------------------------------------------------------------------------
;; Test if selected window is "menu" window.
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-is-menu-window-selected-p ()
  (equal (selected-window) eide-windows-menu-window))

;; ----------------------------------------------------------------------------
;; Test if selected window is "output" window.
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-is-output-window-selected-p ()
  (equal (selected-window) eide-windows-output-window))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Initialize windows.
;; ----------------------------------------------------------------------------
(defun eide-windows-init ()
  (add-hook 'window-setup-hook 'eide-i-windows-window-setup-hook))

;; ----------------------------------------------------------------------------
;; Build windows layout.
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;;          eide-config-menu-position : menu position (windows layout).
;;          eide-config-menu-height : menu height (windows layout).
;;          eide-windows-menu-window-width : width of "menu" window.
;;          eide-windows-output-window-height : height of "output" window.
;;          eide-windows-output-window-buffer : buffer in "output" window.
;; output : eide-windows-source-window : "source" window.
;;          eide-windows-menu-window : "menu" window.
;;          eide-windows-output-window : "output" window.
;;          eide-windows-is-layout-visible-flag : t (windows layout is shown).
;; ----------------------------------------------------------------------------
(defun eide-windows-layout-build ()
  (if (not eide-windows-is-layout-visible-flag)
    (progn
      (ad-deactivate 'select-window)
      (delete-other-windows)
      ;; Make sure that current window is not dedicated
      (set-window-dedicated-p (selected-window) nil)
      ;; Split into 3 windows ("source", "menu", "output")
      (if (string-equal eide-config-menu-height "full")
        (progn
          (split-window-horizontally)
          (if (string-equal eide-config-menu-position "left")
            ;; Menu on left side
            (progn
              (setq eide-windows-menu-window (selected-window))
              (select-window (next-window))
              (split-window-vertically)
              (setq eide-windows-source-window (selected-window))
              (select-window (next-window))
              (setq eide-windows-output-window (selected-window)))
            ;; Menu on right side
            (progn
              (split-window-vertically)
              (setq eide-windows-source-window (selected-window))
              (select-window (next-window))
              (setq eide-windows-output-window (selected-window))
              (select-window (next-window))
              (setq eide-windows-menu-window (selected-window)))))
        (progn
          (split-window-vertically)
          (split-window-horizontally)
          (if (string-equal eide-config-menu-position "left")
            ;; Menu on left side
            (progn
              (setq eide-windows-menu-window (selected-window))
              (select-window (next-window))
              (setq eide-windows-source-window (selected-window))
              (select-window (next-window))
              (setq eide-windows-output-window (selected-window)))
            ;; Menu on right side
            (progn
              (setq eide-windows-source-window (selected-window))
              (select-window (next-window))
              (setq eide-windows-menu-window (selected-window))
              (select-window (next-window))
              (setq eide-windows-output-window (selected-window))))))

      ;; "Menu" window
      (select-window eide-windows-menu-window)
      (switch-to-buffer eide-menu-buffer-name)
      ;; This window should be used for this buffer only
      (set-window-dedicated-p eide-windows-menu-window t)
      ;;(setq window-min-width 1) ; TODO: sans effet ?
      (enlarge-window-horizontally (- eide-windows-menu-window-width (window-width)))

      ;; "Output" window
      (select-window eide-windows-output-window)
      (setq window-min-height 2)
      (enlarge-window (- eide-windows-output-window-height (window-height)))
      (switch-to-buffer (get-buffer-create "*results*"))
      (if eide-windows-output-window-buffer
        (switch-to-buffer eide-windows-output-window-buffer)
        (setq eide-windows-output-window-buffer "*results*"))

      (select-window eide-windows-source-window)
      (setq eide-windows-is-layout-visible-flag t)
      (eide-windows-skip-unwanted-buffers-in-source-window)
      ;; Update menu if necessary
      (if eide-windows-menu-update-request-pending-flag
        (eide-menu-update eide-windows-menu-update-request-pending-force-rebuild-flag eide-windows-menu-update-request-pending-force-update-status-flag))
      (ad-activate 'select-window))))

;; ----------------------------------------------------------------------------
;; Unbuild windows layout (keep only "source" window).
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; output : eide-windows-menu-window-width : width of "menu" window.
;;          eide-windows-output-window-height : height of "output" window.
;;          eide-windows-output-window-buffer : buffer in "output" window.
;;          eide-windows-is-layout-visible-flag : nil (windows layout is
;;              hidden).
;; ----------------------------------------------------------------------------
(defun eide-windows-layout-unbuild ()
  (if eide-windows-is-layout-visible-flag
    (progn
      (ad-deactivate 'select-window)
      (if (and (window-live-p eide-windows-menu-window)
               (window-live-p eide-windows-output-window)
               (window-live-p eide-windows-source-window))
        ;; Remember windows positions only if the layout is complete
        (progn
          ;; Remember "menu" window width
          (eide-windows-select-menu-window)
          (setq eide-windows-menu-window-width (window-width))
          ;; Remember "output" window height
          (eide-windows-select-output-window)
          (setq eide-windows-output-window-height (window-height))
          ;; Remember which result buffer is displayed in "output" window
          (setq eide-windows-output-window-buffer (buffer-name))))
      (if (window-live-p eide-windows-source-window)
        ;; Keep only "source" window
        (eide-windows-select-source-window t))
      (delete-other-windows)
      ;; Make sure that current window is not dedicated
      (set-window-dedicated-p (selected-window) nil)
      ;; Current window becomes - if not already - "source" window
      (setq eide-windows-menu-window nil)
      (setq eide-windows-output-window nil)
      (setq eide-windows-source-window (selected-window))
      (setq eide-windows-is-layout-visible-flag nil)
      (eide-windows-skip-unwanted-buffers-in-source-window)
      (ad-activate 'select-window))))

;; ----------------------------------------------------------------------------
;; Select "source" window.
;;
;; input  : p-force-build-flag : t = build windows layout if not visible.
;;          eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-select-source-window (p-force-build-flag)
  (if (or eide-windows-is-layout-visible-flag p-force-build-flag)
    (progn
      (if (not eide-windows-is-layout-visible-flag)
        (eide-windows-layout-build))
      (select-window eide-windows-source-window))))

;; ----------------------------------------------------------------------------
;; Select "menu" window (build windows layout if necessary).
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-select-menu-window ()
  (if (not eide-windows-is-layout-visible-flag)
    (eide-windows-layout-build))
  (select-window eide-windows-menu-window))

;; ----------------------------------------------------------------------------
;; Select "output" window (build windows layout if necessary).
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-select-output-window ()
  (if (not eide-windows-is-layout-visible-flag)
    (eide-windows-layout-build))
  (select-window eide-windows-output-window))

;; ----------------------------------------------------------------------------
;; Parse buffers list until an appropriate buffer is found, that can be
;; displayed. Current buffer is kept if correct.
;; ----------------------------------------------------------------------------
(defun eide-windows-skip-unwanted-buffers-in-source-window ()
  (eide-windows-select-source-window nil)
  (let ((l-should-we-continue t) (l-current-buffer-name (buffer-name)) (l-first-found-buffer-name nil) (l-iteration 0))
    ;; Temporarily disable switch-to-buffer advice: buffers must be displayed
    ;; in "source" window, until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (while (and (not (equal (eide-i-windows-get-window-for-buffer (buffer-name)) eide-windows-source-window))
                l-should-we-continue
                (< l-iteration 30))
      (progn
        (bury-buffer)
        (if (= l-iteration 0)
          (setq l-first-found-buffer-name (buffer-name))
          (if (string-equal (buffer-name) l-first-found-buffer-name)
            ;; We have parsed the whole buffer list without finding any other
            ;; buffer that fits. Moreover, current buffer cannot be found again
            ;; (because bs-cycle-xxx ignores temporary buffers), which means
            ;; that it is not valid either. Let's display "*scratch*".
            (switch-to-buffer "*scratch*")))
        (if (string-equal (buffer-name) l-current-buffer-name)
          (progn
            ;; We have parsed the whole buffer list without finding any other
            ;; buffer that fits. If this buffer is valid, let's keep it
            ;; current. Otherwise, let's display "*scratch*".
            (setq l-should-we-continue nil)
            (if (not (equal (eide-i-windows-get-window-for-buffer (buffer-name)) eide-windows-source-window))
              (switch-to-buffer "*scratch*"))))
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
  (if eide-project-is-gdb-session-visible-flag
    (eide-project-debug-mode-stop)
    (progn
      ;; Select the window where the mouse is
      (eide-i-windows-select-window-at-mouse-position)

      (if (string-equal (buffer-name) "*Colors*")
        (progn
          ;; Close colors buffer and window
          (kill-this-buffer)
          (select-window (next-window))
          (if (string-equal (buffer-name) eide-config-file)
            (delete-other-windows))))
      (if (string-equal (buffer-name) "* Help *")
        ;; Close "help"
        (progn
          (kill-buffer "* Help *")
          (eide-config-set-colors-for-files)
          (eide-keys-configure-for-editor)
          (eide-windows-layout-build))
        (if (string-equal (buffer-name) eide-config-file)
          ;; Close ".emacs-ide.options"
          (progn
            (save-buffer)
            (eide-config-rebuild-config-file)
            (eide-config-set-colors-for-files)
            (eide-keys-configure-for-editor)
            (eide-windows-layout-build)
            ;; Close colors buffer if opened
            (if (get-buffer "*Colors*")
              (kill-buffer "*Colors*")))
          (if (string-equal (buffer-name) eide-project-config-file)
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
                    ;; If windows layout is supposed to be visible, but one of
                    ;; the three windows is not visible, first unbuild, to
                    ;; force rebuild
                    (if (and eide-windows-is-layout-visible-flag
                             (or (not (window-live-p eide-windows-menu-window))
                                 (not (window-live-p eide-windows-output-window))
                                 (not (window-live-p eide-windows-source-window))))
                      (eide-windows-layout-unbuild))
                    (if (eide-i-windows-is-output-window-selected-p)
                      ;; "Output" window: open search results popup menu
                      (eide-popup-open-menu-for-search-results)
                      (if (eide-i-windows-is-menu-window-selected-p)
                        ;; "Menu" window: open project popup menu
                        (eide-popup-open-menu)
                        ;; "Source" window
                        (if eide-windows-is-layout-visible-flag
                          ;; Hide
                          (eide-windows-layout-unbuild)
                          ;; Show
                          (progn
                            (if eide-menu-browsing-mode-flag
                              (eide-menu-browsing-mode-stop))
                            ;; Build windows layout (if not already built by eide-menu-browsing-mode-stop)
                            (eide-windows-layout-build)))))))))))))))

;; ----------------------------------------------------------------------------
;; Handle mouse-2 (middle click) action.
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-mouse-2 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)

  (if (and eide-windows-is-layout-visible-flag (eide-i-windows-is-menu-window-selected-p))
    (eide-menu-dired-open)
    (yank)))

;; ----------------------------------------------------------------------------
;; Handle shift + mouse-3 (right click) action.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-shift-mouse-3 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)

  (if (eide-i-windows-is-output-window-selected-p)
    ;; In "output" window, open popup menu to delete search results
    (eide-popup-open-menu-for-search-results-delete)
    ;; In options, show/hide list of colors
    (if (string-equal (buffer-name) eide-config-file)
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
          (if (string-equal (buffer-name) eide-config-file)
            (delete-other-windows)))))))

;; ----------------------------------------------------------------------------
;; Load a file without using advice (when "menu" buffer must not be updated).
;;
;; input  : p-file : filename.
;; ----------------------------------------------------------------------------
(defun eide-windows-find-file-without-advice (p-file)
  ;; find-file advice would change eide-current-buffer
  ;; and menu buffer would be updated with temp files
  (ad-deactivate 'switch-to-buffer)
  (find-file p-file)
  (ad-activate 'switch-to-buffer))

;;; eide-windows.el ends here
