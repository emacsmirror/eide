;;; eide-windows.el --- Emacs-IDE, windows

;; Copyright (C) 2005-2010 Cédric Marie

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

(defvar eide-windows-is-layout-visible-flag nil)
(defvar eide-windows-menu-update-request-pending-flag nil)
(defvar eide-windows-menu-update-request-pending-force-rebuild-flag nil)
(defvar eide-windows-menu-update-request-pending-force-update-status-flag nil)

(defvar eide-windows-buffer-in-window-results nil)
(defvar eide-compilation-buffer nil)
(defvar eide-execution-buffer nil)
(defvar eide-debug-buffer nil)
(defvar eide-shell-buffer nil)

(defvar eide-windows-update-result-buffer-id nil)

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
          eide-windows-window-menu
          eide-windows-window-results)
        ;; Layout is not built => windows "menu" and "results" don't exist
        nil)
      (save-excursion
        (set-buffer p-buffer-name)
        (if (or (equal major-mode 'dired-mode)
                (equal major-mode 'Buffer-menu-mode))
          nil
          (if (or (string-equal "TAGS" p-buffer-name)
                  (string-equal eide-options-file p-buffer-name)
                  (string-equal eide-project-file p-buffer-name)
                  (string-equal eide-project-notes-file p-buffer-name))
            nil
            eide-windows-window-file))))
    nil))

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
(defun eide-i-windows-display-buffer-function (p-buffer &optional p-not-this-window p-frame)
  (let ((l-buffer-name) (l-window)
        (l-selected-window (selected-window))
        (l-browsing-mode-flag nil))
    (if (bufferp p-buffer)
      (setq l-buffer-name (buffer-name p-buffer))
      (setq l-buffer-name p-buffer))
    ;;(message (concat "eide-i-windows-display-buffer-function : " l-buffer-name))
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
              (progn
                (select-window eide-windows-window-file)
                (split-window-vertically)
                (select-window (next-window))
                (setq l-window (selected-window)))))
          (progn
            (setq l-window (eide-i-windows-get-window-for-buffer l-buffer-name))
            (if (not l-window)
              (setq l-window l-selected-window))))
        (set-window-buffer l-window p-buffer)
        ;; Result buffer name is updated asynchronously
        (if eide-windows-update-result-buffer-id
          (progn
            (if (string-equal eide-windows-update-result-buffer-id "c")
              (setq eide-compilation-buffer l-buffer-name)
              (if (string-equal eide-windows-update-result-buffer-id "r")
                (setq eide-execution-buffer l-buffer-name)
                (if (string-equal eide-windows-update-result-buffer-id "d")
                  (setq eide-debug-buffer l-buffer-name)
                  (if (string-equal eide-windows-update-result-buffer-id "s")
                    (setq eide-shell-buffer l-buffer-name)))))
            (setq eide-windows-update-result-buffer-id nil)))
        (if (equal l-window eide-windows-window-file)
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
            ;; Window "results" temporarily expands to half or 2/3 of the frame to
            ;; display completions
            (let ((l-completion-height (max (+ (count-lines (point-min) (point-max)) 2) (/ (frame-height) 2))))
              (if (> l-completion-height (/ (frame-height) 2))
                (setq l-completion-height (/ (* (frame-height) 2) 3)))
              (enlarge-window (- l-completion-height (window-height))))))))
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
        ;;(message (concat "switch-to-buffer : " l-buffer-name))
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
            (if (or (string-equal l-buffer-name "TAGS") (string-equal l-buffer-name eide-options-file) (string-equal l-buffer-name eide-project-file) (string-equal l-buffer-name eide-project-notes-file))
              (setq l-do-it-flag nil))
            (if l-do-it-flag
              (progn
                (setq l-window (eide-i-windows-get-window-for-buffer l-buffer-name))
                (if l-window
                  (select-window l-window)
                  (setq l-window (selected-window)))
                ad-do-it
                (set-buffer l-buffer-name)
                (if eide-search-find-symbol-definition-flag
                  (progn
                    (recenter)
                    (setq eide-search-find-symbol-definition-flag nil)))
                (if (equal l-window eide-windows-window-file)
                  (progn
                    (if (and eide-menu-browsing-mode-flag
                             (not (equal major-mode 'dired-mode))
                             (not (equal major-mode 'Buffer-menu-mode)))
                      (eide-menu-browsing-mode-stop))
                    ;; Update menu if necessary
                    (eide-menu-update nil)))
                ;; Select buffer window
                (select-window l-window)
                ;; Return the buffer that it switched to
                p-buffer)
              (progn
                ;; Close unwanted files (except TAGS and project configuration)
                (if (or (string-equal l-buffer-name eide-options-file) (string-equal l-buffer-name eide-project-notes-file))
                  (progn
                    (kill-buffer l-buffer-name)
                    ;; Return the current buffer
                    (current-buffer)))))))))))

;; ----------------------------------------------------------------------------
;; Override C-x C-f find-file, to get default directory from buffer in window
;; "file".
;; ----------------------------------------------------------------------------
(defun eide-windows-find-file ()
  (interactive)
  (if eide-keys-is-editor-configuration-active-flag
    (progn
      (eide-windows-select-window-file nil)
      (call-interactively 'find-file))))

;; ----------------------------------------------------------------------------
;; Override save-buffer function (advice), to save buffer in window "file".
;; ----------------------------------------------------------------------------
(defadvice save-buffer (around eide-save-buffer-advice-around)
  (let ((l-window (selected-window)))
    (eide-windows-select-window-file nil)
    ad-do-it
    (eide-menu-update-current-buffer-modified-status)
    (select-window l-window)))

                                        ;(defun eide-i-windows-is-buffer-appropriate-for-window (p-buffer-name p-window)
                                        ;  (or (equal p-window (eide-i-windows-get-window-for-buffer p-buffer-name))
                                        ;      (and (not eide-windows-is-layout-visible-flag)
                                        ;           (equal p-window eide-windows-window-file)
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
      ;; Temporarily disable switch-to-buffer advice : buffers must be displayed
      ;; in window "file", until a correct one is found
      (ad-deactivate 'switch-to-buffer)
      (while l-do-it-flag
        ad-do-it
        (if (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
                (string-equal (buffer-name) l-starting-from-buffer-name))
          (setq l-do-it-flag nil))))
    (ad-activate 'switch-to-buffer))
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
      ;; Temporarily disable switch-to-buffer advice : buffers must be displayed
      ;; in window "file", until a correct one is found
      (ad-deactivate 'switch-to-buffer)
      (while l-do-it-flag
        ad-do-it
        (if (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
                (string-equal (buffer-name) l-starting-from-buffer-name))
          (setq l-do-it-flag nil))))
    (ad-activate 'switch-to-buffer))
  (eide-menu-update nil))

;; ----------------------------------------------------------------------------
;; Override previous-buffer function (advice), to select appropriate buffer
;; according to selected window.
;; ----------------------------------------------------------------------------
(defadvice previous-buffer (around eide-previous-buffer-advice-around)
  (let ((l-window (selected-window)) (l-starting-from-buffer-name (buffer-name)) (l-do-it-flag t))
    ;; Temporarily disable switch-to-buffer advice : buffers must be displayed
    ;; in window "file", until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (while l-do-it-flag
      ad-do-it
      (if (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
              (string-equal (buffer-name) l-starting-from-buffer-name))
        (setq l-do-it-flag nil))))
  (ad-activate 'switch-to-buffer)
  (eide-menu-update nil))

;; ----------------------------------------------------------------------------
;; Override next-buffer function (advice), to select appropriate buffer
;; according to selected window.
;; ----------------------------------------------------------------------------
(defadvice next-buffer (around eide-next-buffer-advice-around)
  (let ((l-window (selected-window)) (l-starting-from-buffer-name (buffer-name)) (l-do-it-flag t))
    ;; Temporarily disable switch-to-buffer advice : buffers must be displayed
    ;; in window "file", until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (while l-do-it-flag
      ad-do-it
      (if (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
              (string-equal (buffer-name) l-starting-from-buffer-name))
        (setq l-do-it-flag nil))))
  (ad-activate 'switch-to-buffer)
  (eide-menu-update nil))

;; ----------------------------------------------------------------------------
;; Hook to be called once the frame has been resized.
;;
;; output : eide-windows-results-window-height : height of window "results".
;;          eide-windows-menu-window-width : width of window "menu".
;; ----------------------------------------------------------------------------
(defun eide-i-windows-window-setup-hook ()
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
  (setq display-buffer-function 'eide-i-windows-display-buffer-function)
  (eide-windows-skip-unwanted-buffers-in-window-file)
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
;; Test if selected window is window "file".
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-is-window-file-selected-p ()
  (equal (selected-window) eide-windows-window-file))

;; ----------------------------------------------------------------------------
;; Test if selected window is window "menu".
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-is-window-menu-selected-p ()
  (equal (selected-window) eide-windows-window-menu))

;; ----------------------------------------------------------------------------
;; Test if selected window is window "results".
;;
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-i-windows-is-window-results-selected-p ()
  (equal (selected-window) eide-windows-window-results))

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
;;          eide-windows-menu-window-width : width of window "menu".
;;          eide-windows-results-window-height : height of window "results".
;;          eide-windows-buffer-in-window-results : buffer in window "results".
;; output : eide-windows-window-file : window "file".
;;          eide-windows-window-menu : window "menu".
;;          eide-windows-window-results : window "results".
;;          eide-windows-is-layout-visible-flag : t (windows layout is shown).
;; ----------------------------------------------------------------------------
(defun eide-windows-layout-build ()
  (if (not eide-windows-is-layout-visible-flag)
    (progn
      (delete-other-windows)

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

      ;; Window "menu"
      (select-window eide-windows-window-menu)
      (switch-to-buffer eide-menu-buffer-name)
      ;; This window should be used for this buffer only
      (set-window-dedicated-p eide-windows-window-menu t)
      ;;(setq window-min-width 1) ; TODO : sans effet ?
      (enlarge-window-horizontally (- eide-windows-menu-window-width (window-width)))

      ;; Window "results"
      (select-window eide-windows-window-results)
      (setq window-min-height 2)
      (enlarge-window (- eide-windows-results-window-height (window-height)))
      (switch-to-buffer (get-buffer-create "*results*"))
      (if eide-windows-buffer-in-window-results
        (switch-to-buffer eide-windows-buffer-in-window-results)
        (setq eide-windows-buffer-in-window-results "*results*"))

      (select-window eide-windows-window-file)

      (setq eide-windows-is-layout-visible-flag t))))

;; ----------------------------------------------------------------------------
;; Unbuild windows layout (keep only window "file").
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
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
      ;; Keep only window "file"
      (eide-windows-select-window-file t)
      (delete-other-windows)

      (setq eide-windows-is-layout-visible-flag nil))))

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
;; Parse buffers list until an appropriate buffer is found, that can be
;; displayed. Current buffer is kept if correct.
;; ----------------------------------------------------------------------------
(defun eide-windows-skip-unwanted-buffers-in-window-file ()
  (eide-windows-select-window-file nil)
  (let ((l-should-we-continue t) (l-current-buffer-name (buffer-name)) (l-iteration 0))
    ;; Temporarily disable switch-to-buffer advice : buffers must be displayed
    ;; in window "file", until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (setq l-first-found-buffer-name nil)
    (while (and (not (equal (eide-i-windows-get-window-for-buffer (buffer-name)) eide-windows-window-file))
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
            (if (not (equal (eide-i-windows-get-window-for-buffer (buffer-name)) eide-windows-window-file))
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
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)

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
                (if (eide-i-windows-is-window-results-selected-p)
                  ;; Window "results" : open grep results popup menu
                  (eide-popup-open-menu-for-search-results)
                  (if (eide-i-windows-is-window-menu-selected-p)
                    ;; Window "menu" : open project popup menu
                    (eide-popup-open-menu)
                    ;; Window "file"
                    (if eide-windows-is-layout-visible-flag
                      ;; Hide
                      (eide-windows-layout-unbuild)
                      ;; Show
                      (progn
                        (if eide-menu-browsing-mode-flag
                          (eide-menu-browsing-mode-stop))
                        ;; Build windows layout (if not already built by eide-menu-browsing-mode-stop)
                        (eide-windows-layout-build)
                        ;; Update menu if necessary
                        (if eide-windows-menu-update-request-pending-flag
                          (eide-menu-update eide-windows-menu-update-request-pending-force-rebuild-flag eide-windows-menu-update-request-pending-force-update-status-flag))
                        (eide-windows-select-window-file t)))))))))))))

;; ----------------------------------------------------------------------------
;; Handle mouse-2 (middle click) action.
;;
;; input  : eide-windows-is-layout-visible-flag : t = windows layout is shown.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-mouse-2 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)

  (if (and eide-windows-is-layout-visible-flag (eide-i-windows-is-window-menu-selected-p))
    (eide-menu-dired-open)
    (yank)))

;; ----------------------------------------------------------------------------
;; Handle shift + mouse-3 (right click) action.
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-shift-mouse-3 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)

  (if (eide-i-windows-is-window-results-selected-p)
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
