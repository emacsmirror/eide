;;; eide-windows.el --- Emacs-IDE, windows

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

(provide 'eide-windows)

(require 'eide-config)
(require 'eide-menu)

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

(defvar eide-windows-frame-fullscreen-value nil)

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-windows-get-window-for-buffer (p-buffer-name)
  "Get the window in which a buffer should be displayed.
Argument:
- p-buffer-name: buffer name."
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
      (save-current-buffer
        (set-buffer p-buffer-name)
        (if (or (equal major-mode 'dired-mode)
                (equal major-mode 'Buffer-menu-mode))
          nil
          (if (eide-windows-is-file-special-p p-buffer-name)
            nil
            eide-windows-source-window))))
    nil))

(defun eide-i-windows-display-buffer-function (p-buffer &optional p-not-this-window p-frame)
  "Display a buffer in appropriate window (display-buffer-function).
Returns the window.
Called for: - compile, run, and shell buffers.
            - man pages.
Arguments (same as display-buffer function):
- p-buffer: buffer.
- p-not-this-window (optional): force to display in another window.
- p-frame (optional): frame."
  (let ((l-buffer-name) (l-window)
        (l-selected-window (selected-window))
        (l-browsing-mode-flag nil))
    (if (bufferp p-buffer)
      (setq l-buffer-name (buffer-name p-buffer))
      (setq l-buffer-name p-buffer))
    ;;(message (concat "eide-i-windows-display-buffer-function: " l-buffer-name))
    (save-current-buffer
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

(defadvice select-window (after eide-select-window-advice-after (p-window &optional p-norecord))
  "Override select-window function (advice), to know which window is the active
\"source\" window.
Arguments (same as select-window function):
- p-window: window.
- p-norecord (optional): don't add the buffer to the list of recently selected
ones."
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

(defadvice switch-to-buffer (around eide-switch-to-buffer-advice-around (p-buffer &optional p-norecord))
  "Override switch-to-buffer function (advice), to display buffer in appropriate
window.
Returns the buffer.
Arguments (same as switch-to-buffer function):
- p-buffer: buffer.
- p-norecord (optional): don't add the buffer to the list of recently selected
ones."
  (let ((l-buffer-name) (l-browsing-mode-flag nil) (l-window))
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
        (if (get-buffer l-buffer-name)
          (save-current-buffer
            (set-buffer l-buffer-name)
            (if (or (equal major-mode 'dired-mode)
                    (equal major-mode 'Buffer-menu-mode))
              (setq l-browsing-mode-flag t))))
        (if l-browsing-mode-flag
          (progn
            (if (not eide-menu-browsing-mode-flag)
              (eide-menu-browsing-mode-start))
            ad-do-it
            p-buffer)
          (progn
            (if (eide-windows-is-file-special-p l-buffer-name)
              (progn
                ;; Do not display special files
                (if (string-equal l-buffer-name eide-project-notes-file)
                  ;; Project notes file should not be opened with switch-to-buffer advice
                  (kill-buffer l-buffer-name))
                ;; Return the current buffer
                (current-buffer))
              (progn
                (setq l-window (eide-i-windows-get-window-for-buffer l-buffer-name))
                (if l-window
                  (select-window l-window)
                  (setq l-window (selected-window)))
                ad-do-it
                (set-buffer l-buffer-name)
                (if (and eide-custom-override-emacs-settings
                         (not (equal eide-custom-show-trailing-spaces 'ignore))
                         (equal l-window eide-windows-source-window))
                  ;; Show trailing spaces if enabled in options
                  (if eide-custom-show-trailing-spaces
                    (setq show-trailing-whitespace t)
                    (setq show-trailing-whitespace nil)))
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
                p-buffer))))))))

(defun eide-windows-find-file ()
  "Override C-x C-f find-file, to get default directory from buffer in \"source\"
window."
  (interactive)
  (if eide-keys-is-editor-configuration-active-flag
    (progn
      (eide-windows-select-source-window nil)
      (call-interactively 'find-file))))

(defadvice save-buffer (around eide-save-buffer-advice-around (&optional p-backup-option))
  "Override save-buffer function (advice), to save buffer in \"source\" window.
Argument (same as save-buffer function):
- p-backup-option (optional): backup method."
  (let ((l-window (selected-window)))
    (eide-windows-select-source-window nil)
    ad-do-it
    (eide-menu-update-current-buffer-modified-status)
    (if (equal eide-custom-update-cscope-database 'auto)
      ;; Current buffer has been modified and saved: we must update cscope database
      (setq eide-search-cscope-update-database-request-pending-flag t))
    (select-window l-window)))

(defadvice revert-buffer (after eide-revert-buffer-advice-after (&optional p-ignore-auto p-noconfirm p-preserve-modes))
  "Override revert-buffer function (advice), to update cscope database.
Arguments (same as revert-buffer function):
- p-ignore-auto (optional): ignore auto-save file.
- p-noconfirm (optional): don't ask for confirmation.
- p-preserve-modes (optional): preserve file modes."
  (if (equal eide-custom-update-cscope-database 'auto)
    ;; Current buffer has been updated: we must update cscope database
    (setq eide-search-cscope-update-database-request-pending-flag t)))

(defadvice mode-line-unbury-buffer (around eide-previous-buffer-advice-around (p-event))
  "Override mode-line-unbury-buffer (previous buffer) function (advice), to select
appropriate buffer according to selected window (for Emacs 21 only).
Argument (same as mode-line-unbury-buffer):
- p-event: event."
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

(defadvice mode-line-bury-buffer (around eide-previous-buffer-advice-around (p-event))
  "Override mode-line-bury-buffer (next buffer) function (advice), to select
appropriate buffer according to selected window (for Emacs 21 only).
Argument (same as mode-line-bury-buffer):
- p-event: event."
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

(defadvice previous-buffer (around eide-previous-buffer-advice-around)
  "Override previous-buffer function (advice), to select appropriate buffer
according to selected window."
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

(defadvice next-buffer (around eide-next-buffer-advice-around)
  "Override next-buffer function (advice), to select appropriate buffer according
to selected window."
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

(defadvice gdb-setup-windows (before eide-gdb-setup-windows-advice-before)
  "Override gdb-setup-windows function (advice), to unbuild windows layout before
gdb builds its own."
  (eide-project-debug-mode-start))

(defadvice gdb-restore-windows (before eide-gdb-setup-windows-advice-before)
  "Override gdb-restore-windows function (advice), to unbuild windows layout
before gdb builds its own."
  (eide-project-debug-mode-start))

(defun eide-i-windows-window-setup-hook ()
  "Hook to be called once the frame has been resized."
  (eide-config-apply)

  ;; Close buffer "*Buffer List*" (created when emacs is launched with files as
  ;; parameters)
  (if (string-equal (buffer-name) "*Buffer List*")
    (kill-this-buffer))

  (setq eide-windows-output-window-height (/ (frame-height) 5))
  (setq eide-windows-menu-window-width (/ (frame-width) 5))
  (if window-system
    (eide-windows-layout-build))
  (ad-activate 'select-window)
  (ad-activate 'switch-to-buffer)
  (ad-activate 'save-buffer)
  (ad-activate 'revert-buffer)
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
  (eide-menu-update t t)

  (if (and eide-custom-override-emacs-settings eide-custom-start-maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun eide-i-windows-select-window-at-mouse-position ()
  "Select window at mouse position."
  ;; Select the window where the mouse is
  (let ((l-position (last (mouse-position))))
    (select-window (window-at (car l-position) (cdr l-position)))))

(defun eide-i-windows-is-source-window-selected-p ()
  "Test if selected window is \"source\" window."
  (equal (selected-window) eide-windows-source-window))

(defun eide-i-windows-is-menu-window-selected-p ()
  "Test if selected window is \"menu\" window."
  (equal (selected-window) eide-windows-menu-window))

(defun eide-i-windows-is-output-window-selected-p ()
  "Test if selected window is \"output\" window."
  (equal (selected-window) eide-windows-output-window))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-windows-init ()
  "Initialize windows."
  (add-hook 'window-setup-hook 'eide-i-windows-window-setup-hook))

(defun eide-windows-layout-build ()
  "Build windows layout."
  (if (not eide-windows-is-layout-visible-flag)
    (progn
      (ad-deactivate 'select-window)
      (delete-other-windows)
      ;; Make sure that current window is not dedicated
      (set-window-dedicated-p (selected-window) nil)
      ;; Split into 3 windows ("source", "menu", "output")
      (if (equal eide-custom-menu-window-height 'full)
        (progn
          (split-window-horizontally)
          (if (equal eide-custom-menu-window-position 'left)
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
          (if (equal eide-custom-menu-window-position 'left)
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
      (eide-windows-skip-unwanted-buffers-in-source-window)
      (setq eide-windows-is-layout-visible-flag t)
      ;; Update menu if necessary
      (if eide-windows-menu-update-request-pending-flag
        (eide-menu-update nil))
      (ad-activate 'select-window))))

(defun eide-windows-layout-unbuild ()
  "Unbuild windows layout (keep only \"source\" window)."
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

(defun eide-windows-select-source-window (p-force-build-flag)
  "Select \"source\" window.
Argument:
- p-force-build-flag: t = build windows layout if not visible."
  (if (or eide-windows-is-layout-visible-flag p-force-build-flag)
    (progn
      (if (not eide-windows-is-layout-visible-flag)
        (eide-windows-layout-build))
      (select-window eide-windows-source-window))))

(defun eide-windows-select-menu-window ()
  "Select \"menu\" window (build windows layout if necessary)."
  (if (not eide-windows-is-layout-visible-flag)
    (eide-windows-layout-build))
  (select-window eide-windows-menu-window))

(defun eide-windows-select-output-window ()
  "Select \"output\" window (build windows layout if necessary)."
  (if (not eide-windows-is-layout-visible-flag)
    (eide-windows-layout-build))
  (select-window eide-windows-output-window))

(defun eide-windows-is-file-special-p (l-buffer-name)
  "Test if the file is special or not. A special file must not be displayed.
Special files are: tags, cscope files, and emacs-ide hidden files.
Argument:
- l-buffer-name: buffer name."
  (or (string-equal l-buffer-name "TAGS")
      (string-equal l-buffer-name "cscope.files")
      (string-equal l-buffer-name "cscope.out")
      (string-equal l-buffer-name eide-project-config-file)
      (string-equal l-buffer-name eide-project-notes-file)))

(defun eide-windows-skip-unwanted-buffers-in-source-window ()
  "Parse buffers list until an appropriate buffer is found, that can be displayed,
and display it. Current buffer is kept if correct."
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
    (if (and eide-custom-override-emacs-settings
             (not (equal eide-custom-show-trailing-spaces 'ignore)))
      ;; Show trailing spaces if enabled in options
      (if eide-custom-show-trailing-spaces
        (setq show-trailing-whitespace t)
        (setq show-trailing-whitespace nil)))
    ;; Update menu (switch-to-buffer advice was disabled)
    (eide-menu-update nil)))

(defun eide-windows-set-colors-for-files ()
  "Set colors for edition mode."
  (if (and eide-custom-override-emacs-settings eide-custom-extend-color-theme-to-source-code)
    (progn
      (set-background-color eide-config-background-color)
      (set-foreground-color eide-config-foreground-color)
      (set-face-background 'fringe eide-config-background-color))
    (progn
      (set-background-color eide-config-user-background-color)
      (set-foreground-color eide-config-user-foreground-color)
      (set-face-background 'fringe eide-config-user-background-color))))

(defun eide-windows-handle-mouse-3 ()
  "Handle mouse-3 (right click) action."
  (interactive)
  (if eide-project-is-gdb-session-visible-flag
    (eide-project-debug-mode-stop)
    (progn
      ;; Select the window where the mouse is
      (eide-i-windows-select-window-at-mouse-position)
      (if (or (string-equal (buffer-name) "* Help *") (string-equal (buffer-name) eide-project-projects-buffer-name))
        ;; Close "help" or projects list
        (progn
          (kill-this-buffer)
          (eide-windows-set-colors-for-files)
          (eide-keys-configure-for-editor)
          (eide-windows-layout-build))
        (if (string-match "^\*Customize.*" (buffer-name))
          ;; Close customization
          (progn
            ;; NB: Exit button does not work...
            (kill-this-buffer)
            (eide-keys-configure-for-editor)
            (eide-windows-layout-build))
          (if (string-equal (buffer-name) eide-project-config-file)
            ;; Display another buffer (other than ".emacs-ide-project.cfg")
            (progn
              (save-buffer)
              (if (eide-project-rebuild-config-file nil)
                ;; Project name has changed
                (progn
                  (eide-menu-update-project-name)
                  (eide-project-update-name)))
              ;; This buffer must not be closed
              (switch-to-buffer eide-current-buffer)
              (eide-windows-set-colors-for-files)
              (eide-keys-configure-for-editor)
              (eide-windows-layout-build))
            (if (string-equal (buffer-name) eide-project-notes-file)
              ;; Close ".emacs-ide-project.txt"
              (progn
                (save-buffer)
                (kill-buffer eide-project-notes-file)
                (eide-windows-set-colors-for-files)
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

(defun eide-windows-handle-mouse-2 ()
  "Handle mouse-2 (middle click) action."
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)
  (if (and eide-windows-is-layout-visible-flag (eide-i-windows-is-menu-window-selected-p))
    (eide-menu-dired-open)
    (yank)))

(defun eide-windows-handle-shift-mouse-3 ()
  "Handle shift + mouse-3 (right click) action."
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)
  (if (eide-i-windows-is-output-window-selected-p)
    ;; In "output" window, open popup menu to delete search results
    (eide-popup-open-menu-for-search-results-delete)))

(defun eide-windows-find-file-without-advice (p-file)
  "Load a file without using advice (when \"menu\" buffer must not be updated).
Argument:
- p-file: file."
  ;; find-file advice would change eide-current-buffer
  ;; and menu buffer would be updated with temp files
  (ad-deactivate 'switch-to-buffer)
  (find-file p-file)
  (ad-activate 'switch-to-buffer))

(defun eide-windows-toggle-frame-fullscreen-mode ()
  "Toggle frame fullscreen mode between fullboth and nil or maximized (depending
on previous state)."
  (interactive)
  (let ((l-frame-fullscreen-value (frame-parameter nil 'fullscreen)))
    (if (equal l-frame-fullscreen-value 'fullboth)
      ;; Switch back to previous state (nil or maximized)
      (set-frame-parameter nil 'fullscreen eide-windows-frame-fullscreen-value)
      (progn
        ;; Save current state (nil or maximized)
        (setq eide-windows-frame-fullscreen-value l-frame-fullscreen-value)
        ;; Switch to fullboth mode
        (set-frame-parameter nil 'fullscreen 'fullboth)))))

;;; eide-windows.el ends here
