;;; eide-windows.el --- Emacs-IDE, windows

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

(provide 'eide-windows)

(setq eide-windows-buffer-in-window-results nil)
(setq eide-buffer-compile nil)
(setq eide-buffer-run nil)
(setq eide-buffer-debug nil)
(setq eide-buffer-shell nil)


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-display-test-if-buffer-should-be-displayed-p (my-buffer-name)
  (if (or (string-equal my-buffer-name "TAGS") (string-equal my-buffer-name eide-options-file) (string-equal my-buffer-name eide-project-file) (string-equal my-buffer-name eide-project-notes-file) (string-match "^\*.*" my-buffer-name))
    nil
    t))

;; ----------------------------------------------------------------------------
;; If current buffer is a temporary buffer (starts with "*"), or should not be
;; displayed, switch to another one, until correct
;; ----------------------------------------------------------------------------
(defun eide-display-skip-unwanted-buffers (skip-current go-forward)
  (let ((should-we-continue t) (current-buffer-name (buffer-name)) (iteration 0))
    ;; Temporarily disable switch-to-buffer advice : buffers must be displayed
    ;; in window "file", until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    ;(eide-debug-print-trace (concat "current-buffer-name = " current-buffer-name))
    (if skip-current
      (progn
        (if go-forward
          (bs-cycle-previous)
          (bs-cycle-next))
        ;; To avoid list of buffers in minibuffer
        (message "")
        ;(eide-debug-print-trace (concat "bs-cycle-xxx => " (buffer-name)))
        (setq first-found-buffer-name (buffer-name)))
      (setq first-found-buffer-name nil))
    (while (and (not (eide-display-test-if-buffer-should-be-displayed-p (buffer-name)))
                should-we-continue
                (< iteration 10))
      (progn
        (if go-forward
          (bs-cycle-previous)
          (bs-cycle-next))
        ;; To avoid list of buffers in minibuffer
        (message "")
        ;(eide-debug-print-trace (concat "bs-cycle-xxx => " (buffer-name)))
        (if (and (= iteration 0) (not first-found-buffer-name))
          (setq first-found-buffer-name (buffer-name))
          (if (string-equal (buffer-name) first-found-buffer-name)
            ;; We have parsed the whole buffer list without finding any other
            ;; buffer that fits. Moreover, current buffer cannot be found again
            ;; (because bs-cycle-xxx ignores temporary buffers), which means
            ;; that it is not valid either. Let's display "*scratch*".
            (progn
              ;(eide-debug-print-trace (concat "back to first found buffer = " (buffer-name)))
              (switch-to-buffer "*scratch*")
              ;(eide-debug-print-trace (concat "force buffer *scratch* => " (buffer-name)))
              )))
        (if (string-equal (buffer-name) current-buffer-name)
          (progn
            ;; We have parsed the whole buffer list without finding any other
            ;; buffer that fits. If this buffer is valid, let's keep it
            ;; current. Otherwise, let's display "*scratch*".
            ;(eide-debug-print-trace (concat "back to current buffer = " (buffer-name)))
            (setq should-we-continue nil)
            (if (not (eide-display-test-if-buffer-should-be-displayed-p (buffer-name)))
              (progn
                (switch-to-buffer "*scratch*")
                ;(eide-debug-print-trace (concat "force buffer *scratch* => " (buffer-name)))
                ))))
        (setq iteration (1+ iteration))))
    (ad-activate 'switch-to-buffer)
    ;; Update menu (switch-to-buffer advice was disabled)
    (eide-menu-update nil)))

;; ----------------------------------------------------------------------------
;; Display buffer in appropriate window
;; Called for grep (new "*grep*" buffer), and when clicking on a grep result
;; (switch to buffer to display match)
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-special-display-function (my-buffer &optional not-this-window frame)
  (let ((my-buffer-name) (my-window) (my-selected-window) (my-completion-height))
  (if (bufferp my-buffer)
    (setq my-buffer-name (buffer-name my-buffer))
    (setq my-buffer-name my-buffer))
  ;(eide-debug-print-trace (concat "eide-windows-internal-special-display-function : " my-buffer-name))
  (if (string-equal my-buffer-name eide-menu-buffer)
    (setq my-window eide-windows-window-menu)
    (if (and (string-match "^\*.*" my-buffer-name) (not (string-match "\* clean : *" my-buffer-name)))
      (setq my-window eide-windows-window-results)
      (setq my-window eide-windows-window-file)))
  (set-window-buffer my-window my-buffer)
  ;; Run buffer name is updated asynchronously (see eide-project-internal-run)
  (if eide-menu-update-run-buffer
    (progn
      ;; Use get-buffer because my-buffer may be a buffer or a buffer name
      (setq eide-buffer-run (buffer-name (get-buffer my-buffer)))
      (eide-toolbar-update)
      (setq eide-menu-update-run-buffer nil)))
  ;; Update menu when a buffer is displayed by grep result
  (if (equal my-window eide-windows-window-file)
    (eide-menu-update nil))
  (if (string-equal my-buffer-name "*Completions*")
    (progn
      (setq my-selected-window (selected-window))
      (select-window my-window)
      ;; Window "results" temporarily expands to half or 2/3 of the frame to
      ;; display completions
      (setq my-completion-height (max (+ (count-lines (point-min) (point-max)) 2) (/ (frame-height) 2)))
      (if (> my-completion-height (/ (frame-height) 2))
        (setq my-completion-height (/ (* (frame-height) 2) 3)))
;      (if (< (window-height) my-completion-height)
      (enlarge-window (- my-completion-height (window-height))) ;)
      (select-window my-selected-window)))
  (setq my-window my-window)))

;(setq special-display-regexps '(".*"))
;(setq special-display-function 'eide-windows-internal-special-display-function)
;(setq display-buffer-function 'eide-windows-internal-special-display-function)

;; ----------------------------------------------------------------------------
;; Override switch-to-buffer function
;; ----------------------------------------------------------------------------
(defadvice switch-to-buffer (around eide-switch-to-buffer-advice-around (my-buffer))
  (let ((my-buffer-name) (my-really-switch))
  (if (bufferp my-buffer)
    (setq my-buffer-name (buffer-name my-buffer))
    (setq my-buffer-name my-buffer))
  (save-excursion
    (set-buffer my-buffer-name)
    ;; Do not display "dired mode", TAGS file, and configuration files
    (if (or (equal major-mode 'dired-mode) (string-equal my-buffer-name "TAGS") (string-equal my-buffer-name eide-options-file) (string-equal my-buffer-name eide-project-file) (string-equal my-buffer-name eide-project-notes-file))
      (setq my-really-switch nil)
      (setq my-really-switch t)))
  (if my-really-switch
    (progn
      ;(eide-debug-print-trace (concat "switch-to-buffer : " my-buffer-name))
      (if (not (string-equal " SPEEDBAR" my-buffer-name))
        (if eide-windows-is-layout-visible-flag
          (if (and (string-match "^\*.*" my-buffer-name) (not (string-match "\* clean : *" my-buffer-name)))
            (if (string-equal "* Toolbar *" my-buffer-name)
              (eide-windows-select-window-toolbar t)
              (eide-windows-select-window-results t))
            (eide-windows-select-window-file t))))
      ad-do-it
      (if (not (string-equal eide-menu-symbol-definition-to-find "-"))
        (progn
;          (if (not eide-windows-is-layout-visible-flag)
;            (delete-other-windows))
          (recenter)
          (setq eide-menu-symbol-definition-to-find "-")))
      ; TODO : one window !
      ;; Update menu if necessary
      (if (not (string-equal " SPEEDBAR" my-buffer-name))
        (if (or (not (string-match "^\*.*" my-buffer-name)) (string-match "\* clean : *" my-buffer-name))
          ;; Buffer = file buffer : update menu
          (eide-menu-update nil)
          ;; Buffer = result buffer : update toolbar
          (if eide-windows-is-layout-visible-flag
            (eide-toolbar-update))))
      ;; Desktop is saved to avoid loss of opened buffers in case of crash of emacs
      (if (file-exists-p (concat eide-project-directory ".emacs.desktop"))
        (desktop-save eide-project-directory)))
    (progn
      ;; Close unwanted files (except TAGS and project configuration)
      (if (or (equal major-mode 'dired-mode) (string-equal my-buffer-name eide-options-file) (string-equal my-buffer-name eide-project-notes-file))
        (progn
          ;(eide-debug-print-trace (concat "switch-to-buffer : discard dired-mode buffer " my-buffer-name))
          (kill-buffer my-buffer-name)))))))

;; ----------------------------------------------------------------------------
;; Override C-x C-f find-file
;; ----------------------------------------------------------------------------
(defun eide-windows-find-file ()
  (interactive)
  (eide-windows-select-window-file nil)
  (call-interactively 'find-file))

;; ----------------------------------------------------------------------------
;; Override save-buffer function
;; Always save buffer displayed in window "file"
;; ----------------------------------------------------------------------------
(defadvice save-buffer (around eide-save-buffer-advice-around)
  (let ((my-window))
    (setq my-window (selected-window))
    (eide-windows-select-window-file nil)
    ad-do-it
    (eide-menu-update-current-buffer-modified-status)
    (select-window my-window)))

;; ----------------------------------------------------------------------------
;; Hook to be called once frame has been resized
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-window-setup-hook ()
;  (setq eide-windows-results-window-height (/ (frame-height) 5))
;  (setq eide-windows-menu-window-width (/ (frame-width) 4))

  ;; Close buffer "*Buffer List*" (created when emacs is launched with files as
  ;; parameters)
  (if (string-equal (buffer-name) "*Buffer List*")
    (kill-this-buffer))

  (setq eide-windows-results-window-height 9)
  (setq eide-windows-menu-window-width 40)
  (eide-windows-layout-build)
  (ad-activate 'switch-to-buffer)
  (ad-activate 'save-buffer)

  (eide-display-skip-unwanted-buffers nil nil)
  ;; Create menu content
  (eide-menu-update t)
  (eide-toolbar-update))

;; ----------------------------------------------------------------------------
;; Select window at mouse position
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-select-window-at-mouse-position ()
  ;; Select the window where the mouse is
;  (let ((my-position (last (mouse-position))))
;    (let ((my-x (car my-position)) (my-y (cdr my-position)))
;      (select-window (window-at my-x my-y)))))
  (let ((my-position (last (mouse-position))))
    (select-window (window-at (car my-position) (cdr my-position)))))

;; ----------------------------------------------------------------------------
;; Test if selected window is "file"
;;
;; return : t / nil
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-is-window-file-selected-p ()
  (equal (selected-window) eide-windows-window-file))

;; ----------------------------------------------------------------------------
;; Test if selected window is "menu"
;;
;; return : t / nil
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-is-window-menu-selected-p ()
  (equal (selected-window) eide-windows-window-menu))

;; ----------------------------------------------------------------------------
;; Test if selected window is "results"
;;
;; return : t / nil
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-is-window-results-selected-p ()
  (equal (selected-window) eide-windows-window-results))

;; ----------------------------------------------------------------------------
;; Test if selected window is "toolbar"
;;
;; return : t / nil
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-is-window-toolbar-selected-p ()
  (equal (selected-window) eide-windows-window-toolbar))

;; ----------------------------------------------------------------------------
;; Resize layout : up
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-layout-resize-up ()
  (if (and eide-windows-is-layout-visible-flag
           (< eide-windows-results-window-height (- (frame-height) 8)))
    (progn
      (eide-windows-layout-unbuild)
      (setq eide-windows-results-window-height (1+ eide-windows-results-window-height))
      (eide-windows-layout-build))))

;; ----------------------------------------------------------------------------
;; Resize layout : down
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-layout-resize-down ()
  (if (and eide-windows-is-layout-visible-flag
           (> eide-windows-results-window-height 3))
    (progn
      (eide-windows-layout-unbuild)
      (setq eide-windows-results-window-height (1- eide-windows-results-window-height))
      (eide-windows-layout-build))))

;; ----------------------------------------------------------------------------
;; Resize layout : enlarge window "menu"
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-layout-resize-enlarge-menu ()
  (if (and eide-windows-is-layout-visible-flag
           (< eide-windows-menu-window-width (- (frame-width) 16)))
    (progn
      (eide-windows-layout-unbuild)
      (setq eide-windows-menu-window-width (1+ eide-windows-menu-window-width))
      (eide-windows-layout-build))))

;; ----------------------------------------------------------------------------
;; Resize layout : shrink window "menu"
;; ----------------------------------------------------------------------------
(defun eide-windows-internal-layout-resize-shrink-menu ()
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
;; Init windows
;; ----------------------------------------------------------------------------
(defun eide-windows-init ()
  (add-hook 'window-setup-hook 'eide-windows-internal-window-setup-hook))

;; ----------------------------------------------------------------------------
;; Split current single window display into 3 windows
;; (file, menu and results)
;; ----------------------------------------------------------------------------
(defun eide-windows-layout-build ()
  (delete-other-windows)

  ;; Create window "toolbar" now if position is "top" or "bottom"
  (if (and eide-custom-use-toolbar (or (string-equal eide-custom-toolbar-position "top")
                                       (string-equal eide-custom-toolbar-position "bottom")))
    (progn
      (split-window-vertically)
      (if (string-equal eide-custom-toolbar-position "bottom")
        (select-window (next-window)))
      (setq eide-windows-window-toolbar (selected-window))
      (select-window (next-window))))

  ;; Split into 3 windows ("file", "menu", "results")
  (if (string-equal eide-custom-menu-height "full")
    (progn
      (split-window-horizontally)
      (if (string-equal eide-custom-menu-position "left")
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
      (if (string-equal eide-custom-menu-position "left")
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
  (if (and eide-custom-use-toolbar (string-equal eide-custom-toolbar-position "middle"))
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
  (switch-to-buffer eide-menu-buffer)
  ;; This window should be used for this buffer only
  (set-window-dedicated-p eide-windows-window-menu t)
;  (setq window-min-width 1) ; TODO : sans effet ?
  (enlarge-window-horizontally (- eide-windows-menu-window-width (window-width)))

  ;; Window "toolbar"
  (if eide-custom-use-toolbar
    (progn
      (select-window eide-windows-window-toolbar)
      (switch-to-buffer eide-toolbar-buffer)
      ;; This window should be used for this buffer only
      (set-window-dedicated-p eide-windows-window-toolbar t)
      ;; Resize window "toolbar"
      (setq window-min-height 1)
      (if (and (string-equal eide-custom-toolbar-position "middle")
               (string-equal eide-custom-menu-height "full")
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
  (setq display-buffer-function 'eide-windows-internal-special-display-function))

;; ----------------------------------------------------------------------------
;; Close windows layout (keep window "file" only)
;; (file, menu and results)
;; ----------------------------------------------------------------------------
(defun eide-windows-layout-unbuild ()
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Remember window "menu" width
      (eide-windows-select-window-menu t)
      (setq eide-windows-menu-window-width (window-width))
      ;; Remember window "results" height
      (eide-windows-select-window-results t)
      (setq eide-windows-results-window-height (window-height))
      ;; Remember which result buffer is displayed in window "results"
      (setq eide-windows-buffer-in-window-results (buffer-name))
      (if eide-custom-use-toolbar
        (progn
          ;; Enable resizing of window "toolbar", otherwise it can't be deleted
          (eide-windows-select-window-toolbar t)
          (setq window-size-fixed nil)))
      ;; Keep only window "file"
      (eide-windows-select-window-file t)
      (delete-other-windows)

      (setq eide-windows-is-layout-visible-flag nil)
      (setq display-buffer-function nil))))

;; ----------------------------------------------------------------------------
;; Move to window "file" (left) (split into 3 windows if not visible)
;; ----------------------------------------------------------------------------
(defun eide-windows-select-window-file (force-build-flag)
  (if (or eide-windows-is-layout-visible-flag force-build-flag)
    (progn
      (if (not eide-windows-is-layout-visible-flag)
        (eide-windows-layout-build))
      (select-window eide-windows-window-file))))

;; ----------------------------------------------------------------------------
;; Move to window "menu" (right) (split into 3 windows if not visible)
;; ----------------------------------------------------------------------------
(defun eide-windows-select-window-menu (force-build-flag)
  (if (or eide-windows-is-layout-visible-flag force-build-flag)
    (progn
      (if (not eide-windows-is-layout-visible-flag)
        (eide-windows-layout-build))
      (select-window eide-windows-window-menu))))

;; ----------------------------------------------------------------------------
;; Move to window "results" (bottom) (split into 3 windows if not visible)
;; ----------------------------------------------------------------------------
(defun eide-windows-select-window-results (force-build-flag)
  (if (or eide-windows-is-layout-visible-flag force-build-flag)
    (progn
      (if (not eide-windows-is-layout-visible-flag)
        (eide-windows-layout-build))
      (select-window eide-windows-window-results))))

;; ----------------------------------------------------------------------------
;; Move to window "toolbar" (split into 3 windows if not visible)
;; ----------------------------------------------------------------------------
(defun eide-windows-select-window-toolbar (force-build-flag)
  (if (or eide-windows-is-layout-visible-flag force-build-flag)
    (progn
      (if (not eide-windows-is-layout-visible-flag)
        (eide-windows-layout-build))
      (select-window eide-windows-window-toolbar))))

;; ----------------------------------------------------------------------------
;; Handle mouse-3 action
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-mouse-3 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-windows-internal-select-window-at-mouse-position)

  (if (string-equal (buffer-name) "* Help *")
    ;; Close "help"
    (progn
      (kill-buffer "* Help *")
      (eide-set-background-color-for-files)
      (eide-key-bindings-configure-for-editor)
      (eide-windows-layout-build))
    (if (string-equal (buffer-name) eide-options-file)
      ;; Close ".emacs-ide.options"
      (progn
        (save-buffer)
        (eide-custom-rebuild-options-file)
        (eide-custom-apply-options)
        (kill-buffer eide-options-file)
        (eide-set-background-color-for-files)
        (eide-key-bindings-configure-for-editor)
        (eide-windows-layout-build)
        ;; Toolbar might have been modified in options
        (eide-toolbar-update))
      (if (string-equal (buffer-name) eide-project-file)
        ;; Display another buffer (other than ".emacs-ide.project")
        (progn
          (save-buffer)
          (eide-custom-rebuild-project-file)
          (eide-custom-apply-project-configuration)
          ;; This buffer must not be closed
          (switch-to-buffer eide-current-buffer)
          (eide-set-background-color-for-files)
          (eide-key-bindings-configure-for-editor)
          (eide-windows-layout-build))
        (if (string-equal (buffer-name) eide-project-notes-file)
          ;; Close ".emacs-ide.project_notes"
          (progn
            (save-buffer)
            (kill-buffer eide-project-notes-file)
            (eide-set-background-color-for-files)
            (eide-key-bindings-configure-for-editor)
            (eide-windows-layout-build))
          (progn
            ;; mark-active doesn't exist in xemacs
            (if (and (not (featurep 'xemacs)) (eq mark-active t))
              ;; Text is selected
              (if (= (count-screen-lines (region-beginning) (region-end) t) 1)
                ;; Text is selected on a single line
                (eide-popup-open-menu-for-search)
                ;; Text is selected on several lines
                (eide-popup-open-menu-for-cleaning))
              ;; No text selected
              (progn
                (if (eide-windows-internal-is-window-results-selected-p)
                  ;; Window "results" : open grep results popup menu
                  (eide-popup-open-menu-for-search-results)
                  (if (eide-windows-internal-is-window-menu-selected-p)
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
;; Handle mouse-2 action
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-mouse-2 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-windows-internal-select-window-at-mouse-position)

  (if (and eide-windows-is-layout-visible-flag (eide-windows-internal-is-window-menu-selected-p))
    (eide-menu-speedbar-open)
    (yank)))

;; ----------------------------------------------------------------------------
;; Handle control + mouse-4 action (wheel up)
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-control-mouse-4 ()
  (interactive)
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Select the window where the mouse is
      (eide-windows-internal-select-window-at-mouse-position)
      (if (eide-windows-internal-is-window-menu-selected-p)
        (eide-windows-internal-layout-resize-enlarge-menu)
        (eide-windows-internal-layout-resize-up)))))

;; ----------------------------------------------------------------------------
;; Handle control + mouse-5 action (wheel down)
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-control-mouse-5 ()
  (interactive)
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Select the window where the mouse is
      (eide-windows-internal-select-window-at-mouse-position)
      (if (eide-windows-internal-is-window-menu-selected-p)
        (eide-windows-internal-layout-resize-shrink-menu)
        (eide-windows-internal-layout-resize-down)))))

;; ----------------------------------------------------------------------------
;; Handle shift + mouse-4 action (wheel up)
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-shift-mouse-4 ()
  (interactive)
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Select the window where the mouse is
      (eide-windows-internal-select-window-at-mouse-position)
      (if (eide-windows-internal-is-window-menu-selected-p)
        (eide-windows-internal-layout-resize-enlarge-menu)
        (eide-windows-internal-layout-resize-up)))))

;; ----------------------------------------------------------------------------
;; Handle shift + mouse-5 action (wheel down)
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-shift-mouse-5 ()
  (interactive)
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Select the window where the mouse is
      (eide-windows-internal-select-window-at-mouse-position)
      (if (eide-windows-internal-is-window-menu-selected-p)
        (eide-windows-internal-layout-resize-shrink-menu)
        (eide-windows-internal-layout-resize-down)))))

;; ----------------------------------------------------------------------------
;; Handle shift-mouse-3 action
;; ----------------------------------------------------------------------------
(defun eide-windows-handle-shift-mouse-3 ()
  (interactive)
  ;; Select the window where the mouse is
  (eide-windows-internal-select-window-at-mouse-position)

  (if (eide-windows-internal-is-window-results-selected-p)
    (eide-popup-open-menu-for-search-results-delete)))

;;; eide-windows.el ends here
