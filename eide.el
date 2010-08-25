;;; eide.el --- Emacs-IDE

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

;; *****************************************************************************
;; * MEMO                                                                      *
;; *****************************************************************************

;; options : M-x customize
;; M-x replace-string C-Q 0 0 1 5 : pour supprimer les ^M
;; C-h C-f : fonction
;; C-h   v : variable
;; C-h   b : liste des raccourcis claviers

;;     M-. tag
;; C-u M-. prochain tag

;; M-x list-faces-display : liste des styles utilisés
;; M-x list-color-display : liste des couleurs disponibles

;; ^M
;(setq comint-output-filter-functions (remove 'comint-carriage-motion 'comint-output-filter-functions))


;;;; ==========================================================================
;;;; SETTINGS
;;;; ==========================================================================

(setq eide-version "1.0")
(setq eide-release-date "12/2008")

(setq eide-options-file       ".emacs-ide.options")
(setq eide-project-file       ".emacs-ide.project")
(setq eide-project-notes-file ".emacs-ide.project_notes")
;(setq eide-project-lock-file  ".emacs-ide.project_lock")

;; Print debug information in eide-debug-string
(setq eide-debug-flag nil)
(setq eide-debug-string "")

;; ----------------------------------------------------------------------------
;; Debug
;; ----------------------------------------------------------------------------
(defun eide-debug-print-trace (string)
  (if eide-debug-flag
    (setq eide-debug-string (concat eide-debug-string string " --- "))))


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Cleaning before exit
;; ----------------------------------------------------------------------------
;(defun eide-kill-emacs-hook ()
;  (if (and eide-project-name (not eide-project-already-open-flag))
;    ;; "Unlock" project
;    (shell-command (concat "rm -f " eide-project-directory eide-project-lock-file)))
;  (if (string-equal (buffer-name) eide-project-file)
;    ;; Rebuild project configuration file (because project_type might be lost
;    ;; otherwise !)
;    (progn
;      (save-buffer)
;      (eide-custom-rebuild-project-file))))


;;;; ==========================================================================
;;;; LIBRARIES
;;;; ==========================================================================

(require 'desktop)
(require 'hideshow)
(require 'imenu)
(require 'mwheel)

;(require 'gud)

;; Test if xcscope is available
(if (locate-library "xcscope")
  (progn
    (require 'xcscope)
    (setq eide-option-use-cscope-flag t))
  (setq eide-option-use-cscope-flag nil))


;;;; ==========================================================================
;;;; ENVIRONMENT SPECIFIC SETTINGS
;;;; ==========================================================================

;; Directory for including other modules
;; file-truename follows symbolic link .emacs (if any)
;; file-name-directory retrieves directory path (removes file name)
;; directory-file-name removes final "/"
(setq eide-emacs-path (directory-file-name (file-name-directory (file-truename user-init-file))))
(add-to-list 'load-path eide-emacs-path)

(eval-when-compile (require 'eide-custom))
(eval-when-compile (require 'eide-project))
(eval-when-compile (require 'eide-edit))
(eval-when-compile (require 'eide-search))
(eval-when-compile (require 'eide-compare))

(eval-when-compile (require 'eide-popup))
(eval-when-compile (require 'eide-menu))
(eval-when-compile (require 'eide-windows))
(eval-when-compile (require 'eide-toolbar))

(eval-when-compile (require 'eide-key-bindings))


;; ----------------------------------------------------------------------------
;; Set background color for "menu" buffer
;; ----------------------------------------------------------------------------
(defun eide-set-background-color-for-config ()
  (if (featurep 'xemacs)
    (set-face-background 'default eide-option-color-background-for-config)
    (progn
      (set-background-color eide-option-color-background-for-config)
      (set-face-background 'fringe eide-option-color-background-for-config))))

;; ----------------------------------------------------------------------------
;; Set background color for "menu" buffer
;; ----------------------------------------------------------------------------
(defun eide-set-background-color-for-help ()
  (if (featurep 'xemacs)
    (set-face-background 'default eide-option-color-background-for-menu)
    (progn
      (set-background-color eide-option-color-background-for-menu)
      (set-face-background 'fringe eide-option-color-background-for-menu))))

;; ----------------------------------------------------------------------------
;; Set background color for all buffers but "menu" buffer
;; ----------------------------------------------------------------------------
(defun eide-set-background-color-for-files ()
  (if (featurep 'xemacs)
    (set-face-background 'default eide-option-color-background)
    (progn
      (set-background-color eide-option-color-background)
      (set-face-background 'fringe eide-option-color-background))))

;; ----------------------------------------------------------------------------
;; Insert chapter title in "help" buffer
;;
;; input  :   string : string to insert
;; ----------------------------------------------------------------------------
(defun eide-help-internal-insert-header-1 (string)
  (insert "\n\n\n")
  (put-text-property (point) (progn (insert (concat string "\n")) (point)) 'face 'eide-menu-chapter1-face)
  (insert "\n"))

;; ----------------------------------------------------------------------------
;; Insert chapter title in "help" buffer
;;
;; input  :   string : string to insert
;; ----------------------------------------------------------------------------
(defun eide-help-internal-insert-header-2 (string)
  (insert "\n")
  (put-text-property (point) (progn (insert (concat string "\n")) (point)) 'face 'eide-menu-chapter2-face))

;; ----------------------------------------------------------------------------
;; Display Help (full frame)
;; ----------------------------------------------------------------------------
(defun eide-help-open ()
  (interactive)
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  ;; Close menu
  (eide-windows-layout-unbuild)
  (eide-set-background-color-for-help)
  (eide-key-bindings-configure-for-special-buffer)

  (if (get-buffer "* Help *")
    (kill-buffer "* Help *"))
  (switch-to-buffer (get-buffer-create "* Help *"))

  (insert (concat "\nEmacs-IDE\nversion " eide-version "  (" eide-release-date ")\n\n"))

  (insert "(right click to exit help)")

  (eide-help-internal-insert-header-1 "Windows layout")

  (eide-help-internal-insert-header-2 "Overview")

  (insert "
With default options, windows layout should look like this :

  -----------------------------------------------------------
  |                                         |               |
  |                                         |               |
  |       Window 'file'                     | Window 'menu' |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  -----------------------------------------------------------
  |               Window 'toolbar'                          |
  -----------------------------------------------------------
  |                                                         |
  |               Window 'results'                          |
  |                                                         |
  -----------------------------------------------------------

You can modify the layout in options.
")

  (eide-help-internal-insert-header-2 "Resizing")

  (insert "
You can resize all windows, except window 'toolbar'.

Since Emacs behaviour seems to be quite unpredictable (depending on the
version) about window resizing using mouse drag, some new key bindings have
been defined (see mouse wheel below).
")

  (eide-help-internal-insert-header-1 "Mouse actions")

  (eide-help-internal-insert-header-2 "Right click (when no text is selected)")

  (insert "
Right click behaviour depends on mouse position :

In window 'file' :
    Hide/show other windows (to view code full screen).

In window 'menu' :
    Open project popup menu :
    - help (this page)
    - project creation/configuration
    - options

In window 'menu', over a file name :
    Open file popup menu :
    - R/W permissions
    - REF/NEW switching and comparison
      (see 'Actions on files' below)
    - cleaning

In window 'results' :
    Open results popup menu (to display existing 'grep' or 'cscope' result).

Shift + right click behaviour depends on mouse position :

In window 'results' :
    Open results deletion popup menu (to delete existing 'grep' or 'cscope'
    result).
")

  (eide-help-internal-insert-header-2 "Right click (when text is selected)")

  (insert "
If text is selected on a single line :
    Open search popup menu, for selected string.

If text is selected over several lines :
    Open cleaning popup menu (to untabify or indent selection).
")

  (eide-help-internal-insert-header-2 "Middle click")

  (insert "
Middle click behaviour depends on mouse position :

In window 'menu' :
    Open speedbar (to open a file).

In other windows :
    Paste (standard behaviour).
")

  (eide-help-internal-insert-header-2 "Wheel")

  (insert "
Shift + mouse wheel up/down scrolls right/left.

Control + mouse wheel up/down behaviour depends on mouse position :

In window 'menu' :
    Enlarges/shrinks window 'menu'.

In other windows :
    Enlarges/shrinks window 'results'.
")
  (eide-help-internal-insert-header-1 "Options")

  (insert "
Options are saved in a file, '.emacs-ide.options', in your home directory. This
file is created as soon as you launch emacs. To edit options, open project
popup menu and select 'Options'.

Options cover topics such as display, coding rules, and default parameters for
new projects (see '.emacs-ide.project' below).

If you delete this file, it will be created again with default values.
If you delete any parameter in this file, it will be restored with default
value.
")

  (eide-help-internal-insert-header-1 "Work on a project")

  (eide-help-internal-insert-header-2 "Create a project")

  (insert "
Launch emacs from your workset root directory.
Open project popup menu and select 'Create project' (either C or Lisp).

In your workset root directory, several files are created :

- TAGS :
  Tags database.

- cscope.files (for C projects only) :
  Cscope list of files.

- .emacs-ide.project :
  It defines parameters for this project.
  It is created with default values from '~/.emacs-ide.options'.

- .emacs-ide.project_notes :
  It can be used to write notes about the project, it is not used by emacs.
  It is created empty.

To edit project configuration ('.emacs-ide.project'), open project popup menu
and select 'Project configuration'.

To edit project notes ('.emacs-ide.project_notes'), open project popup menu and
select 'Project notes'.
")

  (eide-help-internal-insert-header-2 "Open existing project")

  (insert "
Launch emacs from your workset root directory.
Files opened during last session are opened again automatically.
")

  (eide-help-internal-insert-header-2 "Tags and cscope update")

  (insert "
When code is changed :
- Tags database ('TAGS') needs to be updated.
- Cscope database ('cscope.out') doesn't need to be updated (it is done
  automatically each time you search).

When a file is added or deleted :
- Tags database ('TAGS') needs to be updated.
- Cscope list of files ('cscope.files') needs to be updated ('cscope.out' will
  be updated automatically on next search).
")

  (eide-help-internal-insert-header-1 "Actions on files")

  (eide-help-internal-insert-header-2 "Editing with REF files")

  (insert "
When editing a file, you can create a copy, so as to easily switch between
original and modified file, and compare them.

Original version of 'file' is saved as 'file.ref'.
When switching to original file, 'file' becomes 'file.new', and 'file.ref'
becomes 'file'.

File popup menu actions :
- Backup original file : create a copy of 'file' ('file.ref'), and set
                         read/write permission on 'file'.
- Switch to REF file   : switch to original version ('file.ref').
- Switch to NEW file   : switch to modified version ('file.new').
- Restore REF          : discard modified file, and restore original file.
- Discard REF          : discard original file, and use modified file.

File name colour :
- green when modified file is used.
- red when original file is used.
")

  (eide-help-internal-insert-header-2 "Other actions on files")

  (insert "
File popup menu actions :
- Read/write : Set read/write permission on file.
- Read only  : Set read only permission on file.
- Clean      : Clean file (turn tabs into spaces, indent).

File name colour :
- black when file is read/write.
- grey when file is read only.
")
  (eide-help-internal-insert-header-1 "Standard key bindings")
  (insert "
Control-x Control-f ........... load a file
Control-x Control-s ........... save current file
Control-s ..................... search
Alt-% ......................... replace
Control-_ ..................... undo
")

  (eide-help-internal-insert-header-1 "New key bindings")

  (eide-help-internal-insert-header-2 "Editing")

  (insert "
    Alt - left ........ Cut
    Alt - down ........ Copy
    Alt - right ....... Paste

Control - mouse 1 ..... Cut
Control - mouse 2 ..... Copy
Control - mouse 3 ..... Paste
")

  (eide-help-internal-insert-header-2 "Code browsing with tags/cscope")

  (insert "
          F1 .......... Back from symbol definition
          F2 .......... Go to symbol definition (at cursor position, or selected text if any)
  Shift - F2 .......... Go to symbol definition (prompt for symbol)
  Shift - F1 .......... Go to alternative definition (after F2 or Shift - F2)
          F3 .......... Search symbol in whole project (at cursor position, or selected text if any)
  Shift - F3 .......... Search symbol in whole project (prompt for symbol)
")

  (eide-help-internal-insert-header-2 "Grep search")

  (insert "
          F4 .......... Search string in whole project (at cursor position, or selected text if any)
  Shift - F4 .......... Search string in whole project (prompt for string)
          F6 .......... Search string in current directory (at cursor position, or selected text if any)
  Shift - F6 .......... Search string in current directory (prompt for string)
")

  (eide-help-internal-insert-header-2 "{...} block hiding")

  (insert "
Control - F1 .......... Hide block
Control - F2 .......... Show block
Control - F3 .......... Hide all blocks in current buffer
Control - F4 .......... Show all blocks in current buffer
")

  (eide-help-internal-insert-header-2 "Display")

  (insert "
          F5 .......... Reload current buffer (and update display)
  Shift - F5 .......... Close current buffer
")

  (eide-help-internal-insert-header-2 "Grep result browsing")

  (insert "
          F7 .......... Go to previous instance
          F8 .......... Go to next instance
")

  (eide-help-internal-insert-header-2 "Compilation error browsing")

  (insert "
          F7 .......... Go to previous error
          F8 .......... Go to next error
")

  (eide-help-internal-insert-header-2 "Unix Shell commands")

  (insert "
          F9 .......... Compile (1)
  Shift - F9 .......... Compile (2)
          F10 ......... Run (1)
  Shift - F10 ......... Run (2)
          F11 ......... Debug (1)
  Shift - F11 ......... Debug (2)
          F12 ......... Open shell
")

;  (eide-help-internal-insert-header-2 "User defined text insertions")

;  (insert "
;Control - Shift - F1 .. Insert text #1
;Control - Shift - F2 .. Insert text #2
;")

  (eide-help-internal-insert-header-1 "Windows layout overview during diff session")
  (insert "
  -----------------------------------------------------------
  |                             |                           |
  |                             |                           |
  |     Window 'file A'         |     Window 'file B'       |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  -----------------------------------------------------------
  |                     Window 'control'                    |
  -----------------------------------------------------------
")
  (eide-help-internal-insert-header-1 "Standard key bindings in diff session")
  (insert "
These commands must be typed in window 'control' :

! ..................... update diffs
<backspace> ........... go to previous diff
<space> ............... go to next diff
a ..................... copy highlighted region A --> B
b ..................... copy highlighted region A <-- B
wa .................... save file A
wb .................... save file B
q ..................... quit (y to confirm)
? ..................... display help
")
  (eide-help-internal-insert-header-1 "New key bindings in diff session")
  (insert"
F1 .................... copy highlighted region A --> B
F2 .................... copy highlighted region A <-- B
F5 .................... update diffs
F7 .................... go to previous diff
F8 .................... go to next diff
<right click> ......... quit (yes to confirm)

")
  (setq buffer-read-only t)
  (goto-char (point-min)))

;; ----------------------------------------------------------------------------
;; Build the lists of buffers
;;
;; output :  eide-menu-source-file-list : list of source file buffers
;;           eide-menu-header-file-list : list of header file buffers
;;           eide-menu-other-file-list  : list of other file buffers
;;           eide-menu-grep-result-list : list of grep result buffers
;; ----------------------------------------------------------------------------
(defun eide-files-build-lists ()
  (setq eide-menu-all-file-list nil)
  (setq eide-menu-source-file-list nil)
  (setq eide-menu-header-file-list nil)
  (setq eide-menu-other-file-list nil)
  (setq eide-menu-grep-result-list nil)
  (setq eide-menu-cscope-result-list nil)

  (setq my-buffer-name-list (mapcar 'buffer-name (buffer-list)))
  (setq my-buffer-name-list (sort my-buffer-name-list 'string<))
  (setq my-buffer-name-list (reverse my-buffer-name-list))

  (dolist (this-buffer-name my-buffer-name-list)
    (if (not (or (string-match "^[ \*]" this-buffer-name) (string-equal "TAGS" this-buffer-name)))
      ;; This is a "useful" buffer
      (progn
        (save-excursion
          (set-buffer this-buffer-name)
          (if (or (equal major-mode 'dired-mode) (string-equal this-buffer-name eide-project-file))
            (setq my-really-file nil)
            (setq my-really-file t)))
        (if my-really-file
          (progn
            (setq eide-menu-all-file-list (cons this-buffer-name eide-menu-all-file-list))
            (if (string-match source-file-regexp this-buffer-name)
              (setq eide-menu-source-file-list (cons this-buffer-name eide-menu-source-file-list))
              (if (string-match header-file-regexp this-buffer-name)
                (setq eide-menu-header-file-list (cons this-buffer-name eide-menu-header-file-list))
                (setq eide-menu-other-file-list (cons this-buffer-name eide-menu-other-file-list)))))
          (if (equal major-mode 'dired-mode)
            (kill-buffer this-buffer-name))))
      ;; This is a "*..." buffer
      (if (string-match "^\\*grep.*" this-buffer-name)
        (setq eide-menu-grep-result-list (cons this-buffer-name eide-menu-grep-result-list))
        (if (string-match "^\\*cscope\\*.*" this-buffer-name)
          (setq eide-menu-cscope-result-list (cons this-buffer-name eide-menu-cscope-result-list)))))))

;; ----------------------------------------------------------------------------
;; Run shell
;; ----------------------------------------------------------------------------
(defun eide-shell-open ()
  (interactive)
  (eide-windows-select-window-file t)
  (setq my-buffer-directory (file-name-directory (buffer-file-name)))
  (eide-windows-select-window-results t)
  ;; sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer !
  (switch-to-buffer "*results*")
  (shell)
  (send-invisible (concat "cd " my-buffer-directory " ; echo"))
  ;; Switch to shell window
  (setq eide-buffer-shell (buffer-name))
  (eide-toolbar-update)
  (eide-windows-select-window-results t))

;; ----------------------------------------------------------------------------
;; Define key action for a map
;; ----------------------------------------------------------------------------
(defun eide-set-map-action (my-map my-function)
  (setq my-map (make-sparse-keymap))
  (if (featurep 'xemacs)
    (define-key my-map [button1] my-function)
    (define-key my-map [mouse-1] my-function)))
; TODO : ne fonctionne pas (arg my-function !)

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-display-view-clean-buffer ()
  (interactive)
  (setq my-buffer (buffer-name))
  (let ((my-clean-buffer (concat "* clean : " my-buffer)))
    (setq my-current-line (count-lines (point-min) (point)))

    (if (and (get-buffer my-clean-buffer)
             (> clean-buffer-date (float-time (visited-file-modtime)))
             (not (buffer-modified-p)))

      ;; Clean buffer already exists and is up to date
      ;; (i.e. original buffer not modified and older than clean buffer)
      (switch-to-buffer my-clean-buffer)

      ;; It is necessary to update the clean buffer
      (progn
        ;; Set clean buffer modification date
        (make-local-variable 'clean-buffer-date)
        (setq clean-buffer-date (float-time))

        (get-buffer-create my-clean-buffer)

        ;; Set clean buffer major mode (same as original buffer)
        (setq default-major-mode major-mode)
        (set-buffer-major-mode my-clean-buffer)
        (setq default-major-mode 'fundamental-mode)

        (switch-to-buffer my-clean-buffer)
        (insert-buffer my-buffer)
        (eide-edit-pretty-buffer)
        (vc-toggle-read-only)))

    ;; Go to corresponding line in clean buffer
    (goto-line my-current-line)))


;;;; ==========================================================================
;;;; PREFERENCES
;;;; ==========================================================================

;; Do not display startup message
(setq inhibit-startup-message t)

;; Do not save backup files (~)
(setq make-backup-files nil)

;; Do not save place in .emacs-places
(setq-default save-place nil)

;; No confirmation when refreshing buffer
(setq revert-without-query '(".*"))

;; Use 'y' and 'n' instead of 'yes' and 'no' for minibuffer questions
(fset 'yes-or-no-p 'y-or-n-p) 

;; Use mouse wheel (default for Windows but not for Linux)
(if (not (featurep 'xemacs))
  (mouse-wheel-mode 1))

;; Mouse wheel should scroll the window over which the mouse is
(setq mouse-wheel-follow-mouse t)

;; Set mouse wheel scrolling speed
(if (equal (safe-length mouse-wheel-scroll-amount) 1)
  ;; Old API
  (setq mouse-wheel-scroll-amount '(4 . 1))
  ;; New API
  (setq mouse-wheel-scroll-amount '(4 ((shift) . 1) ((control)))))

;; Keep cursor position when moving page up/down
(setq scroll-preserve-screen-position t)

;; Paper type for printing
(setq ps-paper-type 'a4)

;; Show end of buffer
;(setq-default indicate-empty-lines t)

;; No menu bar (emacs), no icons and tabs (xemacs)
(if window-system
  (if (featurep 'xemacs)
    (set-specifier menubar-visible-p nil)
    (progn
      (menu-bar-mode -1)
      (tool-bar-mode -1))))

;; "One line at a time" scrolling (default value is 0, which moves active line
;; to center - High value is necessary, otherwise it sometimes doesn't work !)
(setq-default scroll-conservatively 2000)

;; Four line margin for scrolling
(setq scroll-margin 4)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Disable beep
;(setq visible-bell t)
(setq ring-bell-function (lambda() ()))

;; Disable line truncation (use horizontal scrolling instead)
;(if (featurep 'xemacs)
;  (setq-default truncate-lines t))

;; Vertical scroll bar on the right
;; (default value : right for Windows, left for Linux)
(if (not (featurep 'xemacs))
  (set-scroll-bar-mode 'right))

; Différents tests (sans succès) pour que le fait de sélectionner du texte ne
; le place pas automatiquement dans le kill-ring :

;(defun my-pop-kill-ring()
;  (interactive)
;  (current-kill 1))

;(add-hook 'deactivate-mark-hook 'my-pop-kill-ring)

;(defun my-before-change-function(my-beg my-end)
;  (interactive)
;  (if (not (eq my-beg my-end))
;    (current-kill 1))
;)

;(setq before-change-functions '(my-before-change-function))

;(defadvice mouse-set-region (around kill-to-primary last act) 
;  "i want the mouse drags to copy only to primary selection" 
;  (let ((x-select-enable-clipboard nil)) 
;        ad-do-it)) 

;(setq interprogram-cut-function nil 
;      interprogram-paste-function nil) 

;(setq mouse-sel-default-bindings 'interprogram-cut-paste)
;(require 'mouse-sel)
;(setq mouse-sel-default-bindings 'interprogram-cut-paste)

;(defvar mouse-sel-default-bindings nil)
;(require 'mouse-sel)

;(add-hook 'zmacs-activate-region-hook 
;          (lambda () 
;              (current-kill 1)))

;; Ignore invisible lines when moving cursor in project configuration
(setq line-move-ignore-invisible t)


;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "EMACS-IDE-CONFIG"
;;;; ==========================================================================

(define-derived-mode emacs-ide-config-mode fundamental-mode "Emacs-IDE config"
  (setq font-lock-defaults '('(("\\(.*\\)(.*):" 1 'font-lock-keyword-face)       ; parameter (with possibilities)
                               ("\\(.*\\):"     1 'font-lock-keyword-face)       ; parameter
                               ("\\((.*)\\):"   1 'font-lock-type-face)          ; "(possibilities)"
                               (":"             . 'font-lock-variable-name-face) ; ":"
                               ;("project_type:\\(.*\\)" 1 'font-lock-keyword-defaults) ; read only value
                               (":\\(.*\\)"     1 'font-lock-string-face)))))    ; value

(setq auto-mode-alist (append '(("\\.emacs-ide.options\\'" . emacs-ide-config-mode)
                                ("\\.emacs-ide.project\\'" . emacs-ide-config-mode)) auto-mode-alist))


;;;; ==========================================================================
;;;; CONFIGURATION
;;;; ==========================================================================

;; Project directory
;; On Windows : it is necessary to open a temporary file for the directory path
;; to be correct (Windows standard vs Unix)
;; On Linux : it is also useful to expand path (~ => /home/xxx/).
;; NB : "temp" was first used as a temporary filename, but it causes the project
;; directory to be changed to "temp" if "temp" already exists and is a
;; directory !... Hence a filename that can not exist !! :-)

(setq eide-temp-file "this-is-a-temporary-file-for-emacs-ide")
(find-file eide-temp-file)
(setq eide-project-directory default-directory)
(kill-buffer eide-temp-file)

(if eide-option-use-cscope-flag
  (progn
    (cscope-set-initial-directory eide-project-directory)
;    (setq cscope-do-not-update-database t)
))

(setq eide-menu-symbol-definition-to-find "-")

;; init a virer si possible :
(setq eide-project-name nil)
;(setq eide-project-already-open-flag nil)
(setq eide-compare-other-project-name nil)
(setq eide-other-projects-unfolded nil)
(setq eide-grep-results-unfolded nil)

(setq eide-session-project nil)
(setq eide-session-desktop nil)

(setq eide-windows-is-layout-visible-flag nil)
(setq eide-windows-menu-update-request-pending-flag nil)

;; Test if a project is defined
(if (file-exists-p eide-project-file)
  (progn
    ;; Check if this project is already open
;    (if (file-exists-p (concat eide-project-directory eide-project-lock-file))
;      (if (eide-popup-question-yes-or-no-p "WARNING : This project is already open (or has not exited properly)\nDo you want to continue ?")
;        (setq eide-project-already-open-flag t)
;        (kill-emacs)))
    (setq eide-session-project t)
    (find-file eide-project-file)
    (setq eide-project-type (eide-custom-get-project-value "project_type"))
    (eide-project-start-with-project)
    (setq eide-compare-other-project-directory nil))
  (if (file-exists-p ".emacs.desktop")
    (setq eide-session-desktop t)))

;; Load options file (it will be closed at the end of init, so that current
;; buffer - from .emacs.desktop - is not changed)
(find-file (concat "~/" eide-options-file))

;; Options file must be rebuilt before calling eide-project-start-with-project
;; (which may read this file to create current project config file)
(eide-custom-rebuild-options-file)

;; Frame size and position
(if window-system
  (if (eq system-type 'windows-nt)
    ;; Windows
    (setq initial-frame-alist '((top . 0) (left . 0) (width . 122) (height . 39)))
    ;; Linux
    (setq initial-frame-alist '((top . 30) (left . 0) (width . 120) (height . 48)))))

;(make-frame '((fullscreen . fullboth)))
;(modify-frame-parameters nil '((fullscreen . nil)))
;(modify-frame-parameters nil '((fullscreen . fullboth)))
;(set-frame-parameter nil 'fullscreen 'fullboth)

(eide-custom-apply-options)

;; Start with "editor" mode
(eide-key-bindings-configure-for-editor)


;;;; ==========================================================================
;;;; PREFERENCES FOR CODE
;;;; ==========================================================================

;; Display current function (relating to cursor position) in info line
;; (if possible with current major mode)
(if (not (featurep 'xemacs))
  (which-function-mode))

;; "Warning" color highlight when possible error is detected
;(global-cwarn-mode)

;; Tag file name with full path
(setq tags-file-name (concat eide-project-directory "TAGS"))

;; Do not prompt for updating tag file if necessary
(setq tags-revert-without-query t)

;; Augmenter le nombre de fonctions dans le menu pop up "liste des fonctions"
;; (sinon, elles sont parfois inutilement regroupées dans des sous-menus)
;; (default : 25)
;; no longer used (personal popup menu)
(setq imenu-max-items 40)

;; Augmenter le nombre de buffers dans le menu pop up "liste des buffers"
;; (sinon, elles sont parfois inutilement regroupées dans des sous-menus)
;; (default : 20)
;; no longer used (personal popup menu)
(setq mouse-buffer-menu-maxlen 40)

;; Highlight matching parentheses (when cursor on "(" or just after ")")
(if (not (featurep 'xemacs))
  (show-paren-mode))

;; moved to major mode hooks ! no effect on emacs linux, if here
;; (but used again, because no effect in hook !!!)


;;;; ==========================================================================
;;;; EDIFF
;;;; ==========================================================================

(require 'ediff)

;; Highlight current diff only
;(setq ediff-highlight-all-diffs nil)

;; Control panel in the same frame
(ediff-toggle-multiframe)

;; Split horizontally for buffer comparison
(setq ediff-split-window-function 'split-window-horizontally)


;;;; ==========================================================================
;;;; SYNTAX HIGHLIGHTING
;;;; ==========================================================================

;; First part of syntax highlighting is done in eide-custom.el

(eide-set-background-color-for-files)

;; New faces
;; Buffer menu
(make-face 'eide-menu-project-type-face)
;(make-face 'eide-menu-project-directory-face)
(make-face 'eide-menu-current-project-face)
(make-face 'eide-menu-file-type-header-face)
(make-face 'eide-menu-commands-header-face)
(make-face 'eide-menu-directory-face)
(make-face 'eide-menu-directory-out-of-project-face)
(make-face 'eide-menu-file-face)
(make-face 'eide-menu-file-ref-face)
(make-face 'eide-menu-file-new-face)
(make-face 'eide-menu-file-rw-face)
(make-face 'eide-menu-function-face)
(make-face 'eide-menu-function-with-highlight-face)
(make-face 'eide-menu-action-face)
(make-face 'eide-menu-disabled-action-face)
(make-face 'eide-menu-current-tab-face)
(make-face 'eide-menu-other-tab-face)
(make-face 'eide-menu-disabled-tab-face)
(make-face 'eide-menu-command-face)
(make-face 'eide-menu-project-face)
(make-face 'eide-menu-grep-result-face)
(make-face 'eide-menu-marker-face)
(make-face 'eide-menu-empty-face)
(make-face 'eide-menu-chapter1-face)
(make-face 'eide-menu-chapter2-face)

;; Information line
(if (featurep 'xemacs)
  (set-face-background 'modeline "wheat")
  (set-face-background 'mode-line "wheat"))

(if (featurep 'xemacs)
  (set-face-background 'vertical-divider "wheat"))

;; Parenthese matching (requires show-paren-mode)
;(set-face-background 'show-paren-match-face "orange")

;; Compilation warnings and file path in a grep result
;; (because grep uses "compile" mode to display its results)
(set-face-foreground 'font-lock-warning-face "tan")

;; Menus
;; (no effect with Windows)
(if (not (featurep 'xemacs))
  (progn
    (set-face-background 'menu "light grey")
    (set-face-foreground 'menu "black")))

;; Vertical scroll bar
;; (no effect with Windows)
(if (not (featurep 'xemacs))
  (set-face-background 'scroll-bar "light grey")
)

(make-face 'font-my-dos-face)
(set-face-foreground 'font-my-dos-face "wheat")

;; Hidden text (for hide/show minor mode)
(if (not (featurep 'xemacs)) ; error "face-id" with xemacs
  (progn
    (make-face 'font-selective-display-face)
    (set-face-foreground 'font-selective-display-face "blue")
    (set-face-background 'font-selective-display-face "lavender")
    (setq font-selective-display-face-id (face-id 'font-selective-display-face))

    (setq selective-display-vector (vconcat "{ ... }\n"))
    (setq selective-display-vector (vconcat "\n" (mapcar '(lambda (x) (+ (* font-selective-display-face-id 524288) x)) selective-display-vector)))
    (set-display-table-slot standard-display-table 'selective-display selective-display-vector)))

;; Ediff
(copy-face 'default 'ediff-even-diff-face-A)
(set-face-background 'ediff-even-diff-face-A "wheat") ;antique white")
(set-face-foreground 'ediff-even-diff-face-A "black")

(copy-face 'default 'ediff-even-diff-face-B)
(set-face-background 'ediff-even-diff-face-B "wheat") ;antique white")
(set-face-foreground 'ediff-even-diff-face-B "black")

(copy-face 'default 'ediff-odd-diff-face-A)
(set-face-background 'ediff-odd-diff-face-A "wheat") ;antique white")
(set-face-foreground 'ediff-odd-diff-face-A "black")

(copy-face 'default 'ediff-odd-diff-face-B)
(set-face-background 'ediff-odd-diff-face-B "wheat") ;antique white")
(set-face-foreground 'ediff-odd-diff-face-B "black")

;; Current difference : what is common or only in one buffer
(copy-face 'default 'ediff-current-diff-face-A)
(set-face-background 'ediff-current-diff-face-A "pink")
(set-face-foreground 'ediff-current-diff-face-A "black")

(copy-face 'default 'ediff-current-diff-face-B)
(set-face-background 'ediff-current-diff-face-B "pink")
(set-face-foreground 'ediff-current-diff-face-B "black")

;; Current difference : what really differs
(copy-face 'default 'ediff-fine-diff-face-A)
(set-face-background 'ediff-fine-diff-face-A "plum")
(set-face-foreground 'ediff-fine-diff-face-A "black")

(copy-face 'default 'ediff-fine-diff-face-B)
(set-face-background 'ediff-fine-diff-face-B "plum")
(set-face-foreground 'ediff-fine-diff-face-B "black")

;; Buffer-Menu

(if eide-option-buffer-menu-white-background-flag
  (progn
    (make-face 'eide-menu-white-background-face)
    (set-face-background 'eide-menu-white-background-face eide-option-color-background-for-menu)))

(set-face-foreground 'eide-menu-project-type-face "blue") ;dark violet")
(if eide-option-buffer-menu-white-background-flag
  (set-face-background 'eide-menu-project-type-face eide-option-color-background-for-menu))
(make-face-bold 'eide-menu-project-type-face)

;(set-face-foreground 'eide-menu-project-directory-face "maroon")
;(if eide-option-buffer-menu-white-background-flag
;  (set-face-background 'eide-menu-project-directory-face eide-option-color-background-for-menu))

(set-face-foreground 'eide-menu-current-project-face "red")
(if eide-option-buffer-menu-white-background-flag
  (set-face-background 'eide-menu-current-project-face eide-option-color-background-for-menu))

;(set-face-background 'eide-menu-current-project-face "pink")
(make-face-bold 'eide-menu-current-project-face)

(set-face-foreground 'eide-menu-file-type-header-face "dark violet") ;red")
(set-face-background 'eide-menu-file-type-header-face "lavender") ;thistle") ;pink")
(make-face-bold 'eide-menu-file-type-header-face)
;(set-face-attribute 'eide-menu-file-type-header-face nil
;                    :width 1.2
;                    :weight :bold
;                    :underline "red")

(set-face-foreground 'eide-menu-commands-header-face "blue") ;brown")
(set-face-background 'eide-menu-commands-header-face "pale turquoise") ;wheat")
(make-face-bold 'eide-menu-commands-header-face)

(if eide-option-buffer-menu-adapt-to-pale-background-flag
  (set-face-foreground 'eide-menu-directory-face "brown")
  (set-face-foreground 'eide-menu-directory-face "dark violet"))
(if eide-option-buffer-menu-adapt-to-pale-background-flag
  (set-face-background 'eide-menu-directory-face "misty rose")
  (set-face-background 'eide-menu-directory-face "lavender blush"))
(set-face-foreground 'eide-menu-directory-out-of-project-face "dark violet")
(set-face-background 'eide-menu-directory-out-of-project-face "lavender blush")

;(copy-face 'highlight 'highlight-action)
;(make-face-bold 'highlight-action)

(set-face-foreground 'eide-menu-file-face "gray55")
(make-face-bold 'eide-menu-file-face)

(set-face-foreground 'eide-menu-file-ref-face "orange red")
(make-face-bold 'eide-menu-file-ref-face)

(set-face-foreground 'eide-menu-file-new-face "medium sea green")
(make-face-bold 'eide-menu-file-new-face)

(set-face-foreground 'eide-menu-file-rw-face "black")
(make-face-bold 'eide-menu-file-rw-face)

(if eide-option-buffer-menu-white-background-flag
  (progn
  (set-face-background 'eide-menu-file-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-file-ref-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-file-new-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-file-rw-face eide-option-color-background-for-menu)))

(copy-face 'eide-menu-file-face 'eide-menu-current-file-face)
(copy-face 'eide-menu-file-ref-face 'eide-menu-current-file-ref-face)
(copy-face 'eide-menu-file-new-face 'eide-menu-current-file-new-face)
(copy-face 'eide-menu-file-rw-face 'eide-menu-current-file-rw-face)

;; for current buffer
(set-face-background 'eide-menu-current-file-face "yellow")
(set-face-background 'eide-menu-current-file-ref-face "yellow")
(set-face-background 'eide-menu-current-file-new-face "yellow")
(set-face-background 'eide-menu-current-file-rw-face "yellow")

(if eide-option-buffer-menu-adapt-to-pale-background-flag
  (set-face-foreground 'eide-menu-function-face "brown")
  (set-face-foreground 'eide-menu-function-face "blue"))
(if eide-option-buffer-menu-adapt-to-pale-background-flag
  (set-face-foreground 'eide-menu-function-with-highlight-face "brown")
  (set-face-foreground 'eide-menu-function-with-highlight-face "blue"))
(set-face-background 'eide-menu-function-with-highlight-face "aquamarine")

(set-face-foreground 'eide-menu-action-face "light slate blue")
(set-face-foreground 'eide-menu-disabled-action-face "red")
(make-face-italic 'eide-menu-disabled-action-face)
(set-face-foreground 'eide-menu-current-tab-face "dark green")
(set-face-foreground 'eide-menu-other-tab-face "dark green")
(set-face-foreground 'eide-menu-disabled-tab-face "red")
(make-face-bold 'eide-menu-current-tab-face)
(set-face-foreground 'eide-menu-command-face "blue")
(set-face-foreground 'eide-menu-project-face "dark green")
(set-face-foreground 'eide-menu-grep-result-face "dark green")
(set-face-foreground 'eide-menu-marker-face "dark green")
(set-face-foreground 'eide-menu-empty-face "black")
(make-face-italic 'eide-menu-empty-face)

(if eide-option-buffer-menu-white-background-flag
  (progn
  (set-face-background 'eide-menu-function-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-action-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-disabled-action-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-current-tab-face "yellow")
  (set-face-background 'eide-menu-other-tab-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-disabled-tab-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-command-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-project-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-grep-result-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-marker-face eide-option-color-background-for-menu)
  (set-face-background 'eide-menu-empty-face eide-option-color-background-for-menu)))

(set-face-foreground 'eide-menu-chapter1-face "red")
(set-face-background 'eide-menu-chapter1-face "yellow")
(set-face-foreground 'eide-menu-chapter2-face "blue")
(set-face-background 'eide-menu-chapter2-face "lavender")

;(require 'glasses)

;(make-face-bold 'glasses-face)


;;;; ==========================================================================
;;;; IMENU (LIST OF FUNCTIONS)
;;;; ==========================================================================

;; Construction de la liste des fonctions (imenu)
;; Deux méthodes :

;; 1) Recherche en arrière d'un { en début de ligne
;; Lorsque imenu-extract-index-name-function est différent de nil,
;; la fonction imenu-default-create-index-function utilise :
;; - imenu-prev-index-position-function (= beginning-of-defun par défaut)
;; - imenu-extract-index-name-function  (= nil par défaut)

;; 2) Utilisation d'expressions régulières
;; (il faut pour cela laisser imenu-extract-index-name-function = nil)
;; Il faut redéfinir les expressions, car les expressions par défaut amènent
;; beaucoup d'erreurs : du code est parfois interprété à tort comme une
;; définition de fonction)


;; Méthode 1 (non utilisée)

(defun eide-imenu-c-extract-index-name-function ()
  (search-backward "(" nil t)
  (skip-chars-backward " \n\r")
  (backward-char)
  (find-tag-default) )

(defun eide-imenu-c-prev-index-position-function ()
  (let ((mychar (string ?=)))
    (while (and (not (string-equal mychar ")")) (not (bobp)))
      (if (beginning-of-defun)
        (save-excursion
          (let ((doitagain (string ?y)))
            (while (string-equal doitagain "y")
            (progn
              (skip-chars-backward " \n\r")
              (setq mychar (char-to-string (char-before)))
              (if (string-equal mychar ")")
                (setq doitagain "n")
                (progn
                  (beginning-of-line)
                  (let ((char1 (char-to-string (char-after))))
                    (forward-char)
                    (let ((char2 (char-to-string (char-after))))
                      (backward-char)
                      (if (not (and (string-equal char1 "/")
                                    (string-equal char2 "/")))
                        (setq doitagain "n") ))))))))))))
  (if (bobp) nil t))

;; Méthode 2 (utilisée)

(setq my-regex-word  "[a-zA-Z_][a-zA-Z0-9_:<>~]*")
(setq my-regex-word-no-underscore "[a-zA-Z][a-zA-Z0-9_:<>~]*")
(setq my-regex-space "[ \t]+")
(setq my-regex-space-or-crlf "[ \t\n\r]+")
(setq my-regex-space-or-crlf-or-nothing "[ \t\n\r]*")
(setq my-regex-space-or-crlf-or-comment-or-nothing "[ \t\n\r]*\\(//\\)*[^\n\r]*[ \t\n\r]*")
;(setq my-regex-space-or-crlf-or-comment-or-nothing "[ \t\n\r]*\\(//\\)*[^\n\r]*[\n\r][ \t\n\r]*")
(setq my-regex-space-or-nothing "[ \t]*")

(setq eide-cc-imenu-c-macro
  (concat
    "^#define" my-regex-space
    "\\(" my-regex-word "\\)("
  )
)

(setq eide-cc-imenu-c-struct
  (concat
    "^typedef"  my-regex-space "struct" my-regex-space-or-crlf-or-nothing
    "{[^{]+}" my-regex-space-or-nothing
    "\\(" my-regex-word "\\)" ))

(setq eide-cc-imenu-c-enum
  (concat
    "^typedef" my-regex-space "enum" my-regex-space-or-crlf-or-nothing
    "{[^{]+}" my-regex-space-or-nothing
    "\\(" my-regex-word "\\)" ))

(setq eide-cc-imenu-c-define
  (concat
    "^#define" my-regex-space
    "\\(" my-regex-word "\\)" my-regex-space ))

(setq eide-cc-imenu-c-function
  (concat
    "^\\(?:" my-regex-word-no-underscore "\\*?" my-regex-space "\\)*" ; void* my_function(void)
    "\\*?"                        ; function may return a pointer, e.g. void *my_function(void)
    "\\(" my-regex-word "\\)"
    my-regex-space-or-nothing "("
    my-regex-space-or-nothing "\\([^ \t(*][^)]*\\)?)"   ; the arg list must not start
;    "[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
    my-regex-space-or-crlf-or-comment-or-nothing "{" ))


(if nil
 (progn
;; temp : remplace la définition au-dessus
(setq eide-cc-imenu-c-function
  (concat
    "^\\(?:" my-regex-word my-regex-space "\\)*"
    "\\(" my-regex-word "\\)"
    my-regex-space-or-nothing "("
    "\\(" my-regex-space-or-crlf-or-nothing my-regex-word "\\)*)"
;    my-regex-space-or-nothing "\\([^ \t(*][^)]*\\)?)"   ; the arg list must not start
;    "[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
    my-regex-space-or-crlf-or-nothing "{" ))
))

(setq eide-cc-imenu-c-interrupt
  (concat
    "\\(__interrupt"  my-regex-space
    "\\(" my-regex-word my-regex-space "\\)*"
    my-regex-word "\\)"
    my-regex-space-or-nothing "("
    my-regex-space-or-nothing "\\([^ \t(*][^)]*\\)?)"          ; the arg list must not start
    "[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
  ))

(setq eide-cc-imenu-c-generic-expression
  `(
    ;; General functions
    (nil          , eide-cc-imenu-c-function 1)

    ;; Interrupts
;    ("--function" , eide-cc-imenu-c-interrupt 1)
;    ("Interrupts" , eide-cc-imenu-c-interrupt 1)
;    (nil          , eide-cc-imenu-c-interrupt 1)

    ;; Macros
;    ("--function" , eide-cc-imenu-c-macro 1)
;    ("Macros"     , eide-cc-imenu-c-macro 1)

    ;; struct
;    ("--var"      , eide-cc-imenu-c-struct 1)
;    ("struct"     , eide-cc-imenu-c-struct 1)

    ;; enum
;    ("--var"      , eide-cc-imenu-c-enum 1)
;    ("enum"       , eide-cc-imenu-c-enum 1)

    ;; Defines
;    ("--var"      , eide-cc-imenu-c-define 1)
;    ("#define"    , eide-cc-imenu-c-define 1)
  ))


;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "C"
;;;; ==========================================================================

(require 'cc-mode)

(if eide-option-select-whole-symbol-flag
  ;; "_" should not be a word delimiter
  (modify-syntax-entry ?_ "w" c-mode-syntax-table))

(add-hook 'c-mode-hook '(lambda()
  (setq indent-tabs-mode nil)                ; Indentation : insert spaces instead of tabs
  (setq tab-width eide-c-indent-offset)      ; Tab display : number of char for one tab (default value : 8)

  (c-set-style "K&R")                        ; Indentation style
  (setq c-basic-offset eide-c-indent-offset) ; Indentation offset (default value : 5)
  (c-set-offset 'case-label '+)              ; Case/default in a switch (default value : 0)

  ;; Autofill minor mode
  ;; (automatic line feed beyond 80th column)
;  (auto-fill-mode 1)
;  (set-fill-column 80)

  ;; Highlight matching parentheses (when cursor on "(" or just after ")")
  (if (not (featurep 'xemacs))
    (if (not show-paren-mode)
      (show-paren-mode)))

  ;; Turn hide/show mode on
  (if (not hs-minor-mode)
    (hs-minor-mode))
  ;; Do not hide comments when hidding all
  (setq hs-hide-comments-when-hiding-all nil)

  ;; Turn ifdef mode on (does not work very well with ^M turned into empty lines)
  (hide-ifdef-mode 1)

  ;; Add Imenu in the menu ("Index")
  ;; (useless here because menu-bar is hidden)
  ;(imenu-add-menubar-index)

  ;; En principe, on préfère la méthode 2, mais pour une raison que j'ignore,
  ;; elle ne fonctionne pas avec xemacs ! (il ne trouve en général aucune
  ;; fonction)
  (if (featurep 'xemacs)
    (progn
      ;; Imenu method 1
      (setq imenu-prev-index-position-function 'eide-imenu-c-prev-index-position-function)
      (setq imenu-extract-index-name-function  'eide-imenu-c-extract-index-name-function))
    (progn
      ;; Imenu method 2
      (setq cc-imenu-c++-generic-expression eide-cc-imenu-c-generic-expression)
      (setq cc-imenu-c-generic-expression   eide-cc-imenu-c-generic-expression)))

  ;; Pour savoir si du texte est sélectionné ou non
  (setq mark-even-if-inactive nil)))

(if (not (featurep 'xemacs))
  (font-lock-add-keywords 'c-mode '(
    ("" . font-my-dos-face) ;font-lock-comment-face)
;    ("__interrupt" . font-lock-keyword-face)

    ("uint8" . font-lock-type-face)
    ("uint16" . font-lock-type-face)
    ("uint32" . font-lock-type-face)
    ("int8" . font-lock-type-face)
    ("int16" . font-lock-type-face)
    ("int32" . font-lock-type-face)
    ("TODO" . font-lock-warning-face))))


;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "EMACS LISP"
;;;; ==========================================================================

(if eide-option-select-whole-symbol-flag
  ;; "-" should not be a word delimiter
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))

(add-hook 'emacs-lisp-mode-hook '(lambda()
  ;; Indentation : insert spaces instead of tabs
  (setq indent-tabs-mode nil)

  ;; Autofill minor mode
  ;; (pour ne pas dépasser la 80ème colonne)
;  (auto-fill-mode 1)
;  (set-fill-column 80)

  ;; Highlight matching parentheses (when cursor on "(" or just after ")")
  (if (not (featurep 'xemacs))
    (if (not show-paren-mode)
      (show-paren-mode)))))


;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "SGML" (HTML, XML...)
;;;; ==========================================================================

(add-hook 'sgml-mode-hook '(lambda()
  ;; Indentation : insert spaces instead of tabs
  (setq indent-tabs-mode nil)
  (setq tab-width 2)))

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "SHELL SCRIPT"
;;;; ==========================================================================

(add-hook 'shell-mode-hook '(lambda()
  ;; Indentation : insert spaces instead of tabs
  (setq indent-tabs-mode nil)
  (setq tab-width 2)))

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "PERL"
;;;; ==========================================================================

(add-hook 'perl-mode-hook '(lambda()
  ;; Indentation : insert spaces instead of tabs
  (setq indent-tabs-mode nil)
  (setq tab-width 2)))

;; accepts file.c<2> for example
(setq source-file-regexp "\\.c\\(<.*>\\)?$")
(setq header-file-regexp "\\.h\\(<.*>\\)?$")


;;;; ==========================================================================
;;;; WINDOWS SETTINGS
;;;; ==========================================================================

;; User config file has been loaded to read configuration parameters
;; Now it can be closed
(kill-buffer eide-options-file)

;; Since "custom rebuild" functions have closed their buffers, and
;; eide-options-file has just been closed, we are back in directory from which
;; emacs has been launched : we can use desktop-read
;; NB : it is important to execute desktop-read after mode-hooks have been
;; defined, otherwise mode-hooks may not apply
(desktop-read)

;; Close temporary buffers from ediff sessions (if emacs has been closed during
;; an ediff session, .emacs.desktop contains temporary buffers (.ref or .new
;; files) and they have been loaded in this new emacs session).
(setq my-buffer-name-list (mapcar 'buffer-name (buffer-list)))
(dolist (this-buffer-name my-buffer-name-list)
  (if (or (string-match "^\* (REF)" this-buffer-name) (string-match "^\* (NEW)" this-buffer-name))
    ;; this is a "useless" buffer (.ref or .new)
    (kill-buffer this-buffer-name)))

(setq eide-current-buffer (buffer-name))
(eide-menu-init)
(eide-toolbar-init)
(eide-windows-init)

;(add-hook 'kill-emacs-hook 'eide-kill-emacs-hook)

(provide 'eide)

;;; init.el ends here
