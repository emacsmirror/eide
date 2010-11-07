;;; eide-help.el --- Emacs-IDE, help

;; Copyright (C) 2008-2010 Cédric Marie

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

(provide 'eide-help)

(defvar eide-version "1.4+")
(defvar eide-release-date "2010-11")

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Insert chapter title (level 1 ) in "help" buffer.
;;
;; input  : p-string : chapter title (string).
;; ----------------------------------------------------------------------------
(defun eide-i-help-insert-header-1 (p-string)
  (insert "\n\n\n")
  (put-text-property (point) (progn (insert (concat p-string "\n")) (point)) 'face 'eide-config-help-chapter1-face)
  (insert "\n"))

;; ----------------------------------------------------------------------------
;; Insert chapter title (level 2 ) in "help" buffer.
;;
;; input  : p-string : chapter title (string).
;; ----------------------------------------------------------------------------
(defun eide-i-help-insert-header-2 (p-string)
  (insert "\n")
  (put-text-property (point) (progn (insert (concat p-string "\n")) (point)) 'face 'eide-config-help-chapter2-face))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Display Help (full frame).
;; ----------------------------------------------------------------------------
(defun eide-help-open ()
  ;; Close menu
  (eide-windows-layout-unbuild)
  (eide-config-set-colors-for-help)
  (eide-keys-configure-for-special-buffer)

  (if (get-buffer "* Help *")
    (kill-buffer "* Help *"))
  (switch-to-buffer (get-buffer-create "* Help *"))

  (insert "\n")
  (put-text-property (point) (progn (insert "Emacs-IDE help page\n") (point)) 'face 'eide-config-help-title-face)
  (insert "\n")

  (insert "(click right to exit this page)\n\n")
  (insert (concat "Emacs-IDE - version " eide-version " - " eide-release-date "\n"))

  (eide-i-help-insert-header-1 "Windows layout")

  (eide-i-help-insert-header-2 "Overview")

  (insert "
With default options, windows layout should look like this:

  -----------------------------------------------------------
  |                                         |               |
  |                                         |               |
  |       'source' window                   | 'menu' window |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  -----------------------------------------------------------
  |                                                         |
  |               'output' window                           |
  |                                                         |
  -----------------------------------------------------------

You can resize all windows.
You can modify the layout in options.

")

  (eide-i-help-insert-header-1 "Mouse actions")

  (eide-i-help-insert-header-2 "Right click (when no text is selected)")

  (insert "
Right click behaviour depends on mouse position:

In 'source' window:
    Hide/show other windows (to view code full screen).

In 'menu' window:
    Open project popup menu:
    - project creation/configuration
    - project commands (compile, run, debug)
    - options
    - help (this page)
    - Emacs-IDE update checking (if Emacs-IDE is under svn)

In 'menu' window, over a file name:
    Open file popup menu (see 'Actions on files' below).

In 'menu' window, over a directory name:
    Open directory popup menu (see 'Actions on directories' below).

In 'output' window:
    Open results popup menu (to display existing 'grep' or 'cscope' result).

Shift + right click behaviour depends on mouse position:

In 'output' window:
    Open results deletion popup menu (to delete existing 'grep' or 'cscope'
    result).
")

  (eide-i-help-insert-header-2 "Right click (when text is selected)")

  (insert "
If text is selected on a single line:
    Open search popup menu, for selected string.

If text is selected over several lines:
    Open cleaning popup menu (to untabify or indent selection).
")

  (eide-i-help-insert-header-2 "Left / right click on mode-line file name")

  (insert "
On 'source' window mode-line:
    Switch to previous / next file.

On 'output' window mode-line:
    Switch to previous / next result buffer.
")

  (eide-i-help-insert-header-2 "Middle click")

  (insert "
Middle click behaviour depends on mouse position:

In 'menu' window:
    Display file browser (to open a file).

In other windows:
    Paste (standard behaviour).
")

  (eide-i-help-insert-header-2 "Wheel")

  (insert "
Shift + mouse wheel up/down scrolls right/left.
")

  (eide-i-help-insert-header-1 "Options")

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

  (eide-i-help-insert-header-1 "Work on a project")

  (eide-i-help-insert-header-2 "Create a project")

  (insert "
Launch emacs from your workset root directory.
Open project popup menu and select 'Create project'.

In your workset root directory, several files are created:

- TAGS:
  Tags database.

- cscope.files:
  Cscope list of files (C/C++ files).

- .emacs-ide.project:
  It defines parameters for this project.
  It is created with default values from '~/.emacs-ide.options'.

- .emacs-ide.project_notes:
  It can be used to write notes about the project, it is not used by emacs.
  It is created empty.

To edit project configuration ('.emacs-ide.project'), open project popup menu
and select 'Project configuration'.

To edit project notes ('.emacs-ide.project_notes'), open project popup menu and
select 'Project notes'.
")

  (eide-i-help-insert-header-2 "Open existing project")

  (insert "
Launch emacs from your workset root directory.
Files opened during last session are opened again automatically.
")

  (eide-i-help-insert-header-2 "Tags and cscope update")

  (insert "
When code is changed:
- Tags database ('TAGS') needs to be updated.
- Cscope database ('cscope.out') doesn't need to be updated (it is done
  automatically each time you search).

When a file is added or deleted:
- Tags database ('TAGS') needs to be updated.
- Cscope list of files ('cscope.files') needs to be updated ('cscope.out' will
  be updated automatically on next search).
")

  (eide-i-help-insert-header-1 "Actions on files (right click on file name)")

  (eide-i-help-insert-header-2 "Editing with REF files")

  (insert "
When editing a file, you can create a copy, so as to easily switch between
original and modified file, and compare them.

Original version of 'file' is saved as 'file.ref'.
When switching to original file, 'file' becomes 'file.new', and 'file.ref'
becomes 'file'.

File popup menu actions:
- Backup original file (REF) to work on a copy (NEW):
  create a copy of 'file' ('file.ref'), and set read/write permission on
  'file'.
- Switch to REF file: switch to original version ('file.ref').
- Discard REF file: discard original file, and use modified file.
- Restore REF file: discard modified file, and restore original file.
- Switch to NEW file: switch to modified version ('file.new').
- Discard NEW file: discard modified file, and use original file.
- Compare REF and NEW files: compare original and modified files.

File name colour:
- green when modified file is used.
- red when original file is used.
")

  (eide-i-help-insert-header-2 "Other actions on files")

  (insert "
File popup menu actions:
- Set read/write: Set read/write permission on file.
- Set read only: Set read only permission on file.
- Untabify and indent: Clean file (turn tabs into spaces and indent).
- Delete trailing spaces
- Convert end of line: DOS to UNIX
- Convert end of line: UNIX to DOS
If show_svn_status option is set:
- svn diff
- svn revert

File name colour:
- black when file is read/write.
- grey when file is read only.
If show_svn_status option is set:
- blue when file is modified compared to svn repository.
")

  (eide-i-help-insert-header-1 "Actions on directories (right click on directory name)")

  (insert "
Actions on files (see above) can be applied to several files - opened files
located in the same directory - at once.
An action is enabled in popup menu if it is allowed for at least one opened
file, and will be applied to all files for which it is allowed.
")

  (eide-i-help-insert-header-1 "Standard key bindings")

  (insert "
Control-x Control-b ........... list all buffers
Control-x Control-f ........... load a file or open a directory (file browsing)
Control-x Control-s ........... save current file
Control-s ..................... search
Alt-% ......................... replace
Control-_ ..................... undo
Control-g ..................... cancel current command
")

  (eide-i-help-insert-header-1 "New key bindings")

  (eide-i-help-insert-header-2 "Editing")

  (insert "
    Alt - left ........ Cut
    Alt - down ........ Copy
    Alt - right ....... Paste

Control - mouse 1 ..... Cut
Control - mouse 2 ..... Copy
Control - mouse 3 ..... Paste
")

  (eide-i-help-insert-header-2 "Code browsing with tags/cscope")

  (insert "
          F1 .......... Back from symbol definition
          F2 .......... Go to symbol definition (at cursor position, or selected text if any)
  Shift - F2 .......... Go to symbol definition (prompt for symbol)
  Shift - F1 .......... Go to alternative definition (after F2 or Shift - F2)
          F3 .......... Search symbol in whole project (at cursor position, or selected text if any)
  Shift - F3 .......... Search symbol in whole project (prompt for symbol)
")

  (eide-i-help-insert-header-2 "Grep search")

  (insert "
          F4 .......... Search string in whole project (at cursor position, or selected text if any)
  Shift - F4 .......... Search string in whole project (prompt for string)
          F6 .......... Search string in current directory (at cursor position, or selected text if any)
  Shift - F6 .......... Search string in current directory (prompt for string)
")

  ;;(eide-i-help-insert-header-2 "{...} block hiding")

  ;;(insert "
;;Control - F1 .......... Hide block
;;Control - F2 .......... Show block
;;Control - F3 .......... Hide all blocks in current buffer
;;Control - F4 .......... Show all blocks in current buffer
;;")

  (eide-i-help-insert-header-2 "Display")

  (insert "
          F5 .......... Reload current buffer (and update display)
  Shift - F5 .......... Close current buffer
")

  (eide-i-help-insert-header-2 "Grep result browsing")

  (insert "
          F7 .......... Go to previous instance
          F8 .......... Go to next instance
")

  (eide-i-help-insert-header-2 "Compilation error browsing")

  (insert "
          F7 .......... Go to previous error
          F8 .......... Go to next error
")

  (eide-i-help-insert-header-2 "Unix Shell commands")

  (insert "
          F9 .......... Compile (1)
  Shift - F9 .......... Compile (2)
          F10 ......... Run (1)
  Shift - F10 ......... Run (2)
          F11 ......... Debug (1)
  Shift - F11 ......... Debug (2)
          F12 ......... Open shell

NB: Additional compile commands (Compile (3) and Compile (4)) are not
available from the keyboard, but only from project popup menu.
")

  ;;  (eide-i-help-insert-header-2 "User defined text insertions")

  ;;  (insert "
  ;;Control - Shift - F1 .. Insert text #1
  ;;Control - Shift - F2 .. Insert text #2
  ;;")

  (eide-i-help-insert-header-1 "Windows layout overview during diff session")
  (insert "
  -----------------------------------------------------------
  |                             |                           |
  |                             |                           |
  |     'file A' window         |     'file B' window       |
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
  |                     'control' window                    |
  -----------------------------------------------------------
")
  (eide-i-help-insert-header-1 "Standard key bindings in diff session")
  (insert "
These commands must be typed in 'control' window:

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
  (eide-i-help-insert-header-1 "New key bindings in diff session")
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

;;; eide-help.el ends here
