;;; eide-menu.el --- Emacs-IDE, menu

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

(provide 'eide-menu)

(require 'imenu) ; for imenu--generic-function and imenu-generic-expression

(require 'eide-edit)
(require 'eide-svn)

(setq eide-menu-local-functions-unfolded-flag nil)
(setq eide-menu-local-highlighted-symbols-list nil)
(setq eide-menu-local-unfolded-symbols-folders-list nil)
(setq eide-menu-local-svn-modified-status-flag nil)
(setq eide-menu-local-edit-status nil)

(defvar eide-current-buffer nil)
(defvar eide-menu-current-buffer-marker nil)

(defvar eide-menu-buffer-name nil)
(defvar eide-menu-files-list nil)
(defvar eide-menu-grep-results-list nil)
(defvar eide-menu-cscope-results-list nil)
(defvar eide-menu-man-pages-list nil)

(defvar eide-menu-local-functions-unfolded-flag-backup nil)
(defvar eide-menu-local-functions-unfolded-flags-list nil)
(defvar eide-menu-local-unfolded-symbols-folders-list-backup nil)
(defvar eide-menu-local-unfolded-symbols-folders-lists-list nil)
(defvar eide-menu-local-highlighted-symbols-list-backup nil)
(defvar eide-menu-local-highlighted-symbols-lists-list nil)

(require 'dired)

(defvar eide-menu-browsing-mode-flag nil)
(defvar eide-i-menu-layout-should-be-built-after-browsing-mode-flag nil)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Insert text in "menu" buffer (with specific background).
;;
;; input  : p-string : string to insert.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-insert-text (p-string)
  (put-text-property (point) (progn (insert p-string) (point)) 'face 'eide-config-menu-default-face))

;; ----------------------------------------------------------------------------
;; Insert imenu elements list (recursive function).
;;
;; input  : p-elements-list : imenu elements list.
;;          p-unfolded-symbols-folders-list : list of unfolded symbols folders.
;;          p-highlighted-symbols-list : list of highlighted symbols.
;;          p-prefix : tabulation prefix string (for recursive calls).
;; ----------------------------------------------------------------------------
(defun eide-i-menu-insert-imenu-elements-list (p-elements-list p-unfolded-symbols-folders-list p-highlighted-symbols-list p-prefix)
  (dolist (l-element p-elements-list)
    (eide-i-menu-insert-text p-prefix)
    (let ((l-begin-point (point)))
      (if (markerp (cdr l-element))
        (progn
          ;; l-element is a function
          (put-text-property l-begin-point (progn (eide-i-menu-insert-text "-->") (point)) 'keymap function-name-highlight-map)
          (put-text-property l-begin-point (point) 'mouse-face 'highlight)
          (eide-i-menu-insert-text " ")
          (put-text-property (setq l-begin-point (point)) (progn (insert (car l-element)) (point)) 'keymap function-name-map)
          (if (member (car l-element) p-highlighted-symbols-list)
            (put-text-property l-begin-point (point) 'face 'eide-config-menu-function-with-highlight-face)
            (put-text-property l-begin-point (point) 'face 'eide-config-menu-function-face))
          (put-text-property l-begin-point (point) 'mouse-face 'highlight)
          (eide-i-menu-insert-text " \n"))
        ;; l-element is a folder
        (if (member (car l-element) p-unfolded-symbols-folders-list)
          (progn
            (put-text-property l-begin-point (progn (eide-i-menu-insert-text (concat "(-) " (car l-element))) (point)) 'keymap unfold-symbols-folder-map)
            (put-text-property l-begin-point (point) 'mouse-face 'highlight)
            (eide-i-menu-insert-text " \n")
            (eide-i-menu-insert-imenu-elements-list (cdr l-element) p-unfolded-symbols-folders-list p-highlighted-symbols-list (concat p-prefix " | ")))
          (progn
            (put-text-property l-begin-point (progn (eide-i-menu-insert-text (concat "(+) " (car l-element))) (point)) 'keymap unfold-symbols-folder-map)
            (put-text-property l-begin-point (point) 'mouse-face 'highlight)
            ;; Add a space after function name, because otherwise, property
            ;; applies on whole line ("\n")
            (eide-i-menu-insert-text " \n")))))))

;; ----------------------------------------------------------------------------
;; Insert a file name - and its functions if unfolded - in "menu" buffer.
;;
;; input  : p-string : name of file buffer.
;;          eide-current-buffer : current buffer name.
;;          eide-menu-current-buffer-marker : marker on current file in "menu"
;;              buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-insert-file (p-string)
  (let ((buffer-read-only nil) (l-imenu-elements-list nil)
        (l-unfolded-symbols-folders-list nil) (l-highlighted-symbols-list nil)
        (l-buffer-rw-flag t) (l-buffer-modified-flag nil) (l-buffer-svn-modified-flag nil)
        (l-buffer-status nil) (l-is-current nil) (l-functions-unfolded-flag nil))
    (save-excursion
      (set-buffer p-string)
      (setq l-buffer-status eide-menu-local-edit-status)
      (setq l-functions-unfolded-flag eide-menu-local-functions-unfolded-flag)
      ;; Check buffer status (r/w, modified, svn status)
      (if buffer-read-only
        (setq l-buffer-rw-flag nil))
      (if (buffer-modified-p)
        (setq l-buffer-modified-flag t))
      (if eide-config-show-svn-status-flag
        (setq l-buffer-svn-modified-flag eide-menu-local-svn-modified-status-flag))
      ;; If the buffer is unfolded, get functions list
      (if l-functions-unfolded-flag
        (save-excursion
          (setq l-imenu-elements-list (imenu--generic-function imenu-generic-expression))
          (setq l-unfolded-symbols-folders-list eide-menu-local-unfolded-symbols-folders-list)
          (setq l-highlighted-symbols-list eide-menu-local-highlighted-symbols-list))))

    ;; Check if this is current buffer
    (if (string-equal eide-current-buffer p-string)
      (setq l-is-current t)
      (setq l-is-current nil))

    (let ((l-begin-point (point)))
      (if l-functions-unfolded-flag
        (put-text-property l-begin-point (progn (eide-i-menu-insert-text "(-)") (point)) 'keymap unfold-functions-map)
        (put-text-property l-begin-point (progn (eide-i-menu-insert-text "(+)") (point)) 'keymap unfold-functions-map))
      (put-text-property l-begin-point (point) 'mouse-face 'highlight)
      (eide-i-menu-insert-text " ")
      (put-text-property (setq l-begin-point (point)) (progn (insert p-string) (point)) 'keymap file-name-map)
      (if l-is-current
        ;; Current file
        (if (string-equal l-buffer-status "nofile")
          (put-text-property l-begin-point (point) 'face 'eide-config-menu-current-file-nofile-face)
          (if (string-equal l-buffer-status "ref")
            (put-text-property l-begin-point (point) 'face 'eide-config-menu-current-file-ref-face)
            (if (string-equal l-buffer-status "new")
              (put-text-property l-begin-point (point) 'face 'eide-config-menu-current-file-new-face)
              (if l-buffer-svn-modified-flag
                (put-text-property l-begin-point (point) 'face 'eide-config-menu-current-file-svn-modified-face)
                (if l-buffer-rw-flag
                  (put-text-property l-begin-point (point) 'face 'eide-config-menu-current-file-rw-face)
                  (put-text-property l-begin-point (point) 'face 'eide-config-menu-current-file-ro-face))))))
        ;; Not current file
        (if (string-equal l-buffer-status "nofile")
          (put-text-property l-begin-point (point) 'face 'eide-config-menu-file-nofile-face)
          (if (string-equal l-buffer-status "ref")
            (put-text-property l-begin-point (point) 'face 'eide-config-menu-file-ref-face)
            (if (string-equal l-buffer-status "new")
              (put-text-property l-begin-point (point) 'face 'eide-config-menu-file-new-face)
              (if l-buffer-svn-modified-flag
                (put-text-property l-begin-point (point) 'face 'eide-config-menu-file-svn-modified-face)
                (if l-buffer-rw-flag
                  (put-text-property l-begin-point (point) 'face 'eide-config-menu-file-rw-face)
                  (put-text-property l-begin-point (point) 'face 'eide-config-menu-file-ro-face)))))))
      (put-text-property l-begin-point (point) 'mouse-face 'highlight))

    ;; Add a space after filename, because otherwise, with some versions of
    ;; emacs, property applies on whole line ("\n")
    (eide-i-menu-insert-text " ")

    (if l-buffer-svn-modified-flag
      (eide-i-menu-insert-text "(M) "))
    (if l-buffer-modified-flag
      (eide-i-menu-insert-text "*"))

    (if l-is-current
      (save-excursion
        (beginning-of-line)
        (forward-char)
        (setq eide-menu-current-buffer-marker (point-marker))))

    (eide-i-menu-insert-text "\n")

    (if l-functions-unfolded-flag
      ;; Insert functions
      (if l-imenu-elements-list
        (eide-i-menu-insert-imenu-elements-list l-imenu-elements-list l-unfolded-symbols-folders-list l-highlighted-symbols-list "  ")
        (progn
          (put-text-property (point) (progn (insert "      (no function)") (point)) 'face 'eide-config-menu-empty-list-face)
          (eide-i-menu-insert-text "\n"))))))

;; ----------------------------------------------------------------------------
;; Insert all files from a directory - in "menu" buffer.
;;
;; input  : p-directory-name : name of directory.
;;          eide-menu-files-list : list of opened files.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-insert-directory (p-directory-name)
  (let ((buffer-read-only nil) (l-directory-short (eide-project-get-short-directory p-directory-name)) (l-begin-point nil))
    (if (string-equal p-directory-name l-directory-short)
      (setq l-directory-short nil)
      (if (string-equal l-directory-short "")
        ;; Make root directory "clickable"
        (setq l-directory-short "./")))

    (if l-directory-short
      (progn
        (put-text-property (setq l-begin-point (point)) (progn (insert l-directory-short) (point)) 'keymap directory-name-map)
        (put-text-property l-begin-point (point) 'face 'eide-config-menu-directory-face))
      (progn
        (put-text-property (setq l-begin-point (point)) (progn (insert p-directory-name) (point)) 'keymap directory-name-map)
        (put-text-property l-begin-point (point) 'face 'eide-config-menu-directory-out-of-project-face)))
    (put-text-property l-begin-point (point) 'mouse-face 'highlight)
    (if l-directory-short
      (put-text-property (point) (progn (insert " \n") (point)) 'face 'eide-config-menu-directory-face)
      (put-text-property (point) (progn (insert " \n") (point)) 'face 'eide-config-menu-directory-out-of-project-face))

    ;; Parse buffer list for buffers from this directory to display
    (dolist (l-buffer eide-menu-files-list)
      (if (string-equal p-directory-name (file-name-directory (buffer-file-name (get-buffer l-buffer))))
        (eide-i-menu-insert-file l-buffer)))
    ;; Insert an empty line between two directories
    (eide-i-menu-insert-text "\n")))

;; ----------------------------------------------------------------------------
;; Insert all files - grouped by directory - in "menu" buffer.
;;
;; input  : eide-menu-files-list : list of opened files.
;; output : eide-menu-current-buffer-marker : marker on current file in "menu"
;;              buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-insert-all-files ()
  (let ((l-directory-list nil))
    ;; First, parse the list of buffers to built the list of directories
    (dolist (l-buffer eide-menu-files-list)
      ;; Extract the directory from the buffer file name
      (let ((l-directory (file-name-directory (buffer-file-name (get-buffer l-buffer)))))
        ;; If this is the first buffer from this directory, add the directory to the list
        (if (not (member l-directory l-directory-list))
          (setq l-directory-list (cons l-directory l-directory-list)))))
    ;; Sort the list in alphabetical order
    (setq l-directory-list (sort l-directory-list 'string<))
    ;; For each directory, insert the directory name, and parse the list of buffers to insert those that match
    (dolist (l-directory l-directory-list)
      (eide-i-menu-insert-directory l-directory))))

;; ----------------------------------------------------------------------------
;; Change current file (select the one on current line in "menu" buffer).
;;
;; input  : p-buffer-name : new current buffer name.
;;          eide-current-buffer : current buffer name.
;;          eide-menu-current-buffer-marker : marker on current file in "menu"
;;              buffer.
;;          eide-root-directory : project root directory.
;; output : eide-current-buffer : new current buffer name.
;;          eide-menu-current-buffer-marker : marker on new current file in
;;              "menu" buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-update-current-buffer (p-buffer-name)
  (save-excursion
    (beginning-of-line)
    ;; Current position might not be on the line of buffer name: in that case
    ;; we must search for buffer name in previous lines
    (while (string-equal (char-to-string (char-after)) " ") (forward-line -1))
    (forward-char)
    ;; Marker is not set on the first char, because a problem occurs when new
    ;; marker is on the line below old marker: when old file is removed (to be
    ;; displayed again later without highlight), old and new markers become
    ;; equals, and when old file is inserted, new marker remains on beginning
    ;; of line of old file. The problem is fixed if the marker is set on second
    ;; char (new marker will not be separated from the line related to new file)
    (let ((eide-menu-old-current-buffer-marker eide-menu-current-buffer-marker)
          (eide-old-current-buffer eide-current-buffer))
      (setq eide-menu-current-buffer-marker (point-marker))
      (setq eide-current-buffer p-buffer-name)

      (goto-char (marker-position eide-menu-old-current-buffer-marker))
      (eide-i-menu-remove-file)
      (eide-i-menu-insert-file eide-old-current-buffer))

    (goto-char (marker-position eide-menu-current-buffer-marker))
    (eide-i-menu-remove-file)
    (eide-i-menu-insert-file eide-current-buffer)))

;; ----------------------------------------------------------------------------
;; Remove a file from "menu" buffer (beginning on current line).
;; ----------------------------------------------------------------------------
(defun eide-i-menu-remove-file ()
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line) (while (string-equal (char-to-string (char-after)) " ") (forward-line)) (point)))))

;; ----------------------------------------------------------------------------
;; Remove a directory from "menu" buffer (beginning on current line).
;; ----------------------------------------------------------------------------
(defun eide-i-menu-remove-directory ()
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line) (while (not (string-equal (char-to-string (char-after)) "\n")) (forward-line)) (progn (forward-line) (point))))))

;; ----------------------------------------------------------------------------
;; Get symbol name on current line in "menu" buffer.
;;
;; return : symbol name.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-get-symbol-name-on-current-line ()
  (beginning-of-line)
  (search-forward "> " nil t)
  (buffer-substring-no-properties
   (point)
   (progn (end-of-line) (backward-char) (point))))

;; ----------------------------------------------------------------------------
;; Get folder name on current line in "menu" buffer.
;;
;; return : symbol name.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-get-folder-name-on-current-line ()
  (beginning-of-line)
  (search-forward ") " nil t)
  (buffer-substring-no-properties
   (point)
   (progn (end-of-line) (backward-char) (point))))

;; ----------------------------------------------------------------------------
;; Get buffer name on previous lines in "menu" buffer.
;;
;; return : buffer name.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-get-buffer-name-on-previous-lines ()
  (beginning-of-line)
  (save-excursion
    (while (string-equal (char-to-string (char-after)) " ") (forward-line -1))
    (eide-menu-get-buffer-name-on-current-line)))

;; ----------------------------------------------------------------------------
;; Rebuild "menu" buffer.
;;
;; input  : p-force-update-status-flag : t = update files status, nil = do not
;;              update.
;;          eide-project-name : project name.
;;          eide-root-directory : project root directory.
;; output : eide-menu-files-list : list of opened files.
;;          eide-menu-current-buffer-marker : marker on current file in "menu"
;;              buffer.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-rebuild (p-force-update-status-flag)
  (let ((buffer-read-only nil) (l-position-marker nil))
    (erase-buffer)
    (setq eide-menu-current-buffer-marker nil)

    (eide-i-menu-insert-text "\n")

    (if eide-project-name
      (progn
        (put-text-property (point) (progn (insert "Project: ") (point)) 'face 'eide-config-menu-project-header-face)
        (put-text-property (point) (progn (insert eide-project-name) (point)) 'face 'eide-config-menu-project-name-face))
      (put-text-property (point) (progn (insert "Root directory:") (point)) 'face 'eide-config-menu-project-header-face))

    (eide-i-menu-insert-text "\n")
    (eide-i-menu-insert-text eide-root-directory)
    (eide-i-menu-insert-text "\n\n")

    (if p-force-update-status-flag
      ;; Update status of all files
      (progn
        (eide-menu-build-files-lists)
        ;; Update edit status (REF/NEW) of all files
        (eide-edit-update-files-status)
        ;; Update svn modified status of all files
        (if eide-config-show-svn-status-flag
          (eide-svn-update-files-status)))
      ;; Retrieve status of new opened files, but do not update status of other files
      (let ((eide-menu-files-old-list eide-menu-files-list) (l-new-files nil))
        (eide-menu-build-files-lists)
        ;; Build a list (l-new-files) with new opened files
        (dolist (l-file eide-menu-files-list)
          (if (not (member l-file eide-menu-files-old-list))
            (setq l-new-files (cons l-file l-new-files))))
        (if l-new-files
          (progn
            ;; Retrieve edit status (REF/NEW) of new opened files
            (eide-edit-update-files-status l-new-files)
            ;; Retrieve svn modified status of new opened files
            (if eide-config-show-svn-status-flag
              (eide-svn-update-files-status l-new-files))))))

    ;; Insert all files
    (if eide-menu-files-list
      (eide-i-menu-insert-all-files))

    ;; 80 blank lines, so that "menu" window seems to have specific background
    (let ((l-loop-count 0))
      (save-excursion
        (while (< l-loop-count 80)
          (eide-i-menu-insert-text "\n")
          (setq l-loop-count (+ l-loop-count 1)))))

    ;; Move cursor to current buffer
    (if eide-menu-current-buffer-marker
      (progn
        (goto-char (marker-position eide-menu-current-buffer-marker))
        (recenter)))))

;; ----------------------------------------------------------------------------
;; Switch to selected file.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-file-open ()
  (interactive)
  (let ((l-buffer (eide-menu-get-buffer-name-on-current-line)))
    (eide-i-menu-update-current-buffer l-buffer)
    (eide-windows-select-source-window t)
    (switch-to-buffer l-buffer)))

;; ----------------------------------------------------------------------------
;; Fold / unfold list of functions for selected file.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-file-unfold-functions ()
  (interactive)
  (let ((l-buffer-name (eide-menu-get-buffer-name-on-current-line)))
    (save-excursion
      (set-buffer l-buffer-name)
      (if eide-menu-local-functions-unfolded-flag
        (setq eide-menu-local-functions-unfolded-flag nil)
        (progn
          (make-local-variable 'eide-menu-local-functions-unfolded-flag)
          (setq eide-menu-local-functions-unfolded-flag t))))
    (eide-i-menu-remove-file)
    (save-excursion
      (eide-i-menu-insert-file l-buffer-name))))

;; ----------------------------------------------------------------------------
;; Fold / unfold symbols folder for selected file.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-file-unfold-symbols-folder ()
  (interactive)
  ;; TODO: position non conservée (on se retrouve au niveau du nom du fichier)
  (save-excursion
    (let ((l-folder-name (eide-i-menu-get-folder-name-on-current-line))
          (l-buffer-name (eide-i-menu-get-buffer-name-on-previous-lines)))
      (save-excursion
        (set-buffer l-buffer-name)
        (make-local-variable 'eide-menu-local-unfolded-symbols-folders-list)
        (if (member l-folder-name eide-menu-local-unfolded-symbols-folders-list)
          ;; Already unfolded => remove it
          (setq eide-menu-local-unfolded-symbols-folders-list (remove l-folder-name eide-menu-local-unfolded-symbols-folders-list))
          ;; Not unfolded yet => add it
          (push l-folder-name eide-menu-local-unfolded-symbols-folders-list)))
      ;; Current position might not be on the line of buffer name: in that case
      ;; we must search for buffer name in previous lines
      ;; TODO: factoriser !
      (while (string-equal (char-to-string (char-after)) " ") (forward-line -1))
      (eide-i-menu-remove-file)
      (eide-i-menu-insert-file l-buffer-name))))

;; ----------------------------------------------------------------------------
;; Get symbol marker in imenu list (recursive function).
;;
;; input  : p-symbol : symbol.
;; return : marker to symbol.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-get-symbol-marker-in-imenu-list (p-symbol p-list)
  (let ((l-marker-found nil))
    (dolist (l-element p-list)
      (if (not l-marker-found)
        ;; Symbol not found yet
        (if (markerp (cdr l-element))
          ;; Check if this element contains the symbol we are looking for
          (if (equal p-symbol (car l-element))
            (setq l-marker-found (cdr l-element)))
          ;; This element is a list: recursive call
          (setq l-marker-found (eide-i-menu-get-symbol-marker-in-imenu-list p-symbol (cdr l-element))))))
    l-marker-found))

;; ----------------------------------------------------------------------------
;; Get symbol marker in current buffer.
;;
;; input  : p-symbol : symbol.
;; return : marker to symbol.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-get-symbol-marker (p-symbol)
  (eide-i-menu-get-symbol-marker-in-imenu-list p-symbol (imenu--generic-function imenu-generic-expression)))

;; ----------------------------------------------------------------------------
;; Enable / disable highlight on selected function.
;;
;; input  : eide-menu-local-highlighted-symbols-list : list of highlighted
;;              symbols.
;; output : eide-menu-local-highlighted-symbols-list : updated list of
;;              highlighted symbols.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-file-highlight-function ()
  (interactive)
  ;; TODO: position non conservée (on se retrouve au niveau du nom du fichier)
  (save-excursion
    (let ((l-symbol-name (eide-i-menu-get-symbol-name-on-current-line))
          (l-buffer-name (eide-i-menu-get-buffer-name-on-previous-lines)))
      (save-excursion
        (set-buffer l-buffer-name)
        (make-local-variable 'eide-menu-local-highlighted-symbols-list)
        (if (member l-symbol-name eide-menu-local-highlighted-symbols-list)
          ;; Already highlighted => remove it
          (setq eide-menu-local-highlighted-symbols-list (remove l-symbol-name eide-menu-local-highlighted-symbols-list))
          ;; Not highlighted yet => add it
          (push l-symbol-name eide-menu-local-highlighted-symbols-list)))
      ;; Current position might not be on the line of buffer name: in that case
      ;; we must search for buffer name in previous lines
      ;; TODO: factoriser !
      (while (string-equal (char-to-string (char-after)) " ") (forward-line -1))
      (eide-i-menu-remove-file)
      (eide-i-menu-insert-file l-buffer-name))))

;; ----------------------------------------------------------------------------
;; Go to selected function.
;; ----------------------------------------------------------------------------
(defun eide-i-menu-goto-function ()
  (interactive)
  (let ((l-symbol-name (eide-i-menu-get-symbol-name-on-current-line))
        (l-buffer-name (eide-i-menu-get-buffer-name-on-previous-lines)))
    (eide-i-menu-update-current-buffer l-buffer-name)
    (eide-windows-select-source-window t)
    (switch-to-buffer l-buffer-name)
    (goto-char (marker-position (eide-i-menu-get-symbol-marker l-symbol-name)))
    (recenter)))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Init "menu" buffer.
;;
;; output : eide-menu-buffer-name : "menu" buffer name.
;; ----------------------------------------------------------------------------
(defun eide-menu-init ()
  ;; Menu buffer is created empty (content will be built by eide-menu-update)
  (setq eide-menu-buffer-name (buffer-name (get-buffer-create "* Menu *")))
  (save-excursion
    (set-buffer eide-menu-buffer-name)
    (setq buffer-read-only t)))

;; ----------------------------------------------------------------------------
;; Update "menu" buffer.
;;
;; input  : p-force-rebuild-flag : t = always rebuild menu, nil = rebuild only
;;              if current buffer has changed.
;;          p-force-update-status-flag : t = update files status, nil = do not
;;              update.
;; output : eide-current-buffer : current buffer name.
;;          eide-windows-menu-update-request-pending-flag : t = update is
;;              postponed until next time "menu" buffer is shown.
;; ----------------------------------------------------------------------------
(defun eide-menu-update (p-force-rebuild-flag &optional p-force-update-status-flag)
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Cancel pending request
      (setq eide-windows-menu-update-request-pending-flag nil)
      (setq eide-windows-menu-update-request-pending-force-rebuild-flag nil)
      (setq eide-windows-menu-update-request-pending-force-update-status-flag nil)
      ;; Save window to go back to, once menu has been updated
      (let ((l-window (selected-window)))
        (eide-windows-select-source-window t)
        ;; On Emacs 22 GTK: buffer-name does not return current but previous
        ;; buffer!... The bug is fixed if window-buffer is used.
        ;;(setq eide-current-buffer-temp (buffer-name))
        (let ((eide-current-buffer-temp (buffer-name (window-buffer (selected-window)))))
          (if p-force-rebuild-flag
            (progn
              (eide-windows-select-menu-window)
              (setq eide-current-buffer eide-current-buffer-temp)
              (eide-i-menu-rebuild p-force-update-status-flag))
            (if (not (string-equal eide-current-buffer eide-current-buffer-temp))
              (progn
                (eide-windows-select-menu-window)
                (goto-char (point-min))
                ;; Case sensitive search is necessary for buffer name
                (if (and (let ((case-fold-search nil)) (search-forward (concat " " eide-current-buffer-temp " ") nil t))
                         (get-buffer eide-current-buffer))
                  ;; Old and new files are both present in menu: just update current buffer
                  (eide-i-menu-update-current-buffer eide-current-buffer-temp)
                  ;; File not present in menu: update whole menu
                  (progn
                    (setq eide-current-buffer eide-current-buffer-temp)
                    (eide-i-menu-rebuild nil)))))))
        ;; Go back to "current window"
        (select-window l-window)))
    (progn
      (setq eide-windows-menu-update-request-pending-flag t)
      ;; Force rebuild flag must not be changed if already set
      (if (not eide-windows-menu-update-request-pending-force-rebuild-flag)
        (if p-force-rebuild-flag
          (setq eide-windows-menu-update-request-pending-force-rebuild-flag t)
          (if (or (not (member eide-current-buffer eide-menu-files-list))
                  (not (member (buffer-name (window-buffer (selected-window))) eide-menu-files-list)))
            (setq eide-windows-menu-update-request-pending-force-rebuild-flag t))))
      ;; Force update status flag must not be changed if already set
      (if p-force-update-status-flag
        (setq eide-windows-menu-update-request-pending-force-update-status-flag t)))))

;; ----------------------------------------------------------------------------
;; Build the lists of buffers.
;;
;; output : eide-menu-files-list : list of opened files.
;;          eide-menu-grep-results-list : list of grep results buffers.
;;          eide-menu-cscope-results-list : list of cscope results buffers.
;;          eide-menu-man-pages-list : list of man pages buffers.
;; ----------------------------------------------------------------------------
(defun eide-menu-build-files-lists ()
  (setq eide-menu-files-list nil)
  (setq eide-menu-grep-results-list nil)
  (setq eide-menu-cscope-results-list nil)
  (setq eide-menu-man-pages-list nil)

  (let ((l-buffer-name-list (mapcar 'buffer-name (buffer-list))))
    (setq l-buffer-name-list (sort l-buffer-name-list 'string<))
    (setq l-buffer-name-list (reverse l-buffer-name-list))

    (dolist (l-buffer-name l-buffer-name-list)
      (if (not (or (string-match "^[ \*]" l-buffer-name) (string-equal "TAGS" l-buffer-name)))
        ;; This is a "useful" buffer
        (save-excursion
          (set-buffer l-buffer-name)
          (if (or (equal major-mode 'dired-mode)
                  (equal major-mode 'Buffer-menu-mode))
            (kill-buffer l-buffer-name)
            (if (not (string-equal l-buffer-name eide-project-config-file))
              (setq eide-menu-files-list (cons l-buffer-name eide-menu-files-list)))))
        ;; This is a "*..." buffer
        (if (string-match "^\*grep.*" l-buffer-name)
          (setq eide-menu-grep-results-list (cons l-buffer-name eide-menu-grep-results-list))
          (if (string-match "^\*cscope\*.*" l-buffer-name)
            (setq eide-menu-cscope-results-list (cons l-buffer-name eide-menu-cscope-results-list))
            (if (string-match "^\*Man .*" l-buffer-name)
              (setq eide-menu-man-pages-list (cons l-buffer-name eide-menu-man-pages-list)))))))))

;; ----------------------------------------------------------------------------
;; Update current buffer "modified" status (in menu).
;; ----------------------------------------------------------------------------
(defun eide-menu-update-current-buffer-modified-status ()
  (save-excursion
    (let ((l-buffer (buffer-name)))
      ;; eide-menu-local-edit-status update is useful when a new buffer is saved
      ;; in file system for the first time (status changes from "nofile" to "")
      (make-local-variable 'eide-menu-local-edit-status)
      (setq eide-menu-local-edit-status (eide-edit-get-buffer-status))
      (if eide-config-show-svn-status-flag
        (progn
          (make-local-variable 'eide-menu-local-svn-modified-status-flag)
          (setq eide-menu-local-svn-modified-status-flag (eide-svn-is-current-buffer-modified-p))))
      (set-buffer eide-menu-buffer-name)
      (save-excursion
        ;; Case sensitive search is necessary for buffer name
        (if (let ((case-fold-search nil))
              (or (search-forward (concat " " l-buffer " \n") nil t)
                  (search-forward (concat " " l-buffer " *\n") nil t)
                  (search-forward (concat " " l-buffer " (M) \n") nil t)
                  (search-forward (concat " " l-buffer " (M) *\n") nil t)))
          (progn
            (forward-line -1)
            (eide-i-menu-remove-file)
            (eide-i-menu-insert-file l-buffer)))))))

;; ----------------------------------------------------------------------------
;; Get directory name on current line in "menu" buffer.
;;
;; return : directory name.
;; ----------------------------------------------------------------------------
(defun eide-menu-get-directory-name-on-current-line ()
  (beginning-of-line)
  (buffer-substring-no-properties
   (point)
   (progn
     (end-of-line)
     (backward-char)
     (while (not (equal (get-text-property (point) 'mouse-face) 'highlight))
       (backward-char))
     (forward-char)
     (point))))

;; ----------------------------------------------------------------------------
;; Get buffer name on current line in "menu" buffer.
;;
;; return : buffer name.
;; ----------------------------------------------------------------------------
(defun eide-menu-get-buffer-name-on-current-line ()
  (beginning-of-line)
  (forward-char 4)
  (buffer-substring-no-properties
   (point)
   (progn
     (end-of-line)
     (backward-char)
     (while (equal (get-text-property (point) 'face) 'eide-config-menu-default-face)
       (backward-char))
     (forward-char)
     (point))))

;; ----------------------------------------------------------------------------
;; Close selected file.
;;
;; input  : p-buffer-name : buffer name.
;; output : eide-current-buffer : current buffer name (may have changed).
;; ----------------------------------------------------------------------------
(defun eide-menu-file-close (p-buffer-name)
  (let ((l-do-it-flag t) (l-buffer-edit-status nil))
    (save-excursion
      (set-buffer p-buffer-name)
      (setq l-buffer-edit-status eide-menu-local-edit-status))
    (if (or (string-equal l-buffer-edit-status "new")
            (string-equal l-buffer-edit-status "ref"))
      (setq l-do-it-flag (eide-popup-question-yes-or-no-p (concat p-buffer-name " has been edited. Do you really want to close it?"))))
    (if l-do-it-flag
      (progn
        (kill-buffer p-buffer-name)
        (setq eide-menu-files-list (remove p-buffer-name eide-menu-files-list))
        (if (string-equal p-buffer-name eide-current-buffer)
          (progn
            ;; Current buffer has been closed: display another one
            (eide-windows-skip-unwanted-buffers-in-source-window)
            ;; Update menu to focus on new current buffer
            (eide-menu-update t))
          (progn
            (eide-i-menu-remove-file)
            (if (string-equal (char-to-string (char-after)) "\n")
              ;; It was the last file of the group
              (progn
                (forward-line -1)
                (let ((l-property (get-text-property (point) 'face)))
                  (if (or (equal l-property 'eide-config-menu-directory-face)
                          (equal l-property 'eide-config-menu-directory-out-of-project-face))
                    ;; It was also the only one: we must delete directory line
                    (let ((buffer-read-only nil))
                      (delete-region (point) (progn (forward-line 2) (point))))))))))))))

;; ----------------------------------------------------------------------------
;; Close all files in selected directory.
;;
;; input  : p-directory-name : directory name.
;;          eide-menu-files-list : list of opened files.
;; output : eide-current-buffer : current buffer name (may have changed).
;; ----------------------------------------------------------------------------
(defun eide-menu-directory-close (p-directory-name)
  (let ((l-ask-flag nil) (l-do-it-flag t) (l-buffer-edit-status nil) (l-buffer-svn-modified-flag nil))
    ;; Check if at least one file has been edited (REF or NEW)
    (dolist (l-buffer eide-menu-files-list)
      (if (eide-menu-is-file-in-directory-p l-buffer p-directory-name)
        (progn
          (save-excursion
            (set-buffer l-buffer)
            (setq l-buffer-edit-status eide-menu-local-edit-status)
            (if eide-config-show-svn-status-flag
              (setq l-buffer-svn-modified-flag eide-menu-local-svn-modified-status-flag)))
          (if (or (string-equal l-buffer-edit-status "new")
                  (string-equal l-buffer-edit-status "ref")
                  l-buffer-svn-modified-flag)
            (setq l-ask-flag t)))))
    (if l-ask-flag
      (setq l-do-it-flag (eide-popup-question-yes-or-no-p (concat "Some files in " p-directory-name " have been edited. Do you really want to close them?"))))
    (if l-do-it-flag
      (progn
        (dolist (l-buffer eide-menu-files-list)
          (if (eide-menu-is-file-in-directory-p l-buffer p-directory-name)
            (progn
              (kill-buffer l-buffer)
              (setq eide-menu-files-list (remove l-buffer eide-menu-files-list)))))
        (if (get-buffer eide-current-buffer)
          ;; Current buffer has not been closed: just remove this directory
          (eide-i-menu-remove-directory)
          (progn
            ;; Current buffer has been closed: display another one
            (eide-windows-skip-unwanted-buffers-in-source-window)
            ;; Update menu to focus on new current buffer
            (eide-menu-update t)))))))

;; ----------------------------------------------------------------------------
;; Prepare update of a file in "menu" buffer.
;;
;; input  : p-buffer-name : buffer name.
;; output : eide-menu-local-functions-unfolded-flag-backup : backup of
;;              functions unfolded status (local variable will be lost on file
;;              update).
;;          eide-menu-local-unfolded-symbols-folders-list-backup : backup of
;;              list of unfolded symbols folders (local variable will be lost
;;              on file update).
;;          eide-menu-local-highlighted-symbols-list-backup : backup of list of
;;              highlighted symbols (local variable will be lost on file
;;              update).
;; ----------------------------------------------------------------------------
(defun eide-menu-buffer-update-start (p-buffer-name)
  (save-excursion
    (set-buffer p-buffer-name)
    (setq eide-menu-local-functions-unfolded-flag-backup eide-menu-local-functions-unfolded-flag)
    (setq eide-menu-local-unfolded-symbols-folders-list-backup eide-menu-local-unfolded-symbols-folders-list)
    (setq eide-menu-local-highlighted-symbols-list-backup eide-menu-local-highlighted-symbols-list)))

;; ----------------------------------------------------------------------------
;; Update a file in "menu" buffer.
;;
;; input  : p-buffer-name : buffer name.
;;          eide-menu-local-functions-unfolded-flag-backup : backup of
;;              functions unfolded status (to restore local variable).
;;          eide-menu-local-unfolded-symbols-folders-list-backup : backup of
;;              list of unfolded symbols folders (to restore local variable).
;;          eide-menu-local-highlighted-symbols-list-backup : backup of list of
;;              of highlighted symbols (to restore local variable).
;; ----------------------------------------------------------------------------
(defun eide-menu-buffer-update-stop (p-buffer-name)
  (save-excursion
    (set-buffer p-buffer-name)
    (make-local-variable 'eide-menu-local-functions-unfolded-flag)
    (setq eide-menu-local-functions-unfolded-flag eide-menu-local-functions-unfolded-flag-backup)
    (make-local-variable 'eide-menu-local-unfolded-symbols-folders-list)
    (setq eide-menu-local-unfolded-symbols-folders-list eide-menu-local-unfolded-symbols-folders-list-backup)
    (make-local-variable 'eide-menu-local-highlighted-symbols-list)
    (setq eide-menu-local-highlighted-symbols-list eide-menu-local-highlighted-symbols-list-backup)
    (make-local-variable 'eide-menu-local-edit-status)
    (setq eide-menu-local-edit-status (eide-edit-get-buffer-status))
    (if eide-config-show-svn-status-flag
      (progn
        (make-local-variable 'eide-menu-local-svn-modified-status-flag)
        (setq eide-menu-local-svn-modified-status-flag (eide-svn-is-current-buffer-modified-p)))))
  (eide-windows-select-menu-window)
  ;; Move one line backward, because current position might be changed by
  ;; deletion/insertion of text
  (forward-line -1)
  (save-excursion
    (forward-line)
    (eide-i-menu-remove-file)
    (eide-i-menu-insert-file p-buffer-name))
  ;; Move one line forward, to restore expected position.
  (forward-line)
  ;; Select "source" window
  ;; After operation on a file, user might be interested in editing this file.
  ;; If he wants to make other operations on files, he doesn't need window
  ;; "menu" to be selected anyway.
  (eide-windows-select-source-window t))

;; ----------------------------------------------------------------------------
;; Prepare update of a directory in "menu" buffer.
;;
;; input  : p-directory-name : directory name.
;; output : eide-menu-local-functions-unfolded-flags-list : list of unfolded
;;              status for all files.
;;          eide-menu-local-unfolded-symbols-folders-lists-list : list of lists
;;              of unfolded symbols folders for all files.
;;          eide-menu-local-highlighted-symbols-lists-list : list of lists of
;;              highlighted symbols for all files.
;; ----------------------------------------------------------------------------
(defun eide-menu-directory-update-start (p-directory-name)
  (setq eide-menu-local-functions-unfolded-flags-list nil)
  (setq eide-menu-local-unfolded-symbols-folders-lists-list nil)
  (setq eide-menu-local-highlighted-symbols-lists-list nil)
  ;; Save unfolded status for all files located in this directory
  (dolist (l-buffer-name eide-menu-files-list)
    (if (eide-menu-is-file-in-directory-p l-buffer-name p-directory-name)
      (save-excursion
        (set-buffer l-buffer-name)
        (push eide-menu-local-functions-unfolded-flag eide-menu-local-functions-unfolded-flags-list)
        (push eide-menu-local-unfolded-symbols-folders-list eide-menu-local-unfolded-symbols-folders-lists-list)
        (push eide-menu-local-highlighted-symbols-list eide-menu-local-highlighted-symbols-lists-list))))
  (setq eide-menu-local-functions-unfolded-flags-list (reverse eide-menu-local-functions-unfolded-flags-list))
  (setq eide-menu-local-unfolded-symbols-folders-lists-list (reverse eide-menu-local-unfolded-symbols-folders-lists-list))
  (setq eide-menu-local-highlighted-symbols-lists-list (reverse eide-menu-local-highlighted-symbols-lists-list)))

;; ----------------------------------------------------------------------------
;; Update a directory in "menu" buffer.
;;
;; input  : p-directory-name : directory name.
;;          eide-menu-local-functions-unfolded-flags-list : list of unfolded
;;              status for all files.
;;          eide-menu-local-unfolded-symbols-folders-lists-list : list of lists
;;              of unfolded symbols folders for all files.
;;          eide-menu-local-highlighted-symbols-lists-list : list of lists of
;;              highlighted symbols for all files.
;; ----------------------------------------------------------------------------
(defun eide-menu-directory-update-stop (p-directory-name)
  ;; Restore unfolded status and highlighted functions for all files located in this directory
  (dolist (l-buffer-name eide-menu-files-list)
    (if (eide-menu-is-file-in-directory-p l-buffer-name p-directory-name)
      (save-excursion
        (set-buffer l-buffer-name)
        (make-local-variable 'eide-menu-local-functions-unfolded-flag)
        (setq eide-menu-local-functions-unfolded-flag (pop eide-menu-local-functions-unfolded-flags-list))
        (make-local-variable 'eide-menu-local-unfolded-symbols-folders-list)
        (setq eide-menu-local-unfolded-symbols-folders-list (pop eide-menu-local-unfolded-symbols-folders-lists-list))
        (make-local-variable 'eide-menu-local-highlighted-symbols-list)
        (setq eide-menu-local-highlighted-symbols-list (pop eide-menu-local-highlighted-symbols-lists-list))
        (make-local-variable 'eide-menu-local-edit-status)
        (setq eide-menu-local-edit-status (eide-edit-get-buffer-status))
        (if eide-config-show-svn-status-flag
          (progn
            (make-local-variable 'eide-menu-local-svn-modified-status-flag)
            (setq eide-menu-local-svn-modified-status-flag (eide-svn-is-current-buffer-modified-p)))))))
  (eide-windows-select-menu-window)
  ;; Move one line backward, because current position might be changed by
  ;; deletion/insertion of text
  (forward-line -1)
  (save-excursion
    (forward-line)
    (let ((l-directory-full-name nil))
      (if (equal (get-text-property (point) 'face) 'eide-config-menu-directory-out-of-project-face)
        (setq l-directory-full-name p-directory-name)
        (setq l-directory-full-name (concat eide-root-directory p-directory-name)))
      (eide-i-menu-remove-directory)
      (eide-i-menu-insert-directory l-directory-full-name)))
  ;; Move one line forward, to restore expected position.
  (forward-line)
  ;; Select "source" window
  ;; After operation on a file, user might be interested in editing this file.
  ;; If he wants to make other operations on files, he doesn't need window
  ;; "menu" to be selected anyway.
  (eide-windows-select-source-window t))

;; ----------------------------------------------------------------------------
;; Load a file without using advice (when "menu" buffer must not be updated).
;;
;; input  : p-buffer-name : buffer name.
;;          p-directory-name : directory name.
;; return : t or nil.
;; ----------------------------------------------------------------------------
(defun eide-menu-is-file-in-directory-p (p-buffer-name p-directory-name)
  ;; Extract the "short" directory from the buffer file name
  (string-equal p-directory-name (eide-project-get-short-directory (file-name-directory (buffer-file-name (get-buffer p-buffer-name))))))

;; ----------------------------------------------------------------------------
;; Revert current file from disk.
;; ----------------------------------------------------------------------------
(defun eide-menu-revert-buffer ()
  (interactive)
  (eide-windows-select-source-window nil)
  (let ((l-functions-unfolded-flag eide-menu-local-functions-unfolded-flag)
        (l-unfolded-symbols-folders-list eide-menu-local-unfolded-symbols-folders-list)
        (l-functions-with-highlight eide-menu-local-highlighted-symbols-list))
    (revert-buffer)

    ;; NB: This part of code was in find-file-hook, which has been moved to
    ;; switch-to-buffer advice. But with revert-buffer, switch-to-buffer is not
    ;; called (while find-file-hook was). Therefore, this part of code has been
    ;; moved here.

    ;; Preserve local variables (necessary for menu update)
    (make-local-variable 'eide-menu-local-functions-unfolded-flag)
    (setq eide-menu-local-functions-unfolded-flag l-functions-unfolded-flag)
    (make-local-variable 'eide-menu-local-unfolded-symbols-folders-list)
    (setq eide-menu-local-unfolded-symbols-folders-list l-unfolded-symbols-folders-list)
    (make-local-variable 'eide-menu-local-highlighted-symbols-list)
    (setq eide-menu-local-highlighted-symbols-list l-functions-with-highlight)
    (make-local-variable 'eide-menu-local-edit-status)
    (setq eide-menu-local-edit-status (eide-edit-get-buffer-status))
    (if eide-config-show-svn-status-flag
      (progn
        (make-local-variable 'eide-menu-local-svn-modified-status-flag)
        (setq eide-menu-local-svn-modified-status-flag (eide-svn-is-current-buffer-modified-p)))))
  ;; Update menu (complete refresh, in case file has changed (read/write status...)
  (eide-menu-update t t))

;; ----------------------------------------------------------------------------
;; Close current file.
;; ----------------------------------------------------------------------------
(defun eide-menu-kill-buffer ()
  (interactive)
  (eide-windows-select-source-window nil)
  (kill-this-buffer)
  (eide-windows-skip-unwanted-buffers-in-source-window))

;; ----------------------------------------------------------------------------
;; Open directory (dired mode).
;; ----------------------------------------------------------------------------
(defun eide-menu-dired-open ()
  (eide-windows-select-source-window nil)
  (find-file default-directory))

;; ----------------------------------------------------------------------------
;; Start browsing mode (dired and buffer menu modes).
;; ----------------------------------------------------------------------------
(defun eide-menu-browsing-mode-start ()
  (if eide-windows-is-layout-visible-flag
    (progn
      (setq eide-i-menu-layout-should-be-built-after-browsing-mode-flag t)
      (eide-windows-layout-unbuild)))
  (eide-keys-configure-for-special-buffer)
  (setq eide-menu-browsing-mode-flag t))

;; ----------------------------------------------------------------------------
;; Stop browsing mode (dired and buffer menu modes).
;; ----------------------------------------------------------------------------
(defun eide-menu-browsing-mode-stop ()
  (eide-keys-configure-for-editor) ;; must be done first, for eide-i-windows-get-window-for-buffer
  (eide-windows-skip-unwanted-buffers-in-source-window)
  (if eide-i-menu-layout-should-be-built-after-browsing-mode-flag
    (progn
      ;; Build windows layout
      (eide-windows-layout-build)
      (setq eide-i-menu-layout-should-be-built-after-browsing-mode-flag nil)))
  ;; Kill all browsing buffers
  (dolist (l-buffer-name (mapcar 'buffer-name (buffer-list)))
    (save-excursion
      (set-buffer l-buffer-name)
      (if (or (equal major-mode 'dired-mode)
              (equal major-mode 'Buffer-menu-mode))
        (kill-buffer l-buffer-name))))
  (setq eide-menu-browsing-mode-flag nil))


;;;; ==========================================================================
;;;; KEYMAPS
;;;; ==========================================================================

(setq directory-name-map (make-sparse-keymap))
(define-key directory-name-map [mouse-3] 'eide-popup-open-menu-for-directory)

(setq file-name-map (make-sparse-keymap))
(define-key file-name-map [mouse-1] 'eide-i-menu-file-open)
(define-key file-name-map [mouse-3] 'eide-popup-open-menu-for-file)

(setq unfold-functions-map (make-sparse-keymap))
(define-key unfold-functions-map [mouse-1] 'eide-i-menu-file-unfold-functions)

(setq unfold-symbols-folder-map (make-sparse-keymap))
(define-key unfold-symbols-folder-map [mouse-1] 'eide-i-menu-file-unfold-symbols-folder)

(setq function-name-highlight-map (make-sparse-keymap))
(define-key function-name-highlight-map [mouse-1] 'eide-i-menu-file-highlight-function)

(setq function-name-map (make-sparse-keymap))
(define-key function-name-map [mouse-1] 'eide-i-menu-goto-function)

;;; eide-menu.el ends here
