;;; eide-menu.el --- Emacs-IDE, menu

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

(provide 'eide-menu)

(require 'speedbar)

;; Pas de regroupement des tags
(setq speedbar-tag-hierarchy-method nil)
;; Donne le focus après sélection
(setq speedbar-activity-change-focus-flag t)
;; Show all files in directory browser
(setq speedbar-show-unknown-files t)

(setq speedbar-frame-parameters
  '((minibuffer)
    (left . 140)
    (top . 64)
    (width . 100)
    (height . 40)
    (border-width . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (unsplittable . t) ))

(setq speedbar-visiting-file-hook 'eide-menu-internal-speedbar-close-hook)
(setq speedbar-visiting-tag-hook  'eide-menu-internal-speedbar-close-hook)

;(setq speedbar-load-hook 'eide-load-speedbar-hook)
(eval-when-compile (require 'eide-custom))

(eval-when-compile (require 'eide-windows))
(eval-when-compile (require 'eide-toolbar))

(setq eide-menu-functions-unfolded nil)
(setq eide-menu-functions-with-highlight nil)

(setq eide-menu-update-run-buffer nil)

(setq eide-menu-update-functions-unfolded nil)
(setq eide-menu-update-functions-with-highlight nil)


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Hook to be executed after a file or a function has been selected in speedbar
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-speedbar-close-hook ()
  (speedbar-close-frame)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Insert text in "menu" buffer (with white background)
;;
;; input  :   string : string to insert
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-insert-text (string)
  (if eide-option-buffer-menu-white-background-flag
    (progn
      (if (and eide-option-buffer-menu-stuff-line-with-spaces-flag (string-equal string "\n"))
        (if (= (current-column) 0)
          (setq string (concat (eide-menu-internal-stuff-string-with-spaces "š") "\n"))
          (setq string (concat (eide-menu-internal-stuff-string-with-spaces " ") "\n"))))
      (put-text-property (point) (progn (insert string) (point)) 'face 'eide-menu-white-background-face))
    (insert string)))

;; ----------------------------------------------------------------------------
;; Insert a list of functions in "menu" buffer
;;
;; input  :   my-function-list                : list of functions
;;            my-function-with-highlight-list : list of functions to highlight
;;                                              (present in my-function-list)
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-insert-functions ()
  (if eide-option-buffer-menu-bug-highlight-flag
    (progn
      (forward-line -1)
      (end-of-line)
      (eide-menu-internal-insert-text "\n")))

  (if my-function-list
    (dolist (this-function my-function-list)
;      (eide-menu-internal-insert-text "  --> ")
      (eide-menu-internal-insert-text "  ")
      (put-text-property (setq from-point (point)) (progn (eide-menu-internal-insert-text "-->") (point)) 'keymap function-name-highlight-map)
      (put-text-property from-point (point) 'mouse-face 'highlight)
      (eide-menu-internal-insert-text " ")
      (put-text-property (setq from-point (point)) (progn (insert (car this-function)) (point)) 'keymap function-name-map)
      (setq is-highlighted nil)
      (dolist (this-function-in-list my-function-with-highlight-list)
        (if (string-equal (car this-function) this-function-in-list)
          (setq is-highlighted t)))
      (if is-highlighted
        (put-text-property from-point (point) 'face 'eide-menu-function-with-highlight-face)
        (put-text-property from-point (point) 'face 'eide-menu-function-face))
      (put-text-property from-point (point) 'mouse-face 'highlight)
      (eide-menu-internal-insert-text "\n"))
    (progn
      (put-text-property (point) (progn (insert "      (no function)") (point)) 'face 'eide-menu-empty-face)
      (eide-menu-internal-insert-text "\n")))

  (if eide-option-buffer-menu-bug-highlight-flag
    (delete-char 1)))

;; ----------------------------------------------------------------------------
;; Insert a file - and its functions if unfolded - in "menu" buffer
;;
;; input  :   string : file buffer name
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-insert-filename-and-functions (string)
  (let ((buffer-read-only nil))
  (save-excursion
    (set-buffer string)
    (setq my-buffer-filename buffer-file-name)
    (setq my-buffer-status (eide-edit-get-buffer-status))
    (setq local-unfolded eide-menu-functions-unfolded)
    ;; Check buffer status (r/w, modified)
    (if buffer-read-only
      (setq local-rw nil)
      (setq local-rw t))
    (if (buffer-modified-p)
      (setq local-mod t)
      (setq local-mod nil))
    ;; If the buffer is unfolded, get functions list
    (if local-unfolded
      (save-excursion
        (setq my-function-list (imenu--generic-function imenu-generic-expression))
        (setq my-function-with-highlight-list eide-menu-functions-with-highlight))))

  ;; Check if this is current buffer
  (if (string-equal eide-current-buffer string)
    (setq this-is-current t)
    (setq this-is-current nil) )

  (if eide-option-buffer-menu-bug-highlight-flag
    (progn
      (forward-line -1)
      (end-of-line)
      (eide-menu-internal-insert-text "\n")))

  (if local-unfolded
    (put-text-property (setq from-point (point)) (progn (eide-menu-internal-insert-text "(-)") (point)) 'keymap unfold-functions-map)
    (put-text-property (setq from-point (point)) (progn (eide-menu-internal-insert-text "(+)") (point)) 'keymap unfold-functions-map))
  (put-text-property from-point (point) 'mouse-face 'highlight)
  (eide-menu-internal-insert-text " ")
  (put-text-property (setq from-point (point)) (progn (insert string) (point)) 'keymap file-name-map)
;  (if this-is-current
;    (put-text-property from-point (point) 'face 'eide-menu-current-file-face)

  (if this-is-current
    (if (string-equal my-buffer-status "ref")
      (put-text-property from-point (point) 'face 'eide-menu-current-file-ref-face)
      (if (string-equal my-buffer-status "new")
        (put-text-property from-point (point) 'face 'eide-menu-current-file-new-face)
        (if local-rw
          (put-text-property from-point (point) 'face 'eide-menu-current-file-rw-face)
          (put-text-property from-point (point) 'face 'eide-menu-current-file-face))))
    (if (string-equal my-buffer-status "ref")
      (put-text-property from-point (point) 'face 'eide-menu-file-ref-face)
      (if (string-equal my-buffer-status "new")
        (put-text-property from-point (point) 'face 'eide-menu-file-new-face)
        (if local-rw
          (put-text-property from-point (point) 'face 'eide-menu-file-rw-face)
          (put-text-property from-point (point) 'face 'eide-menu-file-face)))))
  (put-text-property from-point (point) 'mouse-face 'highlight)

  ;; Add a space after filename, because otherwise, with some versions of
  ;; emacs, property applies on whole line ("\n")
  (eide-menu-internal-insert-text " ")

  (if local-mod
    (eide-menu-internal-insert-text "*"))

  (if (get-buffer (concat "* clean : " string))
    (progn
      (eide-menu-internal-insert-text " ")
      (put-text-property (setq from-point (point)) (progn (eide-menu-internal-insert-text "(clean)") (point)) 'keymap file-name-clean-map)
      (put-text-property from-point (point) 'mouse-face 'highlight)))

  (if this-is-current
    (save-excursion
      (beginning-of-line)
      (forward-char)
      (setq eide-menu-current-buffer-marker (point-marker))))

  (if eide-option-buffer-menu-bug-highlight-flag
    (progn
      (eide-menu-internal-insert-text " ")
      (beginning-of-line)
      (forward-line))
    (eide-menu-internal-insert-text "\n"))

  (if local-unfolded
    (eide-menu-internal-insert-functions))))

;; ----------------------------------------------------------------------------
;; Insert a list of files - grouped by directory - in "menu" buffer
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-insert-directories-and-filenames (this-buffer-list)
  (setq this-directory-list nil)

  ;; First, parse the list of buffers to built the list of directories
  (dolist (this-buffer this-buffer-list)
    ;; Extract the directory from the buffer file name
    (setq this-directory (file-name-directory (buffer-file-name (get-buffer this-buffer))))
    ;; If this is the first buffer from this directory, add the directory to the list
    (if (not (member this-directory this-directory-list))
      (setq this-directory-list (cons this-directory this-directory-list))))
  ;; Sort the list with alphabetical order (? TODO : anglais correct ?)
  (setq this-directory-list (sort this-directory-list 'string<))

  ;; For each directory, insert the directory name, and parse the list of buffers to insert those that match
  (dolist (this-directory this-directory-list)
    (setq this-directory-short (eide-project-get-short-directory this-directory))
    (if eide-option-buffer-menu-stuff-line-with-spaces-flag
      (setq this-directory-display (eide-menu-internal-stuff-string-with-spaces this-directory-short))
      (setq this-directory-display this-directory-short))
    (if (string-equal this-directory this-directory-short)
      (setq this-directory-short nil))

    (if eide-option-buffer-menu-group-files-by-dir-with-empty-line-flag
      (eide-menu-internal-insert-text "\n"))

    (if eide-option-buffer-menu-bug-highlight-flag
      (progn
        (if this-directory-short
          (put-text-property (point) (progn (insert this-directory-display) (point)) 'face 'eide-menu-directory-face)
          (put-text-property (point) (progn (insert this-directory-display) (point)) 'face 'eide-menu-directory-out-of-project-face))
        (eide-menu-internal-insert-text " \n"))
      (if this-directory-short
        (put-text-property (point) (progn (insert (concat this-directory-display "\n")) (point)) 'face 'eide-menu-directory-face)
        (put-text-property (point) (progn (insert (concat this-directory-display "\n")) (point)) 'face 'eide-menu-directory-out-of-project-face)))
    ;; Parse buffer list for buffers from this directory to display
    (dolist (this-buffer this-buffer-list)
      (setq this-buffer-directory (file-name-directory (buffer-file-name (get-buffer this-buffer))))
      (if (string-equal this-directory this-buffer-directory)
        (eide-menu-internal-insert-filename-and-functions this-buffer)))))

;; ----------------------------------------------------------------------------
;; Change current file
;;
;; input  :   my-buffer : file buffer to become current
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-update-current-buffer (my-buffer)
  (save-excursion
    (beginning-of-line)
    (forward-char)
    ;; Marker is not set on the first char, because a problem occurs when new
    ;; marker is on the line below old marker : when old file is removed (to be
    ;; displayed again later without highlight), old and new markers become
    ;; equals, and when old file is inserted, new marker remains on beginning
    ;; of line of old file. The problem is fixed if the marker is set on second
    ;; char (new marker will not be separated from the line related to new file)
    (setq eide-menu-old-current-buffer-marker eide-menu-current-buffer-marker)
    (setq eide-menu-current-buffer-marker (point-marker))

    (setq eide-old-current-buffer eide-current-buffer)
    (setq eide-current-buffer my-buffer)

    (goto-char (marker-position eide-menu-old-current-buffer-marker))
    (eide-menu-internal-remove-list-from-menu)
    (eide-menu-internal-insert-filename-and-functions eide-old-current-buffer)

    (goto-char (marker-position eide-menu-current-buffer-marker))
    (eide-menu-internal-remove-list-from-menu)
    (eide-menu-internal-insert-filename-and-functions eide-current-buffer))
  ;; Desktop is saved to avoid loss of opened buffers in case of crash of emacs
  (if (file-exists-p (concat eide-project-directory ".emacs.desktop"))
    (desktop-save eide-project-directory)))

;; ----------------------------------------------------------------------------
;; Remove a list from "menu" buffer (beginning on current line)
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-remove-list-from-menu ()
  (let ((buffer-read-only nil))
  (beginning-of-line)
  (delete-region (point) (progn (forward-line) (while (string-equal (char-to-string (char-after)) " ") (forward-line)) (point)))))

;; ----------------------------------------------------------------------------
;; Add spaces to the end of a string (for background color in "menu" buffer)
;;
;; input  : string : string to stuff with spaces
;; return : modified string
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-stuff-string-with-spaces (string)
  (setq eide-column-limit 130)
  (while (< (length string) eide-column-limit)
    (setq string (concat string " ")))
  (setq string string))

;; ----------------------------------------------------------------------------
;; Insert "file type" header in "menu" buffer
;;
;; input  :   string : file type (string)
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-insert-file-type-header-text (string)
  (eide-menu-internal-insert-text "\n")
  (if eide-option-buffer-menu-stuff-line-with-spaces-flag
    (setq string (eide-menu-internal-stuff-string-with-spaces string)))
  (put-text-property (point) (progn (insert (concat " " string "\n")) (point)) 'face 'eide-menu-file-type-header-face)
  (if (not eide-option-buffer-menu-group-files-by-dir-with-empty-line-flag)
    (eide-menu-internal-insert-text "\n")))

;; ----------------------------------------------------------------------------
;; Get index of selected object in a list
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-get-index-in-list ()
  (forward-line -1)
  (setq this-index 0)
  (while (not (string-equal (char-to-string (char-after)) "(")) (forward-line -1) (setq this-index (1+ this-index)))
  (setq this-index this-index))

;; ----------------------------------------------------------------------------
;; Update "menu" buffer
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-rebuild ()
  (let ((buffer-read-only nil))
  (delete-region (point-min) (point-max))
  (setq eide-menu-current-buffer-marker nil)

  (eide-menu-internal-insert-text "\n")

  (if eide-project-name
    (progn
      (put-text-property (point) (progn (insert (concat eide-project-type " project : ")) (point)) 'face 'eide-menu-project-type-face)
      (put-text-property (point) (progn (insert eide-project-name) (point)) 'face 'eide-menu-current-project-face))
    (put-text-property (point) (progn (insert "Root directory :") (point)) 'face 'eide-menu-project-type-face))

  (eide-menu-internal-insert-text "\n")
;  (put-text-property (point) (progn (insert eide-project-directory) (point)) 'face 'eide-menu-project-directory-face)
  (eide-menu-internal-insert-text eide-project-directory)
  (eide-menu-internal-insert-text "\n\n")

  (eide-files-build-lists) ; TODO : tri alphabétique à faire à part
  ;; (pas besoin de l'appeler si pas de groupement par type)

  (if eide-option-buffer-menu-group-files-by-type-flag
  (progn

  (if eide-menu-source-file-list
    (progn
      (eide-menu-internal-insert-file-type-header-text "Source files")
      (eide-menu-internal-insert-directories-and-filenames eide-menu-source-file-list)))

  (if eide-menu-header-file-list
    (progn
      (eide-menu-internal-insert-file-type-header-text "Header files")
      (eide-menu-internal-insert-directories-and-filenames eide-menu-header-file-list)))

  (if eide-menu-other-file-list
    (progn
      (eide-menu-internal-insert-file-type-header-text "Other files")
      (eide-menu-internal-insert-directories-and-filenames eide-menu-other-file-list)))
  )
  ;; all types of files at once
  (if eide-menu-all-file-list
;    (progn
      ;(eide-menu-internal-insert-file-type-header-text "All files")
    (eide-menu-internal-insert-directories-and-filenames eide-menu-all-file-list));)
  )

  ;; 80 blank lines, so that window "menu" seems to have white background
  (setq my-loop-count 0)
  (save-excursion
    (while (< my-loop-count 80)
      (eide-menu-internal-insert-text "\n")
      (setq my-loop-count (+ my-loop-count 1))))

  ;; Move cursor to current buffer
  (if eide-menu-current-buffer-marker
    (progn
      (goto-char (marker-position eide-menu-current-buffer-marker))
      (recenter)))))

;; ----------------------------------------------------------------------------
;; Switch to selected file
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-file-open ()
  (interactive) ; necessary for keymap
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (setq my-buffer (eide-menu-get-buffer-name-on-current-line))
  (eide-menu-internal-update-current-buffer my-buffer)
  (eide-windows-select-window-file t)
  (switch-to-buffer my-buffer))

;; ----------------------------------------------------------------------------
;; Fold / unfold list of functions for selected file
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-file-unfold-functions ()
  (interactive) ; necessary for keymap
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (save-excursion
    (setq my-buffer-name (eide-menu-get-buffer-name-on-current-line))
    (set-buffer my-buffer-name)

    (if eide-menu-functions-unfolded
      (setq eide-menu-functions-unfolded nil)
      (progn
        (make-local-variable 'eide-menu-functions-unfolded)
        (setq eide-menu-functions-unfolded t)))

    (setq local-unfolded eide-menu-functions-unfolded)
    (if local-unfolded
      (save-excursion
        (setq my-function-list (imenu--generic-function imenu-generic-expression)) )))

  (eide-menu-internal-remove-list-from-menu)

  (if eide-option-buffer-menu-bug-highlight-flag
    (forward-line -1))
  (save-excursion
    (if eide-option-buffer-menu-bug-highlight-flag
      (forward-line))
    (eide-menu-internal-insert-filename-and-functions my-buffer-name))
  (if eide-option-buffer-menu-bug-highlight-flag
    (forward-line)))

;; ----------------------------------------------------------------------------
;; Enable / disable highlight on selected function
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-file-highlight-function ()
  (interactive) ; necessary for keymap
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  ;; TODO : position non conservée (on se retrouve au niveau du nom du fichier)
  (save-excursion
    (setq my-function-index (eide-menu-internal-get-index-in-list))
    (setq my-buffer (eide-menu-get-buffer-name-on-current-line))

    (save-excursion
      (set-buffer my-buffer)
      (setq my-function (car (nth my-function-index (imenu--generic-function imenu-generic-expression))))

      (make-local-variable 'eide-menu-functions-with-highlight)

      (setq already-highlighted nil)
      ;; test nécessaire ? => remove suffit peut-être (sans effet si non présent)
      (dolist (my-function-in-list eide-menu-functions-with-highlight)
        (if (string-equal my-function my-function-in-list)
          (progn
            (setq eide-menu-functions-with-highlight (remove my-function eide-menu-functions-with-highlight))
            (setq already-highlighted t))))

      (if (not already-highlighted)
        (push my-function eide-menu-functions-with-highlight)))

    (eide-menu-internal-remove-list-from-menu)
    (eide-menu-internal-insert-filename-and-functions my-buffer)))

;; ----------------------------------------------------------------------------
;; Go to selected function
;; ----------------------------------------------------------------------------
(defun eide-menu-internal-goto-function ()
  (interactive) ; necessary for keymap
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (setq my-function-index (eide-menu-internal-get-index-in-list))
  (setq my-buffer (eide-menu-get-buffer-name-on-current-line))

  (eide-menu-internal-update-current-buffer my-buffer)
  (eide-windows-select-window-file t)
  (switch-to-buffer my-buffer)

  (goto-char (marker-position (cdr (nth my-function-index (imenu--generic-function imenu-generic-expression)))))
  (recenter))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Init "menu" buffer
;; ----------------------------------------------------------------------------
(defun eide-menu-init ()
  ;; Menu buffer is created empty (content will be built by eide-menu-update)
  (setq eide-menu-buffer (buffer-name (get-buffer-create "* Menu *")))
  (save-excursion
    (set-buffer eide-menu-buffer)
    (setq buffer-read-only t)))

;; ----------------------------------------------------------------------------
;; Update "menu" buffer
;;
;; input  :   force-update : true  => always update
;;                           false => update only if current buffer has changed
;; ----------------------------------------------------------------------------
(defun eide-menu-update (force-update)
  (if eide-windows-is-layout-visible-flag
    (progn
      ;; Save window to go back to, once menu has been updated
;        (setq my-window (selected-window))
      (eide-windows-select-window-file t)
      (setq eide-current-buffer-temp (buffer-name))
      (if force-update
        (progn
          (eide-windows-select-window-menu t)
          (setq eide-current-buffer eide-current-buffer-temp)
          (eide-menu-internal-rebuild)
          (setq eide-windows-menu-update-request-pending-flag nil))
        (if (not (string-equal eide-current-buffer eide-current-buffer-temp))
          (progn
            (eide-windows-select-window-menu t)
            (goto-char (point-min))
            (if (and (search-forward (concat " " eide-current-buffer-temp " \n") nil t) (get-buffer eide-current-buffer))
              ;; Old and new files are both present in menu : just update current buffer
              (progn
                (forward-line -1)
                (eide-menu-internal-update-current-buffer eide-current-buffer-temp))
              ;; file not present in menu : update whole menu
              (progn
                (setq eide-current-buffer eide-current-buffer-temp)
                (eide-menu-internal-rebuild)))
            (setq eide-windows-menu-update-request-pending-flag nil))))
      ;; Go back to "current window"
; pose pb avec les grep (double-click milieu)
;        (select-window my-window))
      (eide-windows-select-window-file t))
    (setq eide-windows-menu-update-request-pending-flag t)))

;; ----------------------------------------------------------------------------
;; Update current buffer "modified" status (in menu)
;; ----------------------------------------------------------------------------
(defun eide-menu-update-current-buffer-modified-status ()
  (save-excursion
    (setq this-buffer (buffer-name))
    (setq this-modified-flag (buffer-modified-p))
    (set-buffer eide-menu-buffer)
    (save-excursion
      (if (or (and this-modified-flag
                   (progn (goto-char (point-min)) (search-forward (concat " " this-buffer " \n") nil t)))
              (and (not this-modified-flag)
                   (progn (goto-char (point-min)) (search-forward (concat " " this-buffer " *\n") nil t))))
        (progn
          (forward-line -1)
          (eide-menu-internal-remove-list-from-menu)
          (eide-menu-internal-insert-filename-and-functions this-buffer))))))

;; ----------------------------------------------------------------------------
;; Get file buffer name on current line in "menu" buffer
;;
;; return : file buffer name
;; ----------------------------------------------------------------------------
(defun eide-menu-get-buffer-name-on-current-line ()
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (beginning-of-line)
  (forward-char 4)
  (buffer-substring-no-properties (point) (progn (while (not (string-equal (char-to-string (char-after)) " ")) (forward-char)) (point))))

;; ----------------------------------------------------------------------------
;; Close selected file
;;
;; output :  eide-current-buffer : current file may have changed
;; ----------------------------------------------------------------------------
(defun eide-menu-file-close (this-buffer)
  (interactive) ; necessary for keymap
  (if (featurep 'xemacs)
    (mouse-track last-input-event))

  (kill-buffer this-buffer)
  (eide-menu-internal-remove-list-from-menu)

  (if (string-equal this-buffer eide-current-buffer)
    (progn
      (eide-windows-select-window-file t)
      (eide-display-skip-unwanted-buffers nil nil)
      (eide-menu-update t)))

  ;; With option eide-option-buffer-menu-group-files-by-dir-with-empty-line-flag : eide-menu-directory-face property or "\n"
  ;; Without : "\n"
  (setq this-property (get-text-property (point) 'face))
  (if (or (equal this-property 'eide-menu-directory-face)
          (equal this-property 'eide-menu-directory-out-of-project-face)
          (string-equal (char-to-string (char-after)) "\n"))
    ;; It was the last file of the group
    (progn
      (forward-line -1)
      (setq this-property (get-text-property (point) 'face))
      (if (or (equal this-property 'eide-menu-directory-face)
              (equal this-property 'eide-menu-directory-out-of-project-face))
        ;; It was also the only one : we must delete directory line
        (let ((buffer-read-only nil))
        (if eide-option-buffer-menu-group-files-by-dir-with-empty-line-flag
          (delete-region (point) (progn (forward-line 2) (point)))
          (delete-region (point) (progn (forward-line) (point)))))))))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-menu-buffer-update-start (this-buffer)
  (save-excursion
    (set-buffer this-buffer)
    (setq unfolded-status eide-menu-functions-unfolded)))

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-menu-buffer-update-stop (this-buffer)
  (save-excursion
    (set-buffer this-buffer)
    (make-local-variable 'eide-menu-functions-unfolded)
    (setq eide-menu-functions-unfolded unfolded-status))
  ;; Move one line backward, because current position might be changed by
  ;; deletion/insertion of text
  (forward-line -1)
  (save-excursion
    (forward-line)
    (eide-menu-internal-remove-list-from-menu)
    (eide-menu-internal-insert-filename-and-functions my-buffer))
  ;; Move one line forward, to restore expected position.
  (forward-line)
  ;; Select window "file"
  ;; After operation on a file, user might be interested in editing this file.
  ;; If he wants to make other operations on files, he doesn't need window
  ;; "menu" to be selected anyway.
  (eide-windows-select-window-file t))

;; ----------------------------------------------------------------------------
;; Switch to a buffer without using advice
;;
;; input  :   my-buffer : buffer to switch to
;; ----------------------------------------------------------------------------
(defun eide-menu-switch-to-buffer-without-advice (my-buffer)
  ;; switch-to-buffer advice would not display *scratch* buffer in selected
  ;; window
  (ad-deactivate 'switch-to-buffer)
  (switch-to-buffer my-buffer)
  (ad-activate 'switch-to-buffer))

;; ----------------------------------------------------------------------------
;; Load a file without executing hook (when "menu" buffer must not be updated)
;;
;; input  :   my-file : file to be loaded
;; ----------------------------------------------------------------------------
(defun eide-menu-find-file-without-hook (my-file)
  ;; find-file advice would change eide-current-buffer
  ;; and menu buffer would be updated with temp files
  (ad-deactivate 'switch-to-buffer)
  (find-file my-file)
  (ad-activate 'switch-to-buffer))

;; ----------------------------------------------------------------------------
;; Open speedbar
;; ----------------------------------------------------------------------------
(defun eide-menu-speedbar-open ()
  (interactive)
  (eide-windows-select-window-file nil)
  (speedbar-get-focus))

;; ----------------------------------------------------------------------------
;; Hook to be executed when speedbar is opened
;; ----------------------------------------------------------------------------
;(defun eide-load-speedbar-hook ()
;  (setq scroll-preserve-screen-position nil))

;; ----------------------------------------------------------------------------
;; Revert file from disk
;; ----------------------------------------------------------------------------
(defun eide-menu-revert-buffer ()
  (interactive)
  (eide-windows-select-window-file nil)
  (setq eide-menu-update-functions-unfolded eide-menu-functions-unfolded)
  (setq eide-menu-update-functions-with-highlight eide-menu-functions-with-highlight)
  (revert-buffer)

  ;; NB : This part of code was in find-file-hook, which has been moved to
  ;; switch-to-buffer advice. But with revert-buffer, switch-to-buffer is not
  ;; called (while find-file-hook was). Therefore, this part of code has been
  ;; moved here.

  ;; Preserve local variables (necessary for menu update)
  (make-local-variable 'eide-menu-functions-unfolded)
  (make-local-variable 'eide-menu-functions-with-highlight)
  (setq eide-menu-functions-unfolded eide-menu-update-functions-unfolded)
  (setq eide-menu-functions-with-highlight eide-menu-update-functions-with-highlight)
  (setq eide-menu-update-local-var-flag nil)
  ;; Update menu (complete refresh, in case file has changed (R/W status...)
  (eide-menu-update t)

  (if (featurep 'xemacs)
    ;; update fontifying (?)
    (font-lock-fontify-buffer)))

;; ----------------------------------------------------------------------------
;; Close current file
;; ----------------------------------------------------------------------------
(defun eide-menu-kill-buffer ()
  (interactive)
  (eide-windows-select-window-file nil)
  (kill-this-buffer)
  (eide-display-skip-unwanted-buffers nil nil)
  (eide-windows-select-window-file nil))


;;;; ==========================================================================
;;;; KEYMAPS
;;;; ==========================================================================

;(eide-set-map-action help-map (quote eide-help-open))

(setq file-name-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key file-name-map [button1] 'eide-menu-internal-file-open)
  (define-key file-name-map [mouse-1] 'eide-menu-internal-file-open))
(if (featurep 'xemacs)
  (define-key file-name-map [button3] 'eide-popup-open-menu-for-file)
  (define-key file-name-map [mouse-3] 'eide-popup-open-menu-for-file))

(setq file-name-clean-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key file-name-clean-map [button1] 'eide-menu-edit-view-clean-buffer)
  (define-key file-name-clean-map [mouse-1] 'eide-menu-edit-view-clean-buffer))
(if (featurep 'xemacs)
  (define-key file-name-clean-map [button3] 'eide-popup-open-menu-for-file) ; TODO : à adapter à "clean"
  (define-key file-name-clean-map [mouse-3] 'eide-popup-open-menu-for-file))

(setq unfold-functions-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key unfold-functions-map [button1] 'eide-menu-internal-file-unfold-functions)
  (define-key unfold-functions-map [mouse-1] 'eide-menu-internal-file-unfold-functions))

(setq function-name-highlight-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key function-name-highlight-map [button1] 'eide-menu-internal-file-highlight-function)
  (define-key function-name-highlight-map [mouse-1] 'eide-menu-internal-file-highlight-function))

(setq function-name-map (make-sparse-keymap))
(if (featurep 'xemacs)
  (define-key function-name-map [button1] 'eide-menu-internal-goto-function)
  (define-key function-name-map [mouse-1] 'eide-menu-internal-goto-function))

;;; eide-menu.el ends here
