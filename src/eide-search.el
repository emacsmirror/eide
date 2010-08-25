;;; eide-search.el --- Emacs-IDE, search

;; Copyright (C) 2005-2009 Cédric Marie

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(provide 'eide-search)

(require 'etags)

(defvar eide-search-find-symbol-definition-flag nil)

;; grep-find commands should exclude following files :
;; */.svn/*           : Subversion files
;; *.o, *.dbg         : Objects
;; *.d                : Dependencies
;; *.a                : Librairies
;; *.ref, *.new, TAGS : Emacs files
;; *.out              : Executable
;; *.map              : Mapping
(defvar eide-search-grep-find-file-filter "! -path \"*/.svn/*\" ! -name \"*.[oa]\" ! -name \"*.dbg\" ! -name \"*.d\" ! -name \"\.*\" ! -name \"*.ref\" ! -name \"*.new\" ! -name \"*.out\" ! -name \"*.map\" ! -name \"TAGS\" ! -name \".emacs.desktop\"")

(defvar eide-search-tag-string nil)


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Get string to search (either selected text, or word at cursor position).
;;
;; return : string to search.
;; ----------------------------------------------------------------------------
(defun eide-l-search-get-string-to-search ()
  (if (eq mark-active t)
    ;; Text is selected
    (if (= (count-screen-lines (region-beginning) (region-end) t) 1)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (progn
        (message "Text is selected over several lines : cannot search for it...")
        nil))
    ;; No text is selected
    (progn
      (setq l-string (find-tag-default)) ; (cscope-extract-symbol-at-cursor nil)
      (if l-string
        l-string
        (progn
          (message "No text to search at cursor position...")
          nil)))))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Go back from definition.
;; ----------------------------------------------------------------------------
(defun eide-search-back-from-tag ()
  (interactive)
  (eide-windows-select-window-file nil)
  (call-interactively 'pop-tag-mark)
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Go to definition of a symbol.
;;
;; input  : p-string : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag (p-string)
  (eide-windows-select-window-file nil)
  (find-tag p-string)
  (recenter))

;; ----------------------------------------------------------------------------
;; Go to definition of symbol at cursor position.
;;
;; output : eide-search-tag-string : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag-without-prompt ()
  (interactive)
  (setq eide-search-tag-string (eide-l-search-get-string-to-search))
  (if eide-search-tag-string
    (eide-search-find-tag eide-search-tag-string)))

;; ----------------------------------------------------------------------------
;; Go to definition of a symbol (prompt for it).
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag-with-prompt ()
  (interactive)
  (eide-windows-select-window-file nil)
  (call-interactively 'find-tag)
  ;; Saving string is necessary for calling eide-search-find-alternate-tag
  ;; later on... but there is no completion !
  ;;(setq eide-search-tag-string (read-string "Go to symbol definition : "))
  ;;(if (string-equal eide-search-tag-string "")
  ;;  (message "Cannot find empty symbol...")
  ;;  (eide-search-find-tag eide-search-tag-string))
  (recenter))

;; ----------------------------------------------------------------------------
;; Go to alternate definition of previously searched symbol.
;;
;; input  : eide-search-tag-string : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-alternate-tag ()
  (interactive)
  (eide-windows-select-window-file nil)
  (call-interactively 'pop-tag-mark)
  (find-tag eide-search-tag-string t)
  (recenter))

;; ----------------------------------------------------------------------------
;; Go back from definition.
;; ----------------------------------------------------------------------------
(defun eide-search-back-from-symbol-definition ()
  (interactive)
  (eide-windows-select-window-file nil)
  (cscope-pop-mark)
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Go to definition of symbol at cursor position.
;;
;; output : eide-search-find-symbol-definition-flag : t = pending display
;;              update in switch-to-buffer advice.
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-definition-without-prompt ()
  (interactive)
  (setq eide-search-find-symbol-definition-flag t)
  (cscope-find-global-definition-no-prompting)
  ;; Update menu because a new file may have been opened
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Go to definition of a symbol (prompt for it).
;;
;; output : eide-search-cscope-string : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-definition-with-prompt ()
  (interactive)
  (setq eide-search-cscope-string (find-tag-default))
  (eide-windows-select-window-file nil)
  ;; TODO : remplacer find-tag par la bonne commande cscope
  (call-interactively 'find-tag eide-search-cscope-string)
  (recenter))

;; ----------------------------------------------------------------------------
;; Find a symbol.
;;
;; input  : p-symbol : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol (p-symbol)
  (eide-windows-select-window-results)
  (let ((l-result-buffer-name (concat "*cscope* : " p-symbol)))
    (setq l-do-it-flag t)
    (if (get-buffer l-result-buffer-name)
      (if (eide-popup-question-yes-or-no-p "This symbol has already been found... Find again (or use available result) ?")
        ;; Delete existing find-symbol buffer
        (kill-buffer l-result-buffer-name)
        (setq l-do-it-flag nil)))
    (if l-do-it-flag
      (progn
        (cscope-find-this-symbol p-symbol)
        (save-excursion
          (set-buffer "*cscope*")
          (rename-buffer l-result-buffer-name t))
        (eide-menu-build-files-lists))
      ;;(eide-menu-update nil)))
      (eide-search-view-result-buffer l-result-buffer-name))
    (eide-windows-select-window-file t)))

;; ----------------------------------------------------------------------------
;; Find a symbol (prompt for it).
;;
;; output : eide-search-cscope-string : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-with-prompt ()
  (interactive)
  (if eide-project-cscope-files-flag
    (progn
      (setq eide-search-cscope-string (read-string "Find symbol (in whole project) : "))
      (if (string-equal eide-search-cscope-string "")
        (message "Cannot find empty symbol...")
        (eide-search-find-symbol eide-search-cscope-string)))
    (message "Cannot use cscope : there is no C/C++ file in this project...")))

;; ----------------------------------------------------------------------------
;; Find symbol at cursor position.
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-without-prompt ()
  (interactive)
  (if eide-project-cscope-files-flag
    (progn
      (setq eide-search-cscope-string (eide-l-search-get-string-to-search))
      (if eide-search-cscope-string
        (eide-search-find-symbol eide-search-cscope-string)))
    (message "Cannot use cscope : there is no C/C++ file in this project...")))

;; ----------------------------------------------------------------------------
;; Grep a string in current directory.
;;
;; input  : p-string : string.
;; ----------------------------------------------------------------------------
(defun eide-search-grep (p-string)
  (eide-windows-select-window-file t)
  ;; Get current file directory, because shell init file may change current
  ;; directory before grep is executed
  (setq l-buffer-directory (file-name-directory (buffer-file-name)))
  (let ((l-result-buffer-name (concat "*grep* : " p-string "    (in " (eide-project-get-short-directory default-directory) ")")))
    (setq l-do-it-flag t)
    (if (get-buffer l-result-buffer-name)
      (if (eide-popup-question-yes-or-no-p "This string has already been searched... Search again (or use available search result) ?")
        ;; Delete existing grep buffer
        (kill-buffer l-result-buffer-name)
        (setq l-do-it-flag nil)))
    (if l-do-it-flag
      (progn
        (if (eq system-type 'windows-nt)
          (grep-find (concat "echo ; cd " l-buffer-directory " ; find . -maxdepth 1 -type f " eide-search-grep-find-file-filter " -exec grep -Ine \"" p-string "\" {} NUL \\;"))
          (grep-find (concat "echo ; cd " l-buffer-directory " ; find . -maxdepth 1 -type f " eide-search-grep-find-file-filter " -exec grep -IHne \"" p-string "\" {} \\;")))
        (save-excursion
          (set-buffer "*grep*")
          (rename-buffer l-result-buffer-name t))
        (eide-menu-build-files-lists))
      (eide-search-view-result-buffer l-result-buffer-name))
    (eide-windows-select-window-file t)))

;; ----------------------------------------------------------------------------
;; Grep word at cursor position, in current directory.
;; ----------------------------------------------------------------------------
(defun eide-search-grep-without-prompt ()
  (interactive)
  (let ((l-string (eide-l-search-get-string-to-search)))
    (if l-string
      (eide-search-grep l-string))))

;; ----------------------------------------------------------------------------
;; Grep a string in current directory (prompt for it).
;; ----------------------------------------------------------------------------
(defun eide-search-grep-with-prompt ()
  (interactive)
  (let ((l-string (read-string "Grep (in current directory) : ")))
    (if (string-equal l-string "")
      (message "Cannot grep empty string...")
      (eide-search-grep l-string))))

;; Grep-find

;; (find-tag-default) : renvoie le mot pointé par le curseur

;; La commande "sed" permet d'insérer un retour à la ligne après le nom du
;; fichier et le numéro de ligne

;; Utiliser la commande "cd" avant "find" permettrait de raccourcir les noms des
;; fichiers affichés (chemins relatifs par rapport à la base du projet) :
;;
;;              "cd " (substring tags-file-name 0 -4) " ; find . -type f [...]
;; au lieu de "find " (substring tags-file-name 0 -4) " -type f          [...]
;;
;; Malheureusement il ne retrouve pas le chemin lorsqu'on tente d'y accéder, car
;; sa référence (à partir de laquelle il va ajouter le chemin relatif), c'est
;; toujours le répertoire courant (celui du buffer actif) : on est donc obligé
;; d'utiliser le chemin absolu.
;; (Autre solution, plus compliquée : comparer le répertoire courant au
;; répertoire projet pour déterminer de combien de répertoires il faut remonter,
;; et utiliser ainsi un chemin relatif par rapport au buffer courant. Ceci dit,
;; des ../../.. ne sont pas forcément plus élégants qu'un chemin absolu !...)

;; ----------------------------------------------------------------------------
;; Grep a string in the whole project.
;;
;; input  : p-string : string.
;;          eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-search-grep-find (p-string)
  ;; On Emacs 22 GTK : it is necessary to select window "file", otherwise
  ;; current result buffer will be reused if window "results" is selected.
  (eide-windows-select-window-file t)
  (let ((l-result-buffer-name (concat "*grep-find* : " p-string)))
    (setq l-do-it-flag t)
    (if (get-buffer l-result-buffer-name)
      (if (eide-popup-question-yes-or-no-p "This string has already been searched... Search again (or use available search result) ?")
        ;; Delete existing grep buffer
        (kill-buffer l-result-buffer-name)
        (setq l-do-it-flag nil)))
    (if l-do-it-flag
      (progn
        (if eide-option-search-grep-find-on-2-lines-flag
          (grep-find (concat "find " eide-root-directory " -type f " eide-search-grep-find-file-filter " -exec grep -Ine \"" p-string "\" {} NUL \\;; grep -v \"^Binary\" | sed 's/[0-9]:/\&\\\n/'"))
          (if (eq system-type 'windows-nt)
            (grep-find (concat "echo ; find " eide-root-directory " -type f " eide-search-grep-find-file-filter " -exec grep -Ine \"" p-string "\" {} NUL \\;"))
            (grep-find (concat "echo ; find " eide-root-directory " -type f " eide-search-grep-find-file-filter " -exec grep -IHne \"" p-string "\" {} \\;"))))
        (save-excursion
          (set-buffer "*grep*")
          (rename-buffer l-result-buffer-name t))
        (eide-menu-build-files-lists))
      (eide-search-view-result-buffer l-result-buffer-name))
    (eide-windows-select-window-file t)))

;; ----------------------------------------------------------------------------
;; Grep word at cursor position, in the whole project.
;; ----------------------------------------------------------------------------
(defun eide-search-grep-find-without-prompt ()
  (interactive)
  (let ((l-string (eide-l-search-get-string-to-search)))
    (if l-string
      (eide-search-grep-find l-string))))

;; ----------------------------------------------------------------------------
;; Grep a string in the whole project (prompt for it).
;; ----------------------------------------------------------------------------
(defun eide-search-grep-find-with-prompt ()
  (interactive)
  (let ((l-string (read-string "Grep (in whole project) : ")))
    (if (string-equal l-string "")
      (message "Cannot grep empty string...")
      (eide-search-grep-find l-string))))

;; ----------------------------------------------------------------------------
;; Go to previous grep match (or compile error).
;; ----------------------------------------------------------------------------
(defun eide-search-grep-go-to-previous ()
  (interactive)
  ;; Move to window "file" to make sure that the buffer won't be displayed
  ;; in window "menu"
  ;;(eide-windows-select-window-file nil)
  (previous-error)
  (if (not eide-windows-is-layout-visible-flag)
    ;; Close grep window (appears automatically with previous-error)
    (delete-other-windows))
  (recenter)
  ;; Update menu because a new file may have been opened
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Go to next grep match (or compile error).
;; ----------------------------------------------------------------------------
(defun eide-search-grep-go-to-next ()
  (interactive)
  ;; Move to window "file" to make sure that the buffer won't be displayed
  ;; in window "menu"
  ;;(eide-windows-select-window-file nil)
  (next-error)
  (if (not eide-windows-is-layout-visible-flag)
    ;; Close grep window (appears automatically with next-error)
    (delete-other-windows))
  (recenter)
  ;; Update menu because a new file may have been opened
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Display a result buffer.
;; ----------------------------------------------------------------------------
(defun eide-search-view-result-buffer (p-result-buffer-name)
  (eide-windows-select-window-results)
  (switch-to-buffer p-result-buffer-name))

;; ----------------------------------------------------------------------------
;; Close a grep result buffer.
;;
;; input  : p-grep-buffer-name : grep result buffer name.
;;          eide-menu-grep-results-list : grep results list.
;; output : eide-menu-grep-results-list : updated grep results list.
;; ----------------------------------------------------------------------------
(defun eide-search-close-grep-buffer (p-grep-buffer-name)
  (eide-windows-select-window-results)
  (let ((l-buffer (buffer-name)))
    (kill-buffer p-grep-buffer-name)
    (setq eide-menu-grep-results-list (remove p-grep-buffer-name eide-menu-grep-results-list))

    (if (string-equal p-grep-buffer-name l-buffer)
      ;; It was current result buffer : display another one
      (progn
        (setq l-buffer (car eide-menu-grep-results-list))
        (if l-buffer
          (switch-to-buffer l-buffer)
          (progn
            (setq l-buffer (car eide-menu-cscope-results-list))
            (if l-buffer
              (switch-to-buffer l-buffer)
              (switch-to-buffer "*results*"))))))))

;; ----------------------------------------------------------------------------
;; Close all grep result buffers.
;;
;; input  : eide-menu-grep-results-list : grep results list.
;;          eide-menu-cscope-results-list : cscope results list.
;; output : eide-menu-grep-results-list : updated grep results list (nil).
;; ----------------------------------------------------------------------------
(defun eide-search-close-all-grep-buffers ()
  (eide-windows-select-window-results)
  (let ((l-buffer (buffer-name)))
    (dolist (l-grep-buffer-name eide-menu-grep-results-list)
      (kill-buffer l-grep-buffer-name))
    (setq eide-menu-grep-results-list nil)

    (if (not (string-equal (buffer-name) l-buffer))
      ;; It was a grep result buffer : display another search buffer
      (progn
        (setq l-buffer (car eide-menu-cscope-results-list))
        (if l-buffer
          (switch-to-buffer l-buffer)
          (switch-to-buffer "*results*"))))))

;; ----------------------------------------------------------------------------
;; Close a cscope result buffer.
;;
;; input  : p-cscope-buffer-name : cscope result buffer name.
;;          eide-menu-cscope-results-list : cscope results list.
;; output : eide-menu-cscope-results-list : updated cscope results list.
;; ----------------------------------------------------------------------------
(defun eide-search-close-cscope-buffer (p-cscope-buffer-name)
  (eide-windows-select-window-results)
  (let ((l-buffer (buffer-name)))
    (kill-buffer p-cscope-buffer-name)
    (setq eide-menu-cscope-results-list (remove p-cscope-buffer-name eide-menu-cscope-results-list))

    (if (string-equal p-cscope-buffer-name l-buffer)
      ;; It was current result buffer : display another one
      (progn
        (setq l-buffer (car eide-menu-cscope-results-list))
        (if l-buffer
          (switch-to-buffer l-buffer)
          (progn
            (setq l-buffer (car eide-menu-grep-results-list))
            (if l-buffer
              (switch-to-buffer l-buffer)
              (switch-to-buffer "*results*"))))))))

;; ----------------------------------------------------------------------------
;; Close all cscope result buffers.
;;
;; input  : eide-menu-cscope-results-list : cscope results list.
;;          eide-menu-grep-results-list : grep results list.
;; output : eide-menu-cscope-results-list : updated cscope results list (nil).
;; ----------------------------------------------------------------------------
(defun eide-search-close-all-cscope-buffers ()
  (eide-windows-select-window-results)
  (let ((l-buffer (buffer-name)))
    (dolist (l-cscope-buffer-name eide-menu-cscope-results-list)
      (kill-buffer l-cscope-buffer-name))
    (setq eide-menu-cscope-results-list nil)

    (if (not (string-equal (buffer-name) l-buffer))
      ;; It was a grep result buffer : display another search buffer
      (progn
        (setq l-buffer (car eide-menu-grep-results-list))
        (if l-buffer
          (switch-to-buffer l-buffer)
          (switch-to-buffer "*results*"))))))

;;; eide-search.el ends here
