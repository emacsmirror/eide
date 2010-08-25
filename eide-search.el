;;; eide-search.el --- Emacs-IDE, search

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

(provide 'eide-search)

(require 'etags)

;; grep-find commands should exclude following files :
;; */.svn/*           : Subversion files
;; *.o, *.dbg         : Objects
;; *.d                : Dependencies
;; *.a                : Librairies
;; *.ref, *.new, TAGS : Emacs files
;; *.out              : Executable
;; *.map              : Mapping
(setq grep-find-file-filter "! -path \"*/.svn/*\" ! -name \"*.[oa]\" ! -name \"*.dbg\" ! -name \"*.d\" ! -name \"\.*\" ! -name \"*.ref\" ! -name \"*.new\" ! -name \"*.out\" ! -name \"*.map\" ! -name \"TAGS\" ! -name \".emacs.desktop\"")


;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Get string to search (either selected text, or word at cursor position)
;;
;; return : string to search
;; ----------------------------------------------------------------------------
(defun eide-search-internal-get-string-to-search ()
  (if (and (not (featurep 'xemacs)) (eq mark-active t))
    ;; Text is selected
    (if (= (count-screen-lines (region-beginning) (region-end) t) 1)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (progn
        (message "Text is selected over several lines : cannot search for it...")
        nil))
    ;; No text is selected
    (progn
      (setq this-string (find-tag-default)) ; (cscope-extract-symbol-at-cursor nil)
      (if this-string
        this-string
        (progn
          (message "No text to search at cursor position...")
          nil)))))


;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------------
(defun eide-search-back-from-tag ()
  (interactive)
  (eide-windows-select-window-file nil)
  (call-interactively 'pop-tag-mark)
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Go to definition of an element (parameter)
;;
;; input  : this-string
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag (this-string)
  (interactive)
  (eide-windows-select-window-file nil)
  (find-tag this-string)
  (recenter))

;; ----------------------------------------------------------------------------
;; Go to definition of element at cursor position
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag-without-prompt ()
  (interactive)
  (setq eide-search-tag-string (eide-search-internal-get-string-to-search))
  (if eide-search-tag-string
    (eide-search-find-tag eide-search-tag-string)))

;; ----------------------------------------------------------------------------
;; Go to definition of an element (prompt for it)
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag-with-prompt ()
  (interactive)
  (eide-windows-select-window-file nil)
  (call-interactively 'find-tag)
  ;; Saving string is necessary for calling eide-search-find-alternate-tag
  ;; later on... but there is no completion !
  ;(setq eide-search-tag-string (read-string "Go to symbol definition : "))
  ;(if (string-equal eide-search-tag-string "")
  ;  (message "Cannot find empty symbol...")
  ;  (eide-search-find-tag eide-search-tag-string))
  (recenter))

;; ----------------------------------------------------------------------------
;; Go to alternate definition of previously searched element
;; ----------------------------------------------------------------------------
(defun eide-search-find-alternate-tag ()
  (interactive)
  (eide-windows-select-window-file nil)
  (call-interactively 'pop-tag-mark)
  (find-tag eide-search-tag-string t)
  (recenter))

;; ----------------------------------------------------------------------------
;; Back from symbol definition
;; ----------------------------------------------------------------------------
(defun eide-search-back-from-symbol-definition ()
  (interactive)
  (eide-windows-select-window-file nil)
  (cscope-pop-mark)
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Go to symbol definition (at cursor position)
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-definition-without-prompt ()
  (interactive)
  (setq eide-menu-symbol-definition-to-find (find-tag-default))
  (cscope-find-global-definition-no-prompting)
  ;; Update menu because a new file may have been opened
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Go to symbol definition (prompt for it)
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-definition-with-prompt ()
  (interactive)
  (setq eide-search-cscope-string (find-tag-default))
  (eide-windows-select-window-file nil)
  ;; TODO : remplacer find-tag par la bonne commande cscope
  (call-interactively 'find-tag eide-search-cscope-string)
  (recenter))

;; ----------------------------------------------------------------------------
;; Find a symbol in the whole project
;; Result is displayed in a new buffer named "*cscope* : " symbol
;;
;; input  : this-symbol : symbol to find
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol (this-symbol)
  (eide-windows-select-window-results t)
  (setq my-active-grep-buffer-name (concat "*cscope* : " this-symbol))
  (setq do-grep t)
  (if (get-buffer my-active-grep-buffer-name)
    (if (eide-popup-question-yes-or-no-p "This symbol has already been found... Find again (or use available result) ?")
      ;; Delete existing find-symbol buffer
      (kill-buffer my-active-grep-buffer-name)
      (setq do-grep nil)))
  (if do-grep
    (progn
      (cscope-find-this-symbol this-symbol)
      (eide-toolbar-update)
      (save-excursion
        (set-buffer "*cscope*")
        (rename-buffer my-active-grep-buffer-name t))
      (eide-files-build-lists))
;        (eide-menu-update nil)))
    (eide-search-view-result-buffer my-active-grep-buffer-name))
  (eide-windows-select-window-file t))

;; ----------------------------------------------------------------------------
;; Find symbol (prompt for it), in the whole project
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-with-prompt ()
  (interactive)
  (setq eide-search-cscope-string (read-string "Find symbol (in whole project) : "))
  (if (string-equal eide-search-cscope-string "")
    (message "Cannot find empty symbol...")
    (eide-search-find-symbol eide-search-cscope-string)))

;; ----------------------------------------------------------------------------
;; Find symbol at cursor position, in the whole project
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-without-prompt ()
  (interactive)
  (setq eide-search-cscope-string (eide-search-internal-get-string-to-search))
  (if eide-search-cscope-string
    (eide-search-find-symbol eide-search-cscope-string)))

;; ----------------------------------------------------------------------------
;; Grep a string in current directory
;; Result is displayed in a new buffer named "*grep* : " string
;;
;; input  : this-string : string to grep
;; ----------------------------------------------------------------------------
(defun eide-search-grep (this-string)
;    (progn
      ;; set current buffer in window "results" so that grep can apply to its directory
  (eide-windows-select-window-file t)
;      (setq my-buffer (buffer-name))
;      (eide-windows-select-window-results t)
;      (set-buffer my-buffer))
  ;; Get current file directory, because shell init file may change current
  ;; directory before grep is executed
  (setq my-buffer-directory (file-name-directory (buffer-file-name)))
;  (eide-windows-select-window-results t)

  (setq my-active-grep-buffer-name (concat "*grep* : " this-string "    (in " (eide-project-get-short-directory default-directory) ")"))

  (setq do-grep t)
  (if (get-buffer my-active-grep-buffer-name)
    (if (eide-popup-question-yes-or-no-p "This string has already been searched... Search again (or use available search result) ?")
      ;; Delete existing grep buffer
      (kill-buffer my-active-grep-buffer-name)
      (setq do-grep nil)))
  (if do-grep
    (progn
;      (grep (concat "grep -d skip -n -e \"" this-string "\" " grep-file-filter))
      (if (eq system-type 'windows-nt)
        (grep-find (concat "echo ; cd " my-buffer-directory " ; find . -maxdepth 1 -type f " grep-find-file-filter " -exec grep -Ine \"" this-string "\" {} NUL \\;"))
        (grep-find (concat "echo ; cd " my-buffer-directory " ; find . -maxdepth 1 -type f " grep-find-file-filter " -exec grep -IHne \"" this-string "\" {} \\;")))
      (eide-toolbar-update)
      (save-excursion
        (set-buffer "*grep*")
        (rename-buffer my-active-grep-buffer-name t))
      (eide-files-build-lists))
;        (eide-menu-update nil)))
    (eide-search-view-result-buffer my-active-grep-buffer-name))
  (eide-windows-select-window-file t))

;; ----------------------------------------------------------------------------
;; Grep element at cursor position
;; ----------------------------------------------------------------------------
(defun eide-search-grep-without-prompt ()
  (interactive)
  (setq eide-search-grep-string (eide-search-internal-get-string-to-search))
  (if eide-search-grep-string
    (eide-search-grep eide-search-grep-string)))

;; ----------------------------------------------------------------------------
;; Grep a string (prompt for it)
;; ----------------------------------------------------------------------------
(defun eide-search-grep-with-prompt ()
  (interactive)
  (setq eide-search-grep-string (read-string "Grep (in current directory) : "))
  (if (string-equal eide-search-grep-string "")
    (message "Cannot grep empty string...")
    (eide-search-grep eide-search-grep-string)))

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
;; Grep a string in the whole project
;; Result is displayed in a new buffer named "*grep-find* : " string
;;
;; input  : this-string : string to grep
;; ----------------------------------------------------------------------------
(defun eide-search-grep-find (this-string)
  (eide-windows-select-window-results t)
  (setq my-active-grep-buffer-name (concat "*grep-find* : " this-string))

  (setq do-grep t)
  (if (get-buffer my-active-grep-buffer-name)
    (if (eide-popup-question-yes-or-no-p "This string has already been searched... Search again (or use available search result) ?")
      ;; Delete existing grep buffer
      (kill-buffer my-active-grep-buffer-name)
      (setq do-grep nil)))
  (if do-grep
    (progn
      (if eide-option-search-grep-find-on-2-lines-flag
;        (grep-find (concat "find " eide-project-directory " -type f -name \"" grep-file-filter "\" -exec grep -ne \"" this-string "\" {} NUL \\;; sed 's/[0-9]:/\&\\\n/'"))
        (grep-find (concat "find " eide-project-directory " -type f " grep-find-file-filter " -exec grep -Ine \"" this-string "\" {} NUL \\;; grep -v \"^Binary\" | sed 's/[0-9]:/\&\\\n/'"))
        (if (eq system-type 'windows-nt)
          (grep-find (concat "echo ; find " eide-project-directory " -type f " grep-find-file-filter " -exec grep -Ine \"" this-string "\" {} NUL \\;"))
          (grep-find (concat "echo ; find " eide-project-directory " -type f " grep-find-file-filter " -exec grep -IHne \"" this-string "\" {} \\;"))))
      (eide-toolbar-update)
      (save-excursion
        (set-buffer "*grep*")
        (rename-buffer my-active-grep-buffer-name t))
      (eide-files-build-lists))
;        (eide-menu-update nil)))
    (eide-search-view-result-buffer my-active-grep-buffer-name))
  (eide-windows-select-window-file t))

;; ----------------------------------------------------------------------------
;; Grep element at cursor position, in the whole project
;; ----------------------------------------------------------------------------
(defun eide-search-grep-find-without-prompt ()
  (interactive)
  (setq eide-search-grep-string (eide-search-internal-get-string-to-search))
  (if eide-search-grep-string
    (eide-search-grep-find eide-search-grep-string)))

;; ----------------------------------------------------------------------------
;; Grep an element (prompt for it), in the whole project
;; ----------------------------------------------------------------------------
(defun eide-search-grep-find-with-prompt ()
  (interactive)
  (setq eide-search-grep-string (read-string "Grep (in whole project) : "))
  (if (string-equal eide-search-grep-string "")
    (message "Cannot grep empty string...")
    (eide-search-grep-find eide-search-grep-string)))

;; ----------------------------------------------------------------------------
;; Go to previous grep match (or compile error)
;; ----------------------------------------------------------------------------
(defun eide-search-grep-go-to-previous ()
  (interactive)
  ;; Move to window "file" to make sure that the buffer won't be displayed
  ;; in window "menu"
;  (eide-windows-select-window-file nil)
  (previous-error)
  (if (not eide-windows-is-layout-visible-flag)
    ;; Close grep window (appears automatically with previous-error)
    (delete-other-windows))
  (recenter)
  ;; Update menu because a new file may have been opened
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Go to next grep match (or compile error)
;; ----------------------------------------------------------------------------
(defun eide-search-grep-go-to-next ()
  (interactive)
  ;; Move to window "file" to make sure that the buffer won't be displayed
  ;; in window "menu"
;  (eide-windows-select-window-file nil)
  (next-error)
  (if (not eide-windows-is-layout-visible-flag)
    ;; Close grep window (appears automatically with next-error)
    (delete-other-windows))
  (recenter)
  ;; Update menu because a new file may have been opened
  (eide-menu-update nil)
  (eide-windows-select-window-file nil))

;; ----------------------------------------------------------------------------
;; Display a grep result buffer
;; ----------------------------------------------------------------------------
(defun eide-search-view-result-buffer (this-grep-buffer-name)
  (eide-windows-select-window-results t)
  ;(eide-debug-print-trace (concat "view grep : " this-grep-buffer-name))
  (switch-to-buffer this-grep-buffer-name))

;; ----------------------------------------------------------------------------
;; Close a grep result buffer
;;
;; input  :   this-grep-buffer-name : grep result buffer name
;; ----------------------------------------------------------------------------
(defun eide-search-close-grep-buffer (this-grep-buffer-name)
  (eide-windows-select-window-results t)
  (setq this-buffer (buffer-name))
  (kill-buffer this-grep-buffer-name)
  ;; TODO : "delete" plutôt que "setq ... remove" ne fonctionne pas !...
  (setq eide-menu-grep-result-list (remove this-grep-buffer-name eide-menu-grep-result-list))

  (if (string-equal this-grep-buffer-name this-buffer)
    ;; It was current result buffer : display another one
    (progn
      (setq this-buffer (car eide-menu-grep-result-list))
      (if this-buffer
        (switch-to-buffer this-buffer)
        (switch-to-buffer "*results*")))))

;; ----------------------------------------------------------------------------
;; Close a cscope result buffer
;;
;; input  :   this-cscope-buffer-name : cscope result buffer name
;; ----------------------------------------------------------------------------
(defun eide-search-close-cscope-buffer (this-cscope-buffer-name)
  (eide-windows-select-window-results t)
  (setq this-buffer (buffer-name))
  (kill-buffer this-cscope-buffer-name)
  ;; TODO : "delete" plutôt que "setq ... remove" ne fonctionne pas !...
  (setq eide-menu-cscope-result-list (remove this-cscope-buffer-name eide-menu-cscope-result-list))

  (if (string-equal this-cscope-buffer-name this-buffer)
    ;; It was current result buffer : display another one
    (progn
      (setq this-buffer (car eide-menu-cscope-result-list))
      (if this-buffer
        (switch-to-buffer this-buffer)
        (switch-to-buffer "*results*")))))

;;; eide-search.el ends here
