;;; eide-search.el --- Emacs-IDE, search

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

(provide 'eide-search)

(require 'etags)

(defvar eide-search-find-symbol-definition-flag nil)

;; grep commands should exclude following files:
;; .svn: Subversion directory
;; *.d: Dependency files
;; *.o.cmd: Kbuild files
;; *.map: Mapping files
;; *.ref, *.new: Emacs-IDE files
;; .emacs.desktop: Emacs files
;; TAGS: Ctags file
;; cscope.files, cscope.output: Cscope files
(defvar eide-search-grep-exclude-options "--devices=skip --exclude-dir=.svn --exclude=*.d --exclude=*.o.cmd --exclude=*.map --exclude=*.ref --exclude=*.new --exclude=.emacs.desktop --exclude=TAGS --exclude=cscope.files --exclude=cscope.out")

(defvar eide-search-tag-string nil)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Get string to search (either selected text, or word at cursor position).
;;
;; return : string to search.
;; ----------------------------------------------------------------------------
(defun eide-i-search-get-string-to-search ()
  (if (eq mark-active t)
    ;; Text is selected
    (if (= (count-screen-lines (region-beginning) (region-end) t) 1)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (progn
        (message "Text is selected over several lines: cannot search for it...")
        nil))
    ;; No text is selected
    (let ((l-string (find-tag-default))) ; (cscope-extract-symbol-at-cursor nil)
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
  (eide-windows-select-source-window nil)
  (call-interactively 'pop-tag-mark)
  (eide-menu-update nil))

;; ----------------------------------------------------------------------------
;; Go to definition of a symbol.
;;
;; input  : p-string : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag (p-string)
  (eide-windows-select-source-window nil)
  (find-tag p-string)
  (recenter))

;; ----------------------------------------------------------------------------
;; Go to definition of symbol at cursor position.
;;
;; output : eide-search-tag-string : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag-without-prompt ()
  (interactive)
  (setq eide-search-tag-string (eide-i-search-get-string-to-search))
  (if eide-search-tag-string
    (eide-search-find-tag eide-search-tag-string)))

;; ----------------------------------------------------------------------------
;; Go to definition of a symbol (prompt for it).
;; ----------------------------------------------------------------------------
(defun eide-search-find-tag-with-prompt ()
  (interactive)
  (eide-windows-select-source-window nil)
  (call-interactively 'find-tag)
  ;; Saving string is necessary for calling eide-search-find-alternate-tag
  ;; later on... but there is no completion !
  ;;(setq eide-search-tag-string (read-string "Go to symbol definition: "))
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
  (eide-windows-select-source-window nil)
  (call-interactively 'pop-tag-mark)
  (find-tag eide-search-tag-string t)
  (recenter))

;; ----------------------------------------------------------------------------
;; Go back from definition.
;; ----------------------------------------------------------------------------
(defun eide-search-back-from-symbol-definition ()
  (interactive)
  (eide-windows-select-source-window nil)
  (cscope-pop-mark)
  (eide-menu-update nil))

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
  (eide-windows-select-source-window nil))

;; ----------------------------------------------------------------------------
;; Go to definition of a symbol (prompt for it).
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-definition-with-prompt ()
  (interactive)
  (let ((l-string (find-tag-default)))
    (eide-windows-select-source-window nil)
    ;; TODO: remplacer find-tag par la bonne commande cscope
    (call-interactively 'find-tag l-string))
  (recenter))

;; ----------------------------------------------------------------------------
;; Find a symbol.
;;
;; input  : p-symbol : symbol.
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol (p-symbol)
  (eide-windows-select-output-window)
  (let ((l-result-buffer-name (concat "*cscope*: " p-symbol))
        (l-do-it-flag t))
    (if (get-buffer l-result-buffer-name)
      (if (eide-popup-question-yes-or-no-p "This symbol has already been found... Find again (or use available result)?")
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
      (eide-search-view-result-buffer l-result-buffer-name))
    (eide-windows-select-source-window t)))

;; ----------------------------------------------------------------------------
;; Find a symbol (prompt for it).
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-with-prompt ()
  (interactive)
  (if eide-project-cscope-files-flag
    (let ((l-string (read-string "Find symbol with cscope: ")))
      (if (string-equal l-string "")
        (message "Cannot find empty symbol...")
        (eide-search-find-symbol l-string)))
    (message "Cannot use cscope: there is no C/C++ file in this project...")))

;; ----------------------------------------------------------------------------
;; Find symbol at cursor position.
;; ----------------------------------------------------------------------------
(defun eide-search-find-symbol-without-prompt ()
  (interactive)
  (if eide-project-cscope-files-flag
    (let ((l-string (eide-i-search-get-string-to-search)))
      (if l-string
        (eide-search-find-symbol l-string)))
    (message "Cannot use cscope: there is no C/C++ file in this project...")))

;; ----------------------------------------------------------------------------
;; Grep a string in current directory.
;;
;; input  : p-string : string.
;; ----------------------------------------------------------------------------
(defun eide-search-grep-local (p-string)
  (eide-windows-select-source-window t)
  (let ((l-buffer-directory (file-name-directory (buffer-file-name)))
        (l-result-buffer-name (concat "*grep (local)*: " p-string "    (in " (eide-project-get-short-directory default-directory) ")"))
        (l-do-it-flag t))
    (if (get-buffer l-result-buffer-name)
      (if (eide-popup-question-yes-or-no-p "This string has already been searched... Search again (or use available search result)?")
        ;; Delete existing grep buffer
        (kill-buffer l-result-buffer-name)
        (setq l-do-it-flag nil)))
    (if l-do-it-flag
      (progn
        ;; grep options: I (no binary), n (show line number), e (pattern may start with '-')
        ;; 2> /dev/null is used to hide warnings about missing files
        ;; 'cd' is used first, in case shell init changes current directory
        (grep-find (concat "echo ; cd " l-buffer-directory " ; grep -In " eide-search-grep-exclude-options " -e \"" p-string "\" * .* 2> /dev/null"))
        (save-excursion
          (set-buffer "*grep*")
          (rename-buffer l-result-buffer-name t))
        (eide-menu-build-files-lists))
      (eide-search-view-result-buffer l-result-buffer-name))
    (eide-windows-select-source-window t)))

;; ----------------------------------------------------------------------------
;; Grep word at cursor position, in current directory.
;; ----------------------------------------------------------------------------
(defun eide-search-grep-local-without-prompt ()
  (interactive)
  (let ((l-string (eide-i-search-get-string-to-search)))
    (if l-string
      (eide-search-grep-local l-string))))

;; ----------------------------------------------------------------------------
;; Grep a string in current directory (prompt for it).
;; ----------------------------------------------------------------------------
(defun eide-search-grep-local-with-prompt ()
  (interactive)
  (let ((l-string (read-string "Grep (in current directory): ")))
    (if (string-equal l-string "")
      (message "Cannot grep empty string...")
      (eide-search-grep-local l-string))))

;; ----------------------------------------------------------------------------
;; Grep a string in the whole project.
;;
;; input  : p-string : string.
;;          eide-root-directory : project root directory.
;; ----------------------------------------------------------------------------
(defun eide-search-grep-global (p-string)
  ;; On Emacs 22 GTK: it is necessary to select "source" window, otherwise
  ;; current output buffer will be reused if "output" window is selected.
  (eide-windows-select-source-window t)
  (let ((l-result-buffer-name (concat "*grep (global)*: " p-string))
        (l-do-it-flag t))
    (if (get-buffer l-result-buffer-name)
      (if (eide-popup-question-yes-or-no-p "This string has already been searched... Search again (or use available search result)?")
        ;; Delete existing grep buffer
        (kill-buffer l-result-buffer-name)
        (setq l-do-it-flag nil)))
    (if l-do-it-flag
      (progn
        ;; Temporarily change current directory, so that grep results are relative to root directory
        (let ((default-directory eide-root-directory))
          ;; grep options: r (recursive), I (no binary), n (show line number), e (pattern may start with '-')
          ;; 2> /dev/null is used to hide warnings about missing files
          ;; 'cd' is used first, in case shell init changes current directory
          (grep-find (concat "echo ; cd " eide-root-directory " ; grep -rIn " eide-search-grep-exclude-options " -e \"" p-string "\" . 2> /dev/null")))
        (save-excursion
          (set-buffer "*grep*")
          (rename-buffer l-result-buffer-name t))
        (eide-menu-build-files-lists))
      (eide-search-view-result-buffer l-result-buffer-name))
    (eide-windows-select-source-window t)))

;; ----------------------------------------------------------------------------
;; Grep word at cursor position, in the whole project.
;; ----------------------------------------------------------------------------
(defun eide-search-grep-global-without-prompt ()
  (interactive)
  (let ((l-string (eide-i-search-get-string-to-search)))
    (if l-string
      (eide-search-grep-global l-string))))

;; ----------------------------------------------------------------------------
;; Grep a string in the whole project (prompt for it).
;; ----------------------------------------------------------------------------
(defun eide-search-grep-global-with-prompt ()
  (interactive)
  (let ((l-string (read-string "Grep (in whole project): ")))
    (if (string-equal l-string "")
      (message "Cannot grep empty string...")
      (eide-search-grep-global l-string))))

;; ----------------------------------------------------------------------------
;; Go to previous grep match (or compile error).
;; ----------------------------------------------------------------------------
(defun eide-search-grep-go-to-previous ()
  (interactive)
  (previous-error)
  (if (not eide-windows-is-layout-visible-flag)
    ;; Close grep window (appears automatically with previous-error)
    (delete-other-windows))
  (recenter)
  ;; Update menu because a new file may have been opened
  (eide-menu-update nil)
  (eide-windows-select-source-window nil))

;; ----------------------------------------------------------------------------
;; Go to next grep match (or compile error).
;; ----------------------------------------------------------------------------
(defun eide-search-grep-go-to-next ()
  (interactive)
  (next-error)
  (if (not eide-windows-is-layout-visible-flag)
    ;; Close grep window (appears automatically with next-error)
    (delete-other-windows))
  (recenter)
  ;; Update menu because a new file may have been opened
  (eide-menu-update nil)
  (eide-windows-select-source-window nil))

;; ----------------------------------------------------------------------------
;; Display a result buffer.
;; ----------------------------------------------------------------------------
(defun eide-search-view-result-buffer (p-result-buffer-name)
  (eide-windows-select-output-window)
  (switch-to-buffer p-result-buffer-name))

;; ----------------------------------------------------------------------------
;; Close a grep result buffer.
;;
;; input  : p-grep-buffer-name : grep result buffer name.
;;          eide-menu-grep-results-list : grep results list.
;; output : eide-menu-grep-results-list : updated grep results list.
;; ----------------------------------------------------------------------------
(defun eide-search-close-grep-buffer (p-grep-buffer-name)
  (eide-windows-select-output-window)
  (let ((l-buffer (buffer-name)))
    (kill-buffer p-grep-buffer-name)
    (setq eide-menu-grep-results-list (remove p-grep-buffer-name eide-menu-grep-results-list))

    (if (string-equal p-grep-buffer-name l-buffer)
      ;; It was current result buffer: display another one
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
  (eide-windows-select-output-window)
  (let ((l-buffer (buffer-name)))
    (dolist (l-grep-buffer-name eide-menu-grep-results-list)
      (kill-buffer l-grep-buffer-name))
    (setq eide-menu-grep-results-list nil)

    (if (not (string-equal (buffer-name) l-buffer))
      ;; It was a grep result buffer: display another search buffer
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
  (eide-windows-select-output-window)
  (let ((l-buffer (buffer-name)))
    (kill-buffer p-cscope-buffer-name)
    (setq eide-menu-cscope-results-list (remove p-cscope-buffer-name eide-menu-cscope-results-list))

    (if (string-equal p-cscope-buffer-name l-buffer)
      ;; It was current result buffer: display another one
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
  (eide-windows-select-output-window)
  (let ((l-buffer (buffer-name)))
    (dolist (l-cscope-buffer-name eide-menu-cscope-results-list)
      (kill-buffer l-cscope-buffer-name))
    (setq eide-menu-cscope-results-list nil)

    (if (not (string-equal (buffer-name) l-buffer))
      ;; It was a grep result buffer: display another search buffer
      (progn
        (setq l-buffer (car eide-menu-grep-results-list))
        (if l-buffer
          (switch-to-buffer l-buffer)
          (switch-to-buffer "*results*"))))))

;;; eide-search.el ends here
