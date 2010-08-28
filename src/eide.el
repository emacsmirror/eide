;;; eide.el --- Emacs-IDE

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

(if (featurep 'xemacs)
  (progn
    (read-string "Sorry, XEmacs is not supported by Emacs-IDE, press <ENTER> to exit...")
    (kill-emacs)))

;;;; ==========================================================================
;;;; SETTINGS
;;;; ==========================================================================

(defvar eide-version "1.4+")
(defvar eide-release-date "2010-08")

(defvar eide-root-directory nil)
(defvar eide-current-buffer nil)

;; Directory for including other Emacs-IDE modules (src/*.el)
;; file-truename follows symbolic link .emacs (if any)
;; file-name-directory retrieves directory path (removes file name)
(defvar eide-emacs-src-path (file-name-directory (file-truename user-init-file)))
(add-to-list 'load-path eide-emacs-src-path)

;; Emacs-IDE root directory
;; directory-file-name removes final "/"
;; file-name-directory retrieves directory path (removes file name)
(defvar eide-emacs-root-path (file-name-directory (directory-file-name eide-emacs-src-path)))
(defvar eide-under-svn-flag nil)

(if (file-exists-p (concat eide-emacs-root-path ".svn"))
  (setq eide-under-svn-flag t))

;; Project directory
;; On Windows: it is necessary to open a temporary file for the directory path
;; to be correct (Windows standard vs Unix)
;; On Linux: it is also useful to expand path (~ => /home/xxx/).
;; NB: "temp" was first used as a temporary filename, but it causes the project
;; directory to be changed to "temp" if "temp" already exists and is a
;; directory !... Hence a filename that can not exist !! :-)

(let ((l-temp-file "this-is-a-temporary-file-for-emacs-ide"))
  (find-file l-temp-file)
  (setq eide-root-directory default-directory)
  (kill-buffer l-temp-file))

;;;; ==========================================================================
;;;; EMACS MODULES
;;;; ==========================================================================

(require 'desktop)
(require 'hideshow)
(require 'imenu)
(require 'mwheel)
(require 'ediff)

;;;; ==========================================================================
;;;; EMACS-IDE MODULES
;;;; ==========================================================================

(require 'eide-compare)
(require 'eide-config)
(require 'eide-edit)
(require 'eide-help)
(require 'eide-keys)
(require 'eide-menu)
(require 'eide-popup)
(require 'eide-project)
(require 'eide-search)
(require 'eide-svn)
(require 'eide-windows)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Open shell.
;;
;; output : eide-windows-update-result-buffer-id : "s" for "shell".
;; ----------------------------------------------------------------------------
(defun eide-shell-open ()
  (interactive)
  ;; Force to open a new shell (in current directory)
  (if eide-shell-buffer
    (kill-buffer eide-shell-buffer))
  (eide-windows-select-window-file t)
  ;; Shell buffer name will be updated in eide-i-windows-display-buffer-function
  (setq eide-windows-update-result-buffer-id "s")
  (shell))

;; ----------------------------------------------------------------------------
;; If Emacs-IDE is under svn, check if an update is available.
;;
;; input  : eide-under-svn-flag : t if Emacs-IDE is under svn.
;;          eide-emacs-root-path : path of Emacs-IDE.
;; ----------------------------------------------------------------------------
(defun eide-check-update-from-svn ()
  (if eide-under-svn-flag
    (progn
      (message "Checking update (svn)...")
      (if (string-equal (shell-command-to-string (concat "cd " eide-emacs-root-path " && src/svn-check.sh")) "yes\n")
        (if (eide-popup-question-yes-or-no-p "A new version of Emacs-IDE is available from svn.\nDo you want to update ?")
          (progn
            (shell-command (concat "cd " eide-emacs-root-path " && src/svn-update.sh"))
            (eide-popup-message "Emacs-IDE has been updated.\nThis session will be closed and opened again.")
            (if eide-project-name
              ;; Save desktop and disable auto-saving, otherwise emacs will
              ;; display a warning message about desktop being opened by several
              ;; sessions
              (progn
                (desktop-save eide-root-directory)
                (desktop-save-mode -1)))
            (shell-command (concat "cd " eide-root-directory " && emacs &"))
            (kill-emacs))
          (eide-popup-message "Emacs-IDE will not be updated now."))
        (message "Checking update (svn)... No update available.")))))

;;;; ==========================================================================
;;;; EMACS SETTINGS
;;;; ==========================================================================

;; Do not display startup message
(setq inhibit-startup-message t)
;; Disable warning for large files (especially for TAGS)
(setq large-file-warning-threshold nil)
;; Do not save backup files (~)
(setq make-backup-files nil)
;; Do not save place in .emacs-places
(setq-default save-place nil)
;; No confirmation when refreshing buffer
(setq revert-without-query '(".*"))
;; Use 'y' and 'n' instead of 'yes' and 'no' for minibuffer questions
(fset 'yes-or-no-p 'y-or-n-p)
;; Use mouse wheel (default for Windows but not for Linux)
(mouse-wheel-mode 1)
;; Mouse wheel should scroll the window over which the mouse is
(setq mouse-wheel-follow-mouse t)
;; Set mouse wheel scrolling speed
(if (equal (safe-length mouse-wheel-scroll-amount) 1)
  ;; Old API
  (setq mouse-wheel-scroll-amount '(4 . 1))
  ;; New API
  (setq mouse-wheel-scroll-amount '(4 ((shift) . 1) ((control)))))
;; Disable mouse wheel progressive speed
(setq mouse-wheel-progressive-speed nil)
;; Keep cursor position when moving page up/down
(setq scroll-preserve-screen-position t)
;; Paper type for printing
(setq ps-paper-type 'a4)
;; Show end of buffer
;;(setq-default indicate-empty-lines t)
;; No menu bar
(if window-system
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)))
;; "One line at a time" scrolling
(setq-default scroll-conservatively 1)
;; Four line margin for scrolling
(setq scroll-margin 4)
;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
;; Disable beep
;;(setq visible-bell t)
(setq ring-bell-function (lambda() ()))
;; Vertical scroll bar on the right
;; (default value: right for Windows, left for Linux)
(set-scroll-bar-mode 'right)
;; Ignore invisible lines when moving cursor in project configuration
;; TODO: not used anymore in project configuration => still necessary ?
(setq line-move-ignore-invisible t)
;; Display current function (relating to cursor position) in info line
;; (if possible with current major mode)
(which-function-mode)
;; "Warning" color highlight when possible error is detected
;;(global-cwarn-mode)
;; Do not prompt for updating tag file if necessary
(setq tags-revert-without-query t)
;; Augmenter le nombre de fonctions dans le menu pop up "liste des fonctions"
;; (sinon, elles sont parfois inutilement regroupées dans des sous-menus)
;; (default: 25)
;; no longer used (personal popup menu)
;;(setq imenu-max-items 40)
;; Augmenter le nombre de buffers dans le menu pop up "liste des buffers"
;; (sinon, elles sont parfois inutilement regroupées dans des sous-menus)
;; (default: 20)
;; no longer used (personal popup menu)
;;(setq mouse-buffer-menu-maxlen 40)
;; Highlight matching parentheses (when cursor on "(" or just after ")")
(show-paren-mode 1)
;; Frame size and position
(if window-system
  (if (eq system-type 'windows-nt)
    ;; Windows
    (setq initial-frame-alist '((top . 0) (left . 0) (width . 122) (height . 39)))
    ;; Linux
    (setq initial-frame-alist '((top . 30) (left . 0) (width . 120) (height . 48)))))

;;(make-frame '((fullscreen . fullboth)))
;;(modify-frame-parameters nil '((fullscreen . nil)))
;;(modify-frame-parameters nil '((fullscreen . fullboth)))
;;(set-frame-parameter nil 'fullscreen 'fullboth)
(if eide-option-use-cscope-flag
  (progn
    (cscope-set-initial-directory eide-root-directory)
    ;;(setq cscope-do-not-update-database t)
    ))
;; ediff: Highlight current diff only
;;(setq ediff-highlight-all-diffs nil)
;; ediff: Control panel in the same frame
(if window-system
  (ediff-toggle-multiframe))
;; ediff: Split horizontally for buffer comparison
(setq ediff-split-window-function 'split-window-horizontally)

;; gdb: Use graphical interface
(setq gdb-many-windows t)

;;;; ==========================================================================
;;;; IMENU (LIST OF FUNCTIONS)
;;;; ==========================================================================

;; Construction de la liste des fonctions (imenu)
;; Utilisation d'expressions régulières
;; (il faut pour cela laisser imenu-extract-index-name-function = nil)
;; Il faut redéfinir les expressions, car les expressions par défaut amènent
;; beaucoup d'erreurs : du code est parfois interprété à tort comme une
;; définition de fonction)

(setq eide-regex-word              "[a-zA-Z_][a-zA-Z0-9_:<>~]*")
(setq eide-regex-word-no-underscore "[a-zA-Z][a-zA-Z0-9_:<>~]*")
(setq eide-regex-space "[ \t]+")
(setq eide-regex-space-or-crlf "[ \t\n\r]+")
(setq eide-regex-space-or-crlf-or-nothing "[ \t\n\r]*")
(setq eide-regex-space-or-crlf-or-comment-or-nothing "[ \t\n\r]*\\(//\\)*[^\n\r]*[ \t\n\r]*")
;;(setq eide-regex-space-or-crlf-or-comment-or-nothing "[ \t\n\r]*\\(//\\)*[^\n\r]*[\n\r][ \t\n\r]*")
(setq eide-regex-space-or-nothing "[ \t]*")

(setq eide-cc-imenu-c-macro
      (concat
       "^#define" eide-regex-space
       "\\(" eide-regex-word "\\)("
       )
      )

(setq eide-cc-imenu-c-struct
      (concat
       "^typedef"  eide-regex-space "struct" eide-regex-space-or-crlf-or-nothing
       "{[^{]+}" eide-regex-space-or-nothing
       "\\(" eide-regex-word "\\)" ))

(setq eide-cc-imenu-c-enum
      (concat
       "^typedef" eide-regex-space "enum" eide-regex-space-or-crlf-or-nothing
       "{[^{]+}" eide-regex-space-or-nothing
       "\\(" eide-regex-word "\\)" ))

(setq eide-cc-imenu-c-define
      (concat
       "^#define" eide-regex-space
       "\\(" eide-regex-word "\\)" eide-regex-space ))

(setq eide-cc-imenu-c-function
      (concat
       "^\\(?:" eide-regex-word-no-underscore "\\*?" eide-regex-space "\\)*" ; void* my_function(void)
       "\\*?" ; function may return a pointer, e.g. void *my_function(void)
       "\\(" eide-regex-word "\\)"
       eide-regex-space-or-crlf-or-nothing "("
       eide-regex-space-or-crlf-or-nothing "\\([^ \t(*][^)]*\\)?)" ; the arg list must not start
       ;;"[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
       eide-regex-space-or-crlf-or-comment-or-nothing "{" ))

(if nil
  (progn
    ;; temp: remplace la définition au-dessus
    (setq eide-cc-imenu-c-function
          (concat
           "^\\(?:" eide-regex-word eide-regex-space "\\)*"
           "\\(" eide-regex-word "\\)"
           eide-regex-space-or-nothing "("
           "\\(" eide-regex-space-or-crlf-or-nothing eide-regex-word "\\)*)"
           ;;eide-regex-space-or-nothing "\\([^ \t(*][^)]*\\)?)"   ; the arg list must not start
           ;;"[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
           eide-regex-space-or-crlf-or-nothing "{" ))
    ))

;;cc-imenu-c-generic-expression's value is
;;((nil "^\\<.*[^a-zA-Z0-9_:<>~]\\(\\([a-zA-Z0-9_:<>~]*::\\)?operator\\>[   ]*\\(()\\|[^(]*\\)\\)[  ]*([^)]*)[  ]*[^  ;]" 1)
;; (nil "^\\([a-zA-Z_][a-zA-Z0-9_:<>~]*\\)[   ]*([  ]*\\([^   (*][^)]*\\)?)[  ]*[^  ;(]" 1)
;; (nil "^\\<[^()]*[^a-zA-Z0-9_:<>~]\\([a-zA-Z_][a-zA-Z0-9_:<>~]*\\)[   ]*([  ]*\\([^   (*][^)]*\\)?)[  ]*[^  ;(]" 1)
;; ("Class" "^\\(template[  ]*<[^>]+>[  ]*\\)?\\(class\\|struct\\)[   ]+\\([a-zA-Z0-9_]+\\(<[^>]+>\\)?\\)[  \n]*[:{]" 3))
;;cc-imenu-c++-generic-expression's value is
;;((nil "^\\<.*[^a-zA-Z0-9_:<>~]\\(\\([a-zA-Z0-9_:<>~]*::\\)?operator\\>[   ]*\\(()\\|[^(]*\\)\\)[  ]*([^)]*)[  ]*[^  ;]" 1)
;; (nil "^\\([a-zA-Z_][a-zA-Z0-9_:<>~]*\\)[   ]*([  ]*\\([^   (*][^)]*\\)?)[  ]*[^  ;(]" 1)
;; (nil "^\\<[^()]*[^a-zA-Z0-9_:<>~]\\([a-zA-Z_][a-zA-Z0-9_:<>~]*\\)[   ]*([  ]*\\([^   (*][^)]*\\)?)[  ]*[^  ;(]" 1)
;; ("Class" "^\\(template[  ]*<[^>]+>[  ]*\\)?\\(class\\|struct\\)[   ]+\\([a-zA-Z0-9_]+\\(<[^>]+>\\)?\\)[  \n]*[:{]" 3))

(setq eide-cc-imenu-c-interrupt
      (concat
       "\\(__interrupt"  eide-regex-space
       "\\(" eide-regex-word eide-regex-space "\\)*"
       eide-regex-word "\\)"
       eide-regex-space-or-nothing "("
       eide-regex-space-or-nothing "\\([^ \t(*][^)]*\\)?)" ; the arg list must not start
       "[ \t]*[^ \t;(]"              ; with an asterisk or parentheses
       ))

(setq eide-cc-imenu-c-generic-expression
      `(
        ;; General functions
        (nil          , eide-cc-imenu-c-function 1)

        ;; Interrupts
        ;;("--function" , eide-cc-imenu-c-interrupt 1)
        ;;("Interrupts" , eide-cc-imenu-c-interrupt 1)
        ;;(nil          , eide-cc-imenu-c-interrupt 1)

        ;; Macros
        ;;("--function" , eide-cc-imenu-c-macro 1)
        ;;("Macros"     , eide-cc-imenu-c-macro 1)

        ;; struct
        ;;("--var"      , eide-cc-imenu-c-struct 1)
        ;;("struct"     , eide-cc-imenu-c-struct 1)

        ;; enum
        ;;("--var"      , eide-cc-imenu-c-enum 1)
        ;;("enum"       , eide-cc-imenu-c-enum 1)

        ;; Defines
        ;;("--var"      , eide-cc-imenu-c-define 1)
        ;;("#define"    , eide-cc-imenu-c-define 1)
        ))

;;;; ==========================================================================
;;;; SETTINGS FOR FUNDAMENTAL MAJOR MODE
;;;; ==========================================================================

;; Default indentation: insert spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "C" AND "C++"
;;;; ==========================================================================

(add-hook
 'c-mode-hook
 '(lambda()
    (if eide-option-select-whole-symbol-flag
      ;; "_" should not be a word delimiter
      (modify-syntax-entry ?_ "w" c-mode-syntax-table))

    ;; Indentation
    (setq tab-width eide-c-indent-offset) ; Number of spaces for one tab
    (c-set-style "K&R") ; Indentation style
    (setq c-basic-offset eide-c-indent-offset) ; Indentation offset (default value: 5)
    (c-set-offset 'case-label '+) ; Case/default in a switch (default value: 0)

    ;; Autofill minor mode
    ;; (automatic line feed beyond 80th column)
    ;;(auto-fill-mode 1)
    ;;(set-fill-column 80)

    ;; Show trailing spaces if enabled in options
    (if eide-config-show-trailing-spaces
      (setq show-trailing-whitespace t))

    ;; Turn hide/show mode on
    (if (not hs-minor-mode)
      (hs-minor-mode))
    ;; Do not hide comments when hidding all
    (setq hs-hide-comments-when-hiding-all nil)

    ;; Turn ifdef mode on (does not work very well with ^M turned into empty lines)
    (hide-ifdef-mode 1)

    ;; Add Imenu in the menu ("Index")
    ;; (useless here because menu-bar is hidden)
    ;;(imenu-add-menubar-index)

    ;; Imenu regex
    (setq cc-imenu-c++-generic-expression eide-cc-imenu-c-generic-expression)
    (setq cc-imenu-c-generic-expression   eide-cc-imenu-c-generic-expression)

    ;; Pour savoir si du texte est sélectionné ou non
    (setq mark-even-if-inactive nil)))

(add-hook
 'c++-mode-hook
 '(lambda()
    (if eide-option-select-whole-symbol-flag
      ;; "_" should not be a word delimiter
      (modify-syntax-entry ?_ "w" c-mode-syntax-table))

    ;; Indentation
    (setq tab-width eide-c-indent-offset) ; Number of spaces for one tab
    (c-set-style "K&R") ; Indentation style
    (setq c-basic-offset eide-c-indent-offset) ; Indentation offset (default value: 5)
    (c-set-offset 'case-label '+) ; Case/default in a switch (default value: 0)

    ;; Autofill minor mode
    ;; (automatic line feed beyond 80th column)
    ;;(auto-fill-mode 1)
    ;;(set-fill-column 80)

    ;; Show trailing spaces if enabled in options
    (if eide-config-show-trailing-spaces
      (setq show-trailing-whitespace t))

    ;; Turn hide/show mode on
    (if (not hs-minor-mode)
      (hs-minor-mode))
    ;; Do not hide comments when hidding all
    (setq hs-hide-comments-when-hiding-all nil)

    ;; Turn ifdef mode on (does not work very well with ^M turned into empty lines)
    (hide-ifdef-mode 1)

    ;; Add Imenu in the menu ("Index")
    ;; (useless here because menu-bar is hidden)
    ;;(imenu-add-menubar-index)

    ;; Imenu regex
    (setq cc-imenu-c++-generic-expression eide-cc-imenu-c-generic-expression)
    (setq cc-imenu-c-generic-expression   eide-cc-imenu-c-generic-expression)

    ;; Pour savoir si du texte est sélectionné ou non
    (setq mark-even-if-inactive nil)))

(font-lock-add-keywords
 'c-mode
 '(;;("__interrupt" . font-lock-keyword-face)
   ("uint8" . font-lock-type-face)
   ("uint16" . font-lock-type-face)
   ("uint32" . font-lock-type-face)
   ("int8" . font-lock-type-face)
   ("int16" . font-lock-type-face)
   ("int32" . font-lock-type-face)
   ("TODO" . font-lock-warning-face)))

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "EMACS LISP"
;;;; ==========================================================================

(add-hook
 'emacs-lisp-mode-hook
 '(lambda()
    (if eide-option-select-whole-symbol-flag
      ;; "-" should not be a word delimiter
      (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))

    ;; Indentation
    (setq tab-width 2)
    (setq lisp-body-indent 2)
    ;; Indentation after "if" (with default behaviour, the "then" statement is
    ;; more indented than the "else" statement)
    (put 'if 'lisp-indent-function 1)

    ;; Autofill minor mode
    ;; (pour ne pas dépasser la 80ème colonne)
    ;;(auto-fill-mode 1)
    ;;(set-fill-column 80)

    ;; Show trailing spaces if enabled in options
    (if eide-config-show-trailing-spaces
      (setq show-trailing-whitespace t))))

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "SGML" (HTML, XML...)
;;;; ==========================================================================

(add-hook
 'sgml-mode-hook
 '(lambda()
    ;; Indentation
    (setq tab-width 2)

    ;; Show trailing spaces if enabled in options
    (if eide-config-show-trailing-spaces
      (setq show-trailing-whitespace t))))

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "SHELL SCRIPT"
;;;; ==========================================================================

(add-hook
 'shell-mode-hook
 '(lambda()
    ;; Indentation
    (setq tab-width 2)

    ;; Show trailing spaces if enabled in options
    (if eide-config-show-trailing-spaces
      (setq show-trailing-whitespace t))))

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "PERL"
;;;; ==========================================================================

(add-hook
 'perl-mode-hook
 '(lambda()
    ;; Indentation
    (setq tab-width 2)

    ;; Show trailing spaces if enabled in options
    (if eide-config-show-trailing-spaces
      (setq show-trailing-whitespace t))))

;;;; ==========================================================================
;;;; SETTINGS FOR MAJOR MODE "PYTHON"
;;;; ==========================================================================

(add-hook
 'python-mode-hook
 '(lambda()
    (if eide-option-select-whole-symbol-flag
      ;; "_" should not be a word delimiter
      (modify-syntax-entry ?_ "w" python-mode-syntax-table))

    ;; Indentation
    (setq tab-width 4)
    (setq python-indent 4)

    ;; Show trailing spaces if enabled in options
    (if eide-config-show-trailing-spaces
      (setq show-trailing-whitespace t))))

;;;; ==========================================================================
;;;; INITIALIZATION
;;;; ==========================================================================

;; Load options file (it will be closed at the end of "rebuild", so that
;; current buffer - from .emacs.desktop - is not changed)
(find-file-noselect (concat "~/" eide-options-file))
;; Options file must be rebuilt before calling eide-project-start-with-project
;; (which may read this file to create current project config file)
(eide-config-rebuild-options-file)
;; Check if a project is defined, and start it.
;; NB: It is important to read desktop after mode-hooks have been defined,
;; otherwise mode-hooks may not apply.
(if (file-exists-p eide-project-file)
  (progn
    (find-file-noselect eide-project-file)
    (eide-project-start-with-project)))
;; Update frame title and menu
(eide-project-update-frame-title)
;; Start with "editor" mode
(eide-keys-configure-for-editor)
;; eide-options-file might be present in desktop (in case emacs was closed
;; while editing options): we must close it again.
(if (get-buffer eide-options-file)
  (kill-buffer eide-options-file))
;; Close temporary buffers from ediff sessions (if emacs has been closed during
;; an ediff session, .emacs.desktop contains temporary buffers (.ref or .new
;; files) and they have been loaded in this new emacs session).
(let ((l-buffer-name-list (mapcar 'buffer-name (buffer-list))))
  (dolist (l-buffer-name l-buffer-name-list)
    (if (or (string-match "^\* (REF)" l-buffer-name) (string-match "^\* (NEW)" l-buffer-name))
      ;; this is a "useless" buffer (.ref or .new)
      (kill-buffer l-buffer-name))))
;; Init
(setq eide-current-buffer (buffer-name))
(eide-menu-init)
(eide-windows-init)

(provide 'eide)

;;; init.el ends here
