;;; eide.el --- Emacs-IDE

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

(provide 'eide)

(if (featurep 'xemacs)
  (progn
    (read-string "Sorry, XEmacs is not supported by Emacs-IDE, press <ENTER> to exit...")
    (kill-emacs)))

;; Project directory
;; On Windows: it is necessary to open a temporary file for the directory path
;; to be correct (Windows standard vs Unix)
;; On Linux: it is also useful to expand path (~ => /home/xxx/).
;; NB: "temp" was first used as a temporary filename, but it causes the project
;; directory to be changed to "temp" if "temp" already exists and is a
;; directory!... Hence a filename that can not exist! :-)

(let ((l-temp-file "this-is-a-temporary-file-for-emacs-ide"))
  (find-file l-temp-file)
  (setq eide-root-directory default-directory)
  (kill-buffer l-temp-file))

;; Emacs modules
(require 'desktop)
(require 'hideshow)
(require 'imenu)
(require 'mwheel)
(require 'ediff)
(require 'gdb-ui)

;; Emacs-IDE modules
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

(defvar eide-cc-imenu-c-generic-expression nil)
(defvar eide-cc-imenu-c-macro nil)
(defvar eide-cc-imenu-c-struct nil)
(defvar eide-cc-imenu-c-enum nil)
(defvar eide-cc-imenu-c-define nil)
(defvar eide-cc-imenu-c-function nil)
(defvar eide-cc-imenu-c-interrupt nil)

;;;; ==========================================================================
;;;; INTERNAL FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Global settings.
;; ----------------------------------------------------------------------------
(defun eide-i-global-settings ()
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
  ;; Show end of buffer
  ;;(setq-default indicate-empty-lines t)
  ;; "One line at a time" scrolling
  (setq-default scroll-conservatively 1)
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
  ;; TODO: not used anymore in project configuration => still necessary?
  (setq line-move-ignore-invisible t)
  ;; Display current function (relating to cursor position) in info line
  ;; (if possible with current major mode)
  (which-function-mode)
  ;; "Warning" color highlight when possible error is detected
  ;;(global-cwarn-mode)
  ;; Do not prompt for updating tag file if necessary
  (setq tags-revert-without-query t)
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
    (cscope-set-initial-directory eide-root-directory))

  ;; ediff: Highlight current diff only
  ;;(setq ediff-highlight-all-diffs nil)
  ;; ediff: Control panel in the same frame
  (if window-system
    (ediff-toggle-multiframe))
  ;; ediff: Split horizontally for buffer comparison
  (setq ediff-split-window-function 'split-window-horizontally)

  ;; gdb: Use graphical interface
  (setq gdb-many-windows t))

;; ----------------------------------------------------------------------------
;; Add hooks for major modes.
;; ----------------------------------------------------------------------------
(defun eide-i-add-hooks ()
  ;; Construction de la liste des fonctions (imenu)
  ;; Utilisation d'expressions régulières
  ;; (il faut pour cela laisser imenu-extract-index-name-function = nil)
  ;; Il faut redéfinir les expressions, car les expressions par défaut amènent
  ;; beaucoup d'erreurs : du code est parfois interprété à tort comme une
  ;; définition de fonction)

  (let ((l-regex-word "[a-zA-Z_][a-zA-Z0-9_:<>~]*")
        (l-regex-word-no-underscore "[a-zA-Z][a-zA-Z0-9_:<>~]*")
        (l-regex-space "[ \t]+")
        ;;(l-regex-space-or-crlf "[ \t\n\r]+")
        (l-regex-space-or-crlf-or-nothing "[ \t\n\r]*")
        (l-regex-space-or-crlf-or-comment-or-nothing "[ \t\n\r]*\\(//\\)*[^\n\r]*[ \t\n\r]*")
        ;;(l-regex-space-or-crlf-or-comment-or-nothing "[ \t\n\r]*\\(//\\)*[^\n\r]*[\n\r][ \t\n\r]*")
        (l-regex-space-or-nothing "[ \t]*"))

    (setq eide-cc-imenu-c-macro
          (concat
           "^#define" l-regex-space
           "\\(" l-regex-word "\\)(" ))

    (setq eide-cc-imenu-c-struct
          (concat
           "^typedef"  l-regex-space "struct" l-regex-space-or-crlf-or-nothing
           "{[^{]+}" l-regex-space-or-nothing
           "\\(" l-regex-word "\\)" ))

    (setq eide-cc-imenu-c-enum
          (concat
           "^typedef" l-regex-space "enum" l-regex-space-or-crlf-or-nothing
           "{[^{]+}" l-regex-space-or-nothing
           "\\(" l-regex-word "\\)" ))

    (setq eide-cc-imenu-c-define
          (concat
           "^#define" l-regex-space
           "\\(" l-regex-word "\\)" l-regex-space ))

    (setq eide-cc-imenu-c-function
          (concat
           "^\\(?:" l-regex-word-no-underscore "\\*?" l-regex-space "\\)*" ; void* my_function(void)
           "\\*?" ; function may return a pointer, e.g. void *my_function(void)
           "\\(" l-regex-word "\\)"
           l-regex-space-or-crlf-or-nothing "("
           l-regex-space-or-crlf-or-nothing "\\([^ \t(*][^)]*\\)?)" ; the arg list must not start
           ;;"[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
           l-regex-space-or-crlf-or-comment-or-nothing "{" ))

    (if nil
      (progn
        ;; temp: remplace la définition au-dessus
        (setq eide-cc-imenu-c-function
              (concat
               "^\\(?:" l-regex-word l-regex-space "\\)*"
               "\\(" l-regex-word "\\)"
               l-regex-space-or-nothing "("
               "\\(" l-regex-space-or-crlf-or-nothing l-regex-word "\\)*)"
               ;;l-regex-space-or-nothing "\\([^ \t(*][^)]*\\)?)"   ; the arg list must not start
               ;;"[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
               l-regex-space-or-crlf-or-nothing "{" ))
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
           "\\(__interrupt"  l-regex-space
           "\\(" l-regex-word l-regex-space "\\)*"
           l-regex-word "\\)"
           l-regex-space-or-nothing "("
           l-regex-space-or-nothing "\\([^ \t(*][^)]*\\)?)" ; the arg list must not start
           "[ \t]*[^ \t;(]"            ; with an asterisk or parentheses
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
            )))

  ;; Default indentation: insert spaces instead of tabs
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4) ; Number of spaces for one tab

  ;; C major mode
  (add-hook
   'c-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" c-mode-syntax-table))

      ;; Indentation
      (c-set-style "K&R") ; Indentation style
      (if eide-config-use-indent-offsets-flag
        (progn
          (setq tab-width eide-config-c-indent-offset)
          (setq c-basic-offset eide-config-c-indent-offset)))
      (c-set-offset 'case-label '+) ; Case/default in a switch (default value: 0)

      ;; Turn hide/show mode on
      (if (not hs-minor-mode)
        (hs-minor-mode))
      ;; Do not hide comments when hidding all
      (setq hs-hide-comments-when-hiding-all nil)

      ;; Turn ifdef mode on (does not work very well with ^M turned into empty lines)
      (hide-ifdef-mode 1)

      ;; Imenu regex
      (setq cc-imenu-c++-generic-expression eide-cc-imenu-c-generic-expression)
      (setq cc-imenu-c-generic-expression   eide-cc-imenu-c-generic-expression)

      ;; Pour savoir si du texte est sélectionné ou non
      (setq mark-even-if-inactive nil)))

  ;; C++ major mode
  (add-hook
   'c++-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" c-mode-syntax-table))

      ;; Indentation
      (c-set-style "K&R") ; Indentation style
      (if eide-config-use-indent-offsets-flag
        (progn
          (setq tab-width eide-config-c-indent-offset)
          (setq c-basic-offset eide-config-c-indent-offset)))
      (c-set-offset 'case-label '+) ; Case/default in a switch (default value: 0)

      ;; Turn hide/show mode on
      (if (not hs-minor-mode)
        (hs-minor-mode))
      ;; Do not hide comments when hidding all
      (setq hs-hide-comments-when-hiding-all nil)

      ;; Turn ifdef mode on (does not work very well with ^M turned into empty lines)
      (hide-ifdef-mode 1)

      ;; Imenu regex
      (setq cc-imenu-c++-generic-expression eide-cc-imenu-c-generic-expression)
      (setq cc-imenu-c-generic-expression   eide-cc-imenu-c-generic-expression)

      ;; Pour savoir si du texte est sélectionné ou non
      (setq mark-even-if-inactive nil)))

  ;; Shell Script major mode

  ;; Enable colors
  ;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  ;; Shell color mode is disabled because it disturbs shell-command (run
  ;; command), and I have no solution for that!...
  ;; - ansi-term: Does not work correctly ("error in process filter").
  ;; - eshell: Uses specific aliases.
  ;; - ansi-color-for-comint-mode-on: Does not apply to shell-command and
  ;;   disturb it ("Marker does not point anywhere"). Moreover, it is not
  ;;   buffer local (this would partly solve the issue).
  ;; - Using shell for shell-command: previous run command is not killed, even
  ;;   if process and buffer are killed.

  (add-hook
   'sh-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" sh-mode-syntax-table))
      ;; Indentation
      (if eide-config-use-indent-offsets-flag
        (progn
          (setq tab-width eide-config-sh-indent-offset)
          (setq sh-basic-offset eide-config-sh-indent-offset)))))

  ;; Emacs Lisp major mode
  (add-hook
   'emacs-lisp-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "-" should not be a word delimiter
        (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))

      ;; Indentation
      (if eide-config-use-indent-offsets-flag
        (progn
          (setq tab-width eide-config-lisp-indent-offset)
          (setq lisp-body-indent eide-config-lisp-indent-offset)
          ;; Indentation after "if" (with default behaviour, the "then" statement is
          ;; more indented than the "else" statement)
          (put 'if 'lisp-indent-function 1)))))

  ;; Perl major mode
  (add-hook
   'perl-mode-hook
   '(lambda()
      ;; Indentation
      (if eide-config-use-indent-offsets-flag
        (progn
          (setq tab-width eide-config-perl-indent-offset)
          (setq perl-indent-level eide-config-perl-indent-offset)))))

  ;; Python major mode
  (add-hook
   'python-mode-hook
   '(lambda()
      (if eide-option-select-whole-symbol-flag
        ;; "_" should not be a word delimiter
        (modify-syntax-entry ?_ "w" python-mode-syntax-table))

      ;; Indentation
      (if eide-config-use-indent-offsets-flag
        (progn
          (setq tab-width eide-config-python-indent-offset)
          (setq python-indent eide-config-python-indent-offset))))))

  ;; SGML (HTML, XML...) major mode
  (add-hook
   'sgml-mode-hook
   '(lambda()
      ;; Indentation
      (if eide-config-use-indent-offsets-flag
        (progn
          (setq tab-width eide-config-sgml-indent-offset)
          (setq sgml-basic-offset eide-config-sgml-indent-offset)))))

;; ----------------------------------------------------------------------------
;; Initialization.
;; ----------------------------------------------------------------------------
(defun eide-i-init ()
  (eide-config-init)
  ;; Migration from Emacs-IDE 1.5
  (if (and (not (file-exists-p (concat "~/" eide-config-file)))
           (file-exists-p "~/.emacs-ide.options"))
    (shell-command (concat "mv ~/.emacs-ide.options ~/" eide-config-file)))
  ;; Load options file (it will be closed at the end of "rebuild", so that
  ;; current buffer - from .emacs.desktop - is not changed)
  (find-file-noselect (concat "~/" eide-config-file))
  ;; Options file must be rebuilt before calling eide-project-start-with-project
  ;; (which may read this file to create current project config file)
  (eide-config-rebuild-config-file)
  ;; Migration from Emacs-IDE 1.5
  (if (and (not (file-exists-p eide-project-config-file))
           (file-exists-p ".emacs-ide.project"))
    (shell-command (concat "mv .emacs-ide.project " eide-project-config-file)))
  ;; Check if a project is defined, and start it.
  ;; NB: It is important to read desktop after mode-hooks have been defined,
  ;; otherwise mode-hooks may not apply.
  (if (file-exists-p eide-project-config-file)
    (progn
      (find-file-noselect eide-project-config-file)
      (eide-project-start-with-project)))
  ;; Update frame title and menu
  (eide-project-update-frame-title)
  ;; Start with "editor" mode
  (eide-keys-configure-for-editor)
  ;; eide-config-file might be present in desktop (in case emacs was closed
  ;; while editing options): we must close it again.
  (if (get-buffer eide-config-file)
    (kill-buffer eide-config-file))
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
  (eide-windows-init))

;;;; ==========================================================================
;;;; FUNCTIONS
;;;; ==========================================================================

;; ----------------------------------------------------------------------------
;; Open shell.
;;
;; output : eide-windows-update-output-buffer-id : "s" for "shell".
;; ----------------------------------------------------------------------------
(defun eide-shell-open ()
  (interactive)
  ;; Force to open a new shell (in current directory)
  (if eide-shell-buffer
    (kill-buffer eide-shell-buffer))
  (eide-windows-select-source-window t)
  ;; Shell buffer name will be updated in eide-i-windows-display-buffer-function
  (setq eide-windows-update-output-buffer-id "s")
  (shell))

;; ----------------------------------------------------------------------------
;; Start Emacs-IDE.
;; ----------------------------------------------------------------------------
(defun eide-start ()
  (eide-i-global-settings)
  (eide-i-add-hooks)
  (eide-i-init))

;;; eide.el ends here
