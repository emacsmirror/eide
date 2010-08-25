/-----------\
| Emacs-IDE |
\-----------/

Version 1.3 - 03/2010

-------------------------------------------------------------------------------
About Emacs-IDE
-------------------------------------------------------------------------------

Emacs-IDE is an IDE (Integrated Development Environment) interface for Emacs
(code browsing, compilation, debug...).

It is suitable for almost all languages (as long as they are supported by
ctags). Cscope provides additional browsing facility for C/C++ files.

Please note that Emacs-IDE does not stand as a package for Emacs. It should be
considered as an IDE built on top of Emacs.
A package is designed to be loaded from user configuration file. It is also
supposed not to modify standard Emacs behaviour, but only add new features that
can be enabled or disabled.
Emacs-IDE takes the place of user configuration file. It intentionally changes
Emacs behaviour - in order to manage display in dedicated windows - and
redefines reserved keys - in order to have a wide range of convenient
shortcuts.

Homepage : http://emacs-ide.sourceforge.net/

License  : GPL (http://www.gnu.org/licenses/gpl.html)

Requires : - Emacs  (http://www.gnu.org/software/emacs/)
           - Ctags  (http://ctags.sourceforge.net/)
           - Cscope (http://cscope.sourceforge.net/)


-------------------------------------------------------------------------------
Installation
-------------------------------------------------------------------------------

Compile .............................. make

Install (create link ~/.emacs) ....... make install

Uninstall (remove link ~/.emacs) ..... make uninstall

Compilation is optional. Compiled code is just supposed to execute faster...
If you compile, keep in mind that compiled files (*.elc) will always be used
instead of source files (*.el). As a consequence, any change in source files
will be ignored until you compile again.


-------------------------------------------------------------------------------
Windows layout overview
-------------------------------------------------------------------------------

When you launch Emacs, it should look like this :

        -----------------------------------------------------------
        |                                         |               |
        |                                         |               |
        |       Window "file"                     | Window "menu" |
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
        |               Window "results"                          |
        |                                                         |
        -----------------------------------------------------------

If you click on right button over window "file", all other windows are closed.
Click again to get them back.


-------------------------------------------------------------------------------
Instructions
-------------------------------------------------------------------------------

Instructions are available in "Help" page, in Emacs.
Click on right button over window "menu", and select "Help" in popup menu.


-------------------------------------------------------------------------------
Quick start
-------------------------------------------------------------------------------

The directory from which emacs is launched is important: it stands as the root
directory during emacs session.
If you work on a project - which is the main purpose of Emacs-IDE - you should
always run emacs from the root directory of your project.
When you first run emacs for a project, click on right button over window
"menu", and select "Create project".
