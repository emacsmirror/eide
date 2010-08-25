/-----------\
| Emacs-IDE |
\-----------/

Version 1.1 - 04/2009

-------------------------------------------------------------------------------
About Emacs-IDE
-------------------------------------------------------------------------------

Emacs-IDE is an IDE (Integrated Development Environment) interface for Emacs.
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

License  : GPL (http://www.fsf.org/licensing/licenses/gpl.html)

Requires : - Emacs (http://www.gnu.org/software/emacs/)
           - Etags (comes as a part of Emacs distribution)
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
        |               Window "toolbar"                          |
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
