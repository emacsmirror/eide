/-----------\
| Emacs-IDE |
\-----------/

Version 1.0  (12/2008)

-------------------------------------------------------------------------------
About Emacs-IDE
-------------------------------------------------------------------------------

IDE (Integrated Development Environment) interface for Emacs.
For C language (limited features for other languages).

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


NB : Compilation is optional. Compiled code is just supposed to execute
     faster...
     If you compile, keep in mind that compiled files (*.elc) will always be
     used instead of source files (*.el). As a consequence, any change in
     source files will be ignored until you compile again.
     So, if you update from SVN repository (svn update), remember to compile
     after update.


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

NB : If you click on right button over window "file", all other windows are
     closed. Click again to get them back.


-------------------------------------------------------------------------------
Instructions
-------------------------------------------------------------------------------

Instructions are available in "Help" page, in Emacs.
Click on right button over window "menu", and select "Help" in popup menu.
