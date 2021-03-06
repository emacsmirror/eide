=========
Emacs-IDE
=========

:Version: 2.2.0+
:Homepage: https://eide.hjuvi.fr.eu.org/

License
=======

Copyright (C) 2008-2020 Cédric Marie <cedric@hjuvi.fr.eu.org>

This program is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.

Description
===========

Emacs-IDE (eide) is a package for Emacs that provides IDE features (Integrated
Development Environment).

Although most of these features are already available in Emacs, the purpose of
this package is to integrate them into a user-friendly interface, with
dedicated windows (source files, menu, and ouput), convenient keyboard
shortcuts, and project management.

It is suitable for almost all languages (as long as they are supported by
Ctags). Cscope provides additional browsing facility for C/C++ files.

Information
===========

:License: GPLv3 or later
:Programming language: Emacs Lisp
:Dependencies: Emacs (>= 25.1), Ctags, Cscope.
:Supported OS: GNU/Linux

Instructions
============

When you launch Emacs, it should look like this:

::

  +-----------------------------------------+---------------+
  |                                         |               |
  |                                         |               |
  |       "source" window                   | "menu" window |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  +-----------------------------------------+---------------+
  |                                                         |
  |               "output" window                           |
  |                                                         |
  +---------------------------------------------------------+

If you click on right button over "source" window, all other windows are
closed. Click again to get them back.

Instructions are available in "Help" page: click on right button over "menu"
window, and select "Help" in popup menu.

Configuration
=============

Options provided in customization
---------------------------------

Emacs-IDE provides some options in customization. To edit them, click on right
button over "menu" window, and select "Customize" in popup menu.
At top level, you will find categories, and one specific option - called
"Override Emacs settings" - that can globally disable all options in "Emacs
settings" category, if disabled.

While other categories provide Emacs-IDE related options, "Emacs settings"
category provide options that override standard Emacs behaviour:

* F1-F12 key bindings, in order to provide easy access to basic IDE features
  (tags, cscope, grep, compilation...).
* Cscope update policy, in order to add an automatic mode that will update the
  database only when a file is modified in Emacs.

To save your settings, click on "Save for future sessions", and click on right
button to exit customization.

Options provided by themes
--------------------------

Two kind of themes are provided by Emacs-IDE:

* Color themes (eide-dark and eide-light)
* Themes to override standard Emacs behaviour

The settings provided by these themes used to be enabled by default in previous
Emacs-IDE releases (in "Emacs settings" category).
Now you have to enable these themes: click on right button over "menu" window,
and select "Customize themes" in popup menu.
You can add one of the color themes (eide-dark or eide-light), and any of the
following themes:

* eide-browsing
* eide-coding
* eide-display
* eide-settings

Quick start
===========

The directory from which Emacs is launched stands as the root directory. You
can change it during the session: click on right button over "menu" window, and
select "Change root directory" in popup menu.

To create a project, select the directory of the source code tree as the root
directory. Click on right button over "menu" window, and select "Create a
project in this directory". The project is added to your workspace.

To open your project in future sessions, click on right button over "menu"
window, select "Display projects list", and select your project.
