=====================================
Instructions for installing Emacs-IDE
=====================================

You can either:

- Install compiled files in standard system directories
- Create and install a package

You should not mix these methods, because two different versions would be
installed in two different places.

The second method is prefered, but you can also use the first one if you want
to install it for all users.

Please note that with the second method, if you're installing a development
version, the package version number is not relevant.

Installing in standard system directories
=========================================
You can install Emacs-IDE:

- either for all users:
  Execute "./install" (with root privileges) in a terminal.

- or for current user only:
  Execute "./install -l" in a terminal.

At the end of installation, instructions are given for enabling Emacs-IDE in
your ~/.emacs.

Emacs-IDE can be uninstalled using "./uninstall" command (with -l option if it
was installed for current user only).
Instructions are given for removing Emacs-IDE from your ~/.emacs as well.

Installing eide package
=======================
You just have to create the package and install it.

::

  ./build-package
  ./install-package

At the end of installation, instructions are given for enabling Emacs-IDE in
your ~/.emacs.
