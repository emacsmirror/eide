#!/bin/sh

# Emacs-IDE package installation for the user
#
# Copyright © 2014-2025 Cédric Marie
#
# This file is part of Emacs-IDE.
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.

# Check current directory
if [ ! -e user-install.sh ]; then
    printf "\033[1;31mThis script must be executed from the root directory\033[0m\n"
    exit 1
fi

# Get information from eide.el
LINE=$(grep -m 1 "Version:" src/eide.el)
VERSION=$(expr match "$LINE" ";; Version: \(.*\)")
LINE=$(grep -m 1 "eide.el ---" src/eide.el)
SHORT_DESC=$(expr match "$LINE" ";;; eide.el --- \(.*\)")
LINE=$(grep -m 1 "Package-Requires:" src/eide.el)
DEPENDENCIES=$(expr match "$LINE" ";; Package-Requires: \(.*\)")
LINE=$(grep -m 1 "Homepage:" src/eide.el)
HOMEPAGE=$(expr match "$LINE" ";; Homepage: \(.*\)")

# Remove the directory of the current version (it might be present if the
# creation of the package has failed)
printf "\033[1mRemove eide-$VERSION directory if still present\033[0m\n"
rm -vrf eide-$VERSION

# Remove local package if already built
# (and possibly old versions if built before version upgrade)
printf "\n\033[1mRemove local package(s) (eide-*.tar) if already built\033[0m\n"
PKG_FILES=$(ls --color=never eide-*.tar 2> /dev/null)
if [ ! -z "$PKG_FILES" ]; then
    rm -vf $PKG_FILES
fi

# Remove user installed package
# (and possibly old versions if installed before version upgrade)
printf "\n\033[1mRemove ~/.emacs.d/elpa/eide-* if already installed\033[0m\n"
rm -vrf ~/.emacs.d/elpa/eide-*

# Create the package (.tar file)
printf "\n\033[1mCreate package directory (eide-$VERSION)\033[0m\n"
mkdir -v eide-$VERSION
printf "\n\033[1mCopy source files to package directory (eide-$VERSION)\033[0m\n"
cp -v src/*.el src/themes/*.el eide-$VERSION

printf "\n\033[1mCreate eide-pkg.el in package directory (eide-$VERSION)\033[0m\n"
echo "(define-package \"eide\" \"$VERSION\" \"$SHORT_DESC\"" > eide-$VERSION/eide-pkg.el
echo "  '$DEPENDENCIES" >> eide-$VERSION/eide-pkg.el
echo "  :homepage \"$HOMEPAGE\")" >> eide-$VERSION/eide-pkg.el
cat eide-$VERSION/eide-pkg.el

printf "\n\033[1mCreate package eide-$VERSION.tar\033[0m\n"
tar -cvf eide-$VERSION.tar eide-$VERSION
rm -rf eide-$VERSION

# Install the package in ~/.emacs.d
printf "\n\033[1mInstall package eide-$VERSION.tar\033[0m\n"
# --batch: don't use interactive display (implies -q: don't load ~/.emacs)
# --execute: execute package-install-file command
# With Emacs 26.1, (package-initialize) is necessary when Emacs version requirement is defined in
# eide-pkg.el (otherwise, it fails with the message: package.el is not yet initialized!)
if emacs --batch --execute "(progn (package-initialize) (package-install-file \"$PWD/eide-$VERSION.tar\"))" ; then
  printf "\nInstallation successful (version $VERSION)\n"
else
  printf "\n\033[1;31mInstallation failed\033[0m\n"
  exit 1
fi

if grep -q \(eide-start\) ~/.emacs; then
    printf "\nNothing more to do:\n"
    printf "(eide-start) is already present in your ~/.emacs\n"
else
    printf "\nAdd the following lines in your ~/.emacs:\n"
    printf "(package-initialize)\n"
    printf "(eide-start)\n"
fi
