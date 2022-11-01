#!/bin/sh

# Emacs-IDE package installation for the user

# Copyright © 2014-2022 Cédric Marie

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

VERSION=2.3.0

# Emacs version >= 25 is required
printf "\033[1mCheck Emacs version >= 25\033[0m\n"
emacs_major_version=`emacs --version | grep -m1 "GNU Emacs" | cut -d" " -f3 | cut -d"." -f1`
if [ "${emacs_major_version}" != "" ]; then
  if [ "${emacs_major_version}" -gt "24" ]; then
    printf "OK (${emacs_major_version})\n"
  else
    printf "\033[1;31mFAILED: Your version of Emacs (${emacs_major_version}) is not supported (version >= 25 is required)\033[0m\n"
    exit 1
  fi
else
  printf "\033[1;31mFAILED: Can't find Emacs version\033[0m\n"
  exit 1
fi

# Create the package (.tar file)
rm -rf eide-$VERSION eide-$VERSION.tar
printf "\n\033[1mCopy source files to package directory\033[0m\n"
mkdir eide-$VERSION
cp -v src/*.el src/themes/*.el eide-$VERSION
printf "\n\033[1mCreate package archive\033[0m\n"
tar -cvf eide-$VERSION.tar eide-$VERSION
rm -rf eide-$VERSION

# Install the package in ~/.emacs.d
printf "\n\033[1mInstall package eide-$VERSION.tar\033[0m\n"
# --batch: don't use interactive display (implies -q: don't load ~/.emacs)
# --execute: execute package-install-file command
if emacs --batch --execute "(package-install-file \"$PWD/eide-$VERSION.tar\")" ; then
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
