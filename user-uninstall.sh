#!/bin/sh

# Emacs-IDE package uninstallation for the user
#
# Copyright © 2022-2025 Cédric Marie
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
if [ ! -e user-uninstall.sh ]; then
    printf "\033[1;31mThis script must be executed from the root directory\033[0m\n"
    exit 1
fi

# Remove local package file (and possibly old versions)
printf "\033[1mRemove local package(s)\033[0m\n"
PKG_FILES=$(ls --color=never eide-*.tar 2> /dev/null)
if [ ! -z "$PKG_FILES" ]; then
    rm -vf $PKG_FILES
fi

# Remove user installed package (and possibly old versions)
printf "\n\033[1mRemove ~/.emacs.d/elpa/eide-*\033[0m\n"
rm -vrf ~/.emacs.d/elpa/eide-*

if grep -q \(eide-start\) ~/.emacs; then
    printf "\nYou must remove (eide-start) from your ~/.emacs\n"
else
    printf "\nNothing more to do:\n"
    printf "(eide-start) is not present in your ~/.emacs\n"
fi
