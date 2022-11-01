#!/bin/sh

# Emacs-IDE package uninstallation for the user

# Copyright © 2022 Cédric Marie

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

printf "\033[1mRemove eide-$VERSION.tar\033[0m\n"
rm -vf eide-$VERSION.tar

printf "\n\033[1mRemove ~/.emacs.d/elpa/eide-$VERSION\033[0m\n"
rm -vrf ~/.emacs.d/elpa/eide-$VERSION

if grep -q \(eide-start\) ~/.emacs; then
    printf "\nYou must remove (eide-start) from your ~/.emacs\n"
else
    printf "\nNothing more to do:\n"
    printf "(eide-start) is not present in your ~/.emacs\n"
fi
