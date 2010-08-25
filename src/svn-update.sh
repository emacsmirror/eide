#!/bin/sh

# svn-update.sh
# This file is part of Emacs-IDE

# Copyright (C) 2005-2010 Cédric Marie

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

svn up
compiled_files=`ls src/*.elc 2> /dev/null`
if [ "$compiled_files" != "" ]; then
  make
fi
