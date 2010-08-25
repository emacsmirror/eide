#!/bin/sh

# svn-check.sh
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

# Get current revision
for rev in `svn info --xml | grep revision | cut -d'"' -f2`; do
  current_rev=$rev;
done

# Get latest revision
for rev in `svn info --xml -rHEAD | grep revision | cut -d'"' -f2`; do
  latest_rev=$rev;
done

# NB: we check the second revision in svn info output, because:
# - first revision means: last commit in whole repository
# - second revision means: last modification

if [ $current_rev != $latest_rev ]; then
  echo "yes"
else
  echo "no"
fi
