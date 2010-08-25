#!/bin/sh

# Get current revision
for rev in `svn info --xml | grep revision | cut -d'"' -f2`; do
  current_rev=$rev;
done

# Get latest revision
for rev in `svn info --xml -rHEAD | grep revision | cut -d'"' -f2`; do
  latest_rev=$rev;
done

# NB : we check the second revision in svn info output, because:
# - first revision means: last commit in whole repository
# - second revision means: last modification

if [ $current_rev != $latest_rev ]; then
  echo "yes"
else
  echo "no"
fi
