#!/bin/sh

svn up
compiled_files=`ls src/*.elc 2> /dev/null`
if [ "$compiled_files" != "" ]; then
  make
fi
