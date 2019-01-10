#!/usr/bin/env bash
#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#

#
# Execute a command that generates a single output file. If the output file is
# has unchanged contents, restore the old modification date to prevent GNU make
# et. al. from continuing building down the dependency chain.
#

if [ $# -le 1 ]; then
    echo "Usage: $0 <output file> <cmd> [<cmd args> ...]"
    exit 1
fi

# Get the target file
TARGET_FILE=$1
shift

# If the file doesn't exist, we always run the command.
if [ ! -e $TARGET_FILE ]; then
    V=$@
    sh -c "$V"
    exit $?
fi

# Make a copy
TMP_FILE=`mktemp /tmp/XXXXXXXX`
cp -a $TARGET_FILE $TMP_FILE

# Run the command
V=$@
sh -c "$V"
ERROR_CODE=$?

# Restore the old file if the contents are the same.
if cmp $TMP_FILE $TARGET_FILE > /dev/null 2> /dev/null; then
    mv -f $TMP_FILE $TARGET_FILE
else
    rm -f $TMP_FILE
fi

# Return with the error code.
exit $ERROR_CODE

