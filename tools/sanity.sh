#!/bin/bash
#
# Copyright 2014, NICTA
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(NICTA_BSD)
#

# This script checks that two files are the same excluding 
# the first 10 lines, which are assumed to be license tags.
#
# We don't check that the first 10 lines are license tags, 
# which is fairly fragile and will break if the license length 
# changes, however that is LESS fragile than allowing
# the files this script checks to be different, which will
# result in the kernel and user using different constants
# and sending the developer into a pointless debugging spiral
# which could have been easily avoided using this script.

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <file1> <file2>"
    echo "$#"
    exit 1
fi 

# make copies of the files, skipping the first 10 lines 
# and transforming uintXX_t types to seL4_UintXX types
tail -n +10 $1 | sed 's/uint\([0-9]*\)_t/seL4_Uint\1/' | sed 's/word_t/seL4_Word/' > $1.tmp
tail -n +10 $2 | sed 's/uint\([0-9]*\)_t/seL4_Uint\1/' | sed 's/word_t/seL4_Word/' > $2.tmp

# diff the files without the license tags
diff=$(diff "$1".tmp "$2".tmp)

# clean up the copies
rm $1.tmp
rm $2.tmp

# if the diff is empty, success!
if [[ -z $diff ]]; then
    exit 0
fi

# fail, files were different
echo "Error $1 and $2 are different. Diff:"
echo $diff
exit 1

