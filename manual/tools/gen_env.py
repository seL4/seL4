#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

import subprocess
import datetime
import sys

output = []

# Check arguments.
if len(sys.argv) != 2:
    print("Usage: gen_env.py <output file>")
    sys.exit(1)
output_filename = sys.argv[1]

# Fetch details about the current repo revision.
p = subprocess.Popen(["git", "log", "-r", "HEAD", "-n", "1", "--pretty=format:%ci"],
                     stdout=subprocess.PIPE)
commit_date_string = p.communicate()[0]

# in python3, this is a bytes, so convert it to a str
if isinstance(commit_date_string, bytes):
    commit_date_string = commit_date_string.decode("utf-8")

commit_date = datetime.datetime.strptime(commit_date_string.split()[0], "%Y-%m-%d")

# Output in a format that LaTeX can read.
output.append('\\newcommand{\\commitdate}{%s}' % (
    commit_date.strftime("%-d %B %Y")))
output.append('\\newcommand{\\commityear}{%s}' % (
    commit_date.strftime("%Y")))

# Output file, if it has changed.
new_data = "\n".join(output) + "\n"
old_data = None
try:
    with open(sys.argv[1], "r") as f:
        old_data = f.read()
except:
    pass
if new_data != old_data:
    with open(sys.argv[1], "w") as f:
        f.write(new_data)

# Done!
sys.exit(0)
