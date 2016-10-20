#!/usr/bin/perl -w

@file_stack=();

while (<>) {
    $input=$_;
    if ($input =~ /^# 1 "kernel_all.c"/) {
    } elsif ($input =~ /^# 1 "(.*\..)"/) {
        # Found a new header
        $header = $1;
        foreach $item (@file_stack) {
            if ($item eq $header) {
                print "Circular includes found:\n";
                map { print "$_\n" } @file_stack;
                print "$header\n";
                exit -1;
            }
        }
        push @file_stack, $header;
    } elsif ($input =~ /^# \d+ "(.*\..)"/) {
        # Have popped back up to an earlier header
        $header = $1;
        while ($file_stack[$#file_stack] ne $header) {
            pop @file_stack;
        }
    }
}
exit 0;
