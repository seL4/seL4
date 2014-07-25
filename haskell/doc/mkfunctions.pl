#! /usr/bin/perl -w
#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

open OUT, ">functions.aux";

print OUT <<END
% Prelude (but missing from lambdaTeX)
\\functions(min, max)

% Data.Bits
\\functions(bit, shiftL, shiftR, testBit, setBit, clearBit, bitSize)

% Control.Monad
\\functions(lift, liftM, mapM, mapM_, zipWithM, zipWithM_, when, unless, fail)

% Control.Monad.State
\\functions(runState, runStateT, get, put, gets, modify)

% Control.Monad.Error
\\functions(throwError, catchError, runErrorT)
END
;

@lhsfiles = ();

@srcdirs = map { chomp; $_ } `find ../src -type d`;

foreach $dir (@srcdirs) {
    opendir DIR, $dir or die "couldn't read $dir";
    @files = readdir DIR;
    closedir DIR;

    @lhsfiles = (@lhsfiles, map { $dir."/".$_ } grep (/\.lhs$/, @files));
}

foreach $source (@lhsfiles) {
    print OUT "% $source\n";
    open SRC, $source;
    @lines = <SRC>;
    close SRC;
    @codelines = grep $_ =~ /^>/, @lines;
    $code = join "", @codelines;
    $code =~ s/^> //mg;
    print OUT "\\functions ( ";
    while ($code =~ /(\n|{)\s*(([a-z][A-Za-z0-9']*(,\s+)?)*)\s+::/sg) {
	$names = $2;
	$names =~ s/\n//g;
	$names =~ s/[0-9]//g;
	print OUT "$names, ";
    }
    print OUT ")\n\n";
}

close OUT;

