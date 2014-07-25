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

print <<EOF
digraph G {
    page = "8.26, 11.69";
    size = "10.5, 7.2";
    orientation = landscape;
    ratio = fill;
    fontname = "Palatino-Roman";
    fontsize = 24;
EOF
;

@lhsfiles = ();

@srcdirs = ("../src", "../src/SEL4", "../src/SEL4/Machine", "../src/SEL4/Model",
    "../src/SEL4/Object", "../src/SEL4/Kernel", "../src/SEL4/API");

foreach $dir (@srcdirs) {
    opendir DIR, $dir or die "couldn't read $dir";
    @files = readdir DIR;
    closedir DIR;

    @lhsfiles = (@lhsfiles, map { $dir."/".$_ } grep (/\.lhs$/, @files));
}

foreach $source (@lhsfiles) {
    open SRC, $source;
    $code = join "", <SRC>;
    close SRC;

    $code =~ /> module (SEL4[a-zA-Z0-9.]*)/;
    $module = $1;
    next unless defined $module;
    $moduledepth = 0;
    $moduledepth++ while($module =~ /\./g);

    while($code =~ /> import (qualified )?({-# SOURCE #-} )?(SEL4[a-zA-Z0-9.]*)/g) {
	$import = $3;
	$is_source = defined $2;
	@options = ();

	if($is_source) { @options = (@options, "style=dotted", "dir=back"); }

	$importdepth = 0;
        $importdepth++ while($import =~ /\./g);
	if($moduledepth == $importdepth) {
	    @options = (@options, "constraint=false");
	}

	$options = join ",", @options;
	if($is_source) {
	    print qq(\t"$import" -> "$module" [$options];\n);
	} else {
	    print qq(\t"$module" -> "$import" [$options];\n);
	}
    }
}

print "}\n";

