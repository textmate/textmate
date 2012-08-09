#! /usr/bin/perl

# Copyright (C) 2000-2012  Free Software Foundation, Inc.

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


require 5;
use Getopt::Long;

my $USAGE = <<ENDUSAGE;
Remove \@tindex lines from files that were already present in previous
versions.

Usage: $0 [--old=EXT] FILE...
       $0 --help
       $0 --version

  --help	display this help and exit
  --version	print version and exit
  --old=DIR	find old files in DIR

The script performs two passes.  In the first pass, Texinfo files from
DIR are scanned for \@tindex lines, and identifiers in them are
recorded.  In a second pass, Texinfo files in the current directory
are scanned, and \@tindex lines for identifiers that were recorded in
the first pass are removed.  Old file contents are saved in files
with extension ".orig".  A list of modified files and removed \@tindex
identifiers is printed to stdout at the end.
ENDUSAGE

sub fatal {
    print STDERR "$0: ", @_, ".\n";
    exit 1;
}

my $help = 0;
my $version = 0;
my $old;

my $rc = GetOptions ('help' => \$help, 'version' => \$version,
                     'old=s' => \$old);
if ($version) {
    print "0.1\n";
    exit 0;
} elsif (!$rc || !$old || @ARGV) {
    print $USAGE;
    exit 1;
} elsif ($help) {
    print $USAGE;
    exit 0;
}

# Fill the hash %tindex with associations VAR -> COUNT where
# the keys VAR are identifiers mentioned in @tindex lines in the older
# files to process and COUNT is the number of times they are seen in
# the files.

my %tindex;
my %removed;
my @old_files = glob "$old/*.texi";
my @new_files = glob "*.texi";
fatal ("No Texinfo files found in `$old'") unless @old_files;
fatal ("No Texinfo files found in current directory") unless @new_files;

print "Scanning old files for \@tindex lines\n";
foreach $file (@old_files) {
    open (IN, "<$file") or fatal "Cannot open $file: $!";
    while (<IN>) {
	++$tindex{$1} if /^\s*\@tindex\s+(\S+)/;
    }
    close IN;
}

# Process current files and remove those @tindex lines which we
# know were already present in the files scanned above.

print "Removing old \@tindex lines\n";
foreach $file (@new_files) {
    my $modified = 0;
    my $contents = "";

    open (IN, "< $file") or fatal "Cannot open $file.orig for reading: $!";
    while (<IN>) {
	if (/^\s*\@tindex\s+(\S+)/ && $tindex{$1}) {
	    ++$removed{$1};
	    $modified = 1;
	} else {
	    $contents = $contents . $_;
	}
    }

    close IN;

    if ($modified) {
	print "  $file\n";
	system ("cp $file $file.orig") == 0 or fatal "Cannot backup $file: $!";
	open (OUT, ">$file") or fatal "Cannot open $file for writing: $!";
	print OUT $contents;
	close OUT;
    }
}

# Print a list of identifiers removed.

print "Removed \@tindex commands for:\n";
my $key;
foreach $key (keys %removed) {
    print "  $key\n";
}

