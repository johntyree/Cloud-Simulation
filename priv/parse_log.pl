#!/usr/bin/perl
use strict;
use warnings;

# This is the error_logger header. Not in use because it is too slow.
# my $header = '=INFO REPORT==== \d+-\w+-\d+::\d+:\d+:\d+ ===';

while (<stdin>) {
    if ($_ !~ /{(\d+),(\d+),(\d+)},\[(.*?)\]}/g) {
        next;
    }
    while ($_ =~ /{(\d+),(\d+),(\d+)},\[(.*?)\]}/g) {
        my $coords = "$1 $2";
        my $drops = $4;
        $drops =~ s/{dropstate,([\d.]+)},?/$1 /g;
        my @drops = split " ",$drops;
        foreach (@drops) {
            print $coords." $_ ";
        }
    }
    print "\n";
}
