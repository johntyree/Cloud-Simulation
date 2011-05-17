#!/usr/bin/perl
use strict;
use warnings;

my $header = '=INFO REPORT==== \d+-\w+-\d+::\d+:\d+:\d+ ===';

while (<stdin>) {
    if ($_ =~ /($header)/) {
        while (<stdin>) {
            if ($_ =~ /^\n$/) {
                print "\n";
                last;
            }
            if ($_ =~ /{(\d+),(\d+),(\d+)},\[(.*)\]}/) {
                my $coords = "$1 $2 $3";
                my $drops = $4;
                $drops =~ s/{dropstate,([\d.]+)},?/$1 /g;
                my @drops = split " ",$drops;
                foreach (@drops) {
                    print $coords." $_\n";
                }
            }
            #$lines .= $line;
        }
        #print $lines.="\n\n\n";
    }
}

