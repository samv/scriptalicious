#!/usr/bin/perl

use warnings;
use strict;

use Scriptalicious;

use Test::More tests => 3;

my ($rc, @out) = capture_err($^X, "-Mlib=lib", "t/fork.pl", "-v");

is($rc, 0, "Command completed successfully");

my $out = join "", @out;

like($out, qr/\(parent\)/, "Parent managed to use the timer");
like($out, qr/\(child\)/, "Child managed to use the timer");
