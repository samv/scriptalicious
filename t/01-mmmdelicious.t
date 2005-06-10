# -*- perl -*-

use Test::More tests => 7;

use_ok( 'Scriptalicious', -progname => "myscript" );

is($PROGNAME, "myscript", "got PROGNAME ok");

my $string;
{
    local(@ARGV) = ("-v", "-s", "foo");
    getopt("string|s=s" => \$string);
}

is($VERBOSE, 1, "Parsed built-in argument");
is($string, "foo", "Parsed custom argument");

$VERBOSE = 0;
( -e "t/testfile" ) && do { unlink("t/testfile")
				|| die "Can't unlink t/testfile; $!" };
run("touch", "t/testfile");
ok( -f "t/testfile", "run()");
unlink("t/testfile");

my ($error, $output) = capture_err("head -5 $0");

is($error, 0, "capture_err() - error code");
is($output, `head -5 $0`, "capture_err() - output");
