
use Scriptalicious;

my $ifd = fileno(STDIN);
my $ofd = fileno(STDOUT);

getopt("ifd|i=i" => sub {
	   close STDIN;
	   open STDIN, "<&$_[1]" or barf "failed to open fd $_[1]; $!";
       },
       "ofd|o=i" => sub {
	   close STDOUT;
	   open STDOUT, ">&$_[1]" or barf "failed to open fd $_[1]; $!";
       },
      );

my $lines = 0;
while ( <STDIN> ) {
    $lines++;
    chomp;
    say "got `$_'";
}

say "saw $lines line(s) on input";

close STDIN;
