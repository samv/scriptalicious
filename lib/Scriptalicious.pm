
package Scriptalicious;

use 5.006;
use strict;
use warnings;
use Carp qw(croak);

our $VERSION = "1.03";

=head1 NAME

Scriptalicious - Delicious scripting goodies

=head1 SYNOPSIS

 use Scriptalicious
      -progname => "pu";
 
 our $VERSION = "1.00";
 
 my $url = ".";
 getopt getconf("u|url" => \$url);
 
 run("echo", "doing something with $url");
 my $output = capture("svn", "info", $url);
 
 __END__
 
 =head1 NAME
 
 pu - an uncarved block of wood
 
 =head1 SYNOPSIS
 
 pu [options] arguments
 
 =head1 DESCRIPTION
 
 This script's function is to be a blank example that many
 great and simple scripts may be built upon.
 
 Remember, you cannot carve rotten wood.
 
 =head1 COMMAND LINE OPTIONS
 
 =over
 
 =item B<-h, --help>
 
 Display a program usage screen and exit.
 
 =item B<-V, --version>
 
 Display program version and exit.
 
 =item B<-v, --verbose>
 
 Verbose command execution, displaying things like the
 commands run, their output, etc.
 
 =item B<-q, --quiet>
 
 Suppress all normal program output; only display errors and
 warnings.
 
 =item B<-d, --debug>
 
 Display output to help someone debug this script, not the
 process going on.
 
 =back

=head1 DESCRIPTION

This module helps you write scripts, quickly.  Just include the above
as a template.  Unfortunately, it is not possible to have a `use'
dependency automatically add structure to your POD yet, so you have to
include the above manually.  If you want your help message to be
meaningful, that is.

To avoid all that unnecessary explicit importing of symbols, the
following symbols and functions are exported into the caller's
namespace:

=over

=item C<$VERBOSE>

Set to 0 by default, and 1 if C<-v> or C<--verbose> was found during
the call to C<getopt()>.  Extra C<-v>'s or C<--debug> will push this
variable higher.  If C<-q> or <--quiet> is specified, this will be
less than one.

=item C<$PROGNAME>

It is recommended that you only ever read this variable, and pass it
in via the import.  This is not automatically extracted from the POD
for performance reasons.

=cut

use Getopt::Long;
use base qw(Exporter);

BEGIN {
    our @EXPORT = qw(say mutter whisper abort moan barf run run_err
		     capture capture_err getopt $VERBOSE $PROGNAME
		     start_timer show_delta show_elapsed getconf
		     getconf_f sci_unit);
}


our ($VERBOSE, $closure);
$VERBOSE = 0;

#---------------------------------------------------------------------
#  parse import arguments and export symbols
#---------------------------------------------------------------------
sub import {
    my $pkg = shift;
    no strict 'refs';

    # look for options in the importer arguments
    for ( my $i = 0; $i < $#_; $i++ ) {
	if ( $_[$i] =~ m/^-(.*)/ ) {
	    die "Bad option `$1' from $pkg"
		unless *{uc($1)}{SCALAR};
	    my $x = uc($1); ($x eq "VERSION") && ($x="main::$x");
	    ${$x} = $_[$i+1];
	    (@_) = (@_[0..($i-1)], @_[($i+2)..$#_]);
	    $i--;
	}
    }

    unshift @_, $pkg;
    goto &Exporter::import;
}

# automatically grok the program name if called for
(our $PROGNAME = $0) =~ s{.*/}{} unless $PROGNAME;

=item B<getopt(@getopt_args)>

Fetch arguments via C<Getopt::Long::GetOptions>.  The C<bundling>
option is enabled by default - which differs from the standard
configuration of B<Getopt::Long>.  To alter the configuration, simply
call C<Getopt::Long::config>.  See L<Getopt::Long> for more
information.

=cut

BEGIN {
    Getopt::Long::config("bundling", "pass_through");
}

END { $closure->() if $closure }

sub getopt {

    local($closure) = \&show_usage;

    Getopt::Long::GetOptions
	    (
	     'help|h' => \&show_help,
	     'verbose|v' => sub { $VERBOSE++ },
	     'quiet|q' => sub { $VERBOSE = -1 },
	     'debug|d' => sub { $VERBOSE = 2 },
	     'version|V' => \&show_version,
	     @_,
	    );

    # check for unknown arguments and print a nice error message
    # instead of the nasty default Getopt::Long message

    shift @ARGV, return if $#ARGV >= 0 and $ARGV[0] eq "--";

    abort("unrecognised option: $ARGV[0]")
	if $#ARGV >= 0 and $ARGV[0] =~ m/^-/;
}

=item B<getconf(@getopt_args)>

Fetches configuration, takes arguments in the same form as
B<getopt()>..

The configuration file is expected to be in F<~/.PROGNAMErc>,
F</etc/perl/PROGNAME.conf>, or F</etc/PROGNAME.conf>.  Only the first
found file is read, and unknown options are ignored for the time
being.

The file is expected to be in YAML format, with the top entity being a
hash, and the keys of the hash being the same as specifying options on
the command line.  Using YAML as a format allows some simplificiations
to getopt-style processing - C<=s%> and C<=s@> style options are
expected to be in a real hash or list format in the config file, and
boolean options must be set to C<true> or C<false> (or some common
equivalents).

Returns the configuration file as Load()'ed by YAML in scalar context,
or the argument list it was passed in list context.

For example, this script should work As You'd Expect(tm):

  getopt getconf
      ( "something|s" => \$foo,
        "invertable|I!" => \$invertable,
        "integer|i=i" => \$bar,
        "string|s=s" => \$cheese,
        "list|l=s@" => \@list,
        "hash|H=s%" => \%hash, );

Examples of valid command lines for such a script:

  foo.pl --something
  foo.pl --invertable
  foo.pl --no-invertable    <=== FORM DIFFERS IN CONFIG FILE
  foo.pl --integer=7
  foo.pl --string=anything
  foo.pl --list one --list two --list three
  foo.pl --hash foo=bar --hash baz=cheese

Equivalent config files:

  something: 1

  invertable: on

  invertable: off

  integer: 7

  string: anything

  list:
    - one
    - two
    - three

  list: [ one, two, three ]

  hash:
    foo: bar
    baz: cheese

Note that more complex and possibly arcane usages of Getopt::Long
features may not work with getconf (patches welcome).

=item B<getconf_f($filename, @getopt_args)>

As B<getconf()>, but specify a filename.

=cut

=item B<say "something">

Prints a message to standard output, unless quiet mode (C<-q> or
C<--quiet>) was specified.  For normal program messages.

=item B<mutter "progress">

Prints a message to standard output, if verbose mode (C<-v>) or debug
mode (C<-d>) is enabled (ie, if C<$VERBOSE E<gt> 0>).  For messages
designed to help a I<user of the script> to see more information about
what is going on.

=item B<whisper "detail">

Prints a message to standard output, if debug mode (C<-d>) is enabled
or multiple verbose options were passed (ie, if C<$VERBOSE E<gt> 1>).
For messages designed to help a I<person debugging the script> to see
more information about what is going on internally to the script.

=item B<abort "won't go to sea in a storm">

Prints a short program usage message (extracted from the POD synopsis)
and exits with an error code.

=item B<moan "weather is miserable">

Prints a warning to standard error.  It is preceded with the text
C<warning:>.  The program does not exit.

=item B<barf "hit an iceberg">

Prints a warning to standard error.  It is preceded with the text
C<warning:>.  The program does not exit.

=cut

sub say { print "$PROGNAME: @_\n" unless $VERBOSE < 0 }
sub mutter { say @_ if $VERBOSE }
sub whisper { say @_ if $VERBOSE > 1 }
sub _err_say { print STDERR "$PROGNAME: @_\n" }
sub abort { _err_say "aborting: @_"; &show_usage; }
sub moan { _err_say "warning: @_" }
sub barf { if($^S){die"@_"}else{ _err_say "ERROR: @_"; exit(1); } }

sub getconf {
    my $conf_obj;
    eval 'use YAML'; barf "failed to include YAML; $@" if $@;
    for my $loc ( "$ENV{HOME}/.${PROGNAME}rc",
		  "/etc/perl/$PROGNAME.conf",
		  "/etc/$PROGNAME.conf",
		  "POD"
		) {
	
	eval {
	    $conf_obj = getconf_f($loc);
	};
	if ( $@ ) {
	    if ( $@ =~ /^no such config/ ) {
		next;
	    } else {
		barf "error processing config file $loc; $@";
	    }
	} else {
	    last;
	}
    }
    if ( wantarray ) {
	return @_;
    } else {
	return $conf_obj;
    }
}

sub getconf_f {
    my $filename = shift;
    eval 'use YAML'; barf "failed to include YAML; $@" if $@;

    my $conf_obj;

    if ( $filename eq "POD" ) {
	eval "use Pod::Constants";
	barf "no such config file <POD>" if $@;

	my $conf;
	Pod::Constants::import_from_file
		($0, "DEFAULT_CONFIG_FILE" => \$conf);
	$conf or barf "no such config section";
	eval { $conf_obj = YAML::Load($conf) };

    } else {
	barf "no such config file $filename" unless -f $filename;

	open CONF, "<$filename"
	    or barf "failed to open config file $filename; $!";
	whisper "about to set YAML on config file $filename";
	eval { $conf_obj = YAML::Load(join "", <CONF>); };
	close CONF;
    }
    barf "YAML exception parsing config file $filename: $@" if $@;
    whisper "YAML on config file $filename complete";

    return _process_conf($filename, $conf_obj, @_);
}

sub _process_conf {
    my $filename = shift;
    my $conf_obj = shift;
    my @save__ = @_ if wantarray;
    while ( my ($opt, $target) = splice @_, 0, 2 ) {

	# wheels, reinvented daily, around the world.
	my ($opt_list, $type) = ($opt =~ m{^([^!+=:]*)([!+=:].*)?$});
	$type ||= "";
	my @names = split /\|/, $opt_list;

	for my $name ( @names ) {
	    if ( exists $conf_obj->{$name} ) {
		whisper "found config option `$name'";

		my $val = $conf_obj->{$name};

		# if its a hash or a list, don't beat around the bush,
		# just assign it.
		if ( $type =~ m{\@$} ) {
		    ref $target eq "ARRAY" or
			croak("$opt: list options must be assigned "
			      ."to an array ref, not `$target'");

		    ref $val eq "ARRAY"
			or barf("list specified in config options, "
				."but `$val' found in config file "
				." $filename for option $name"
				.($name ne $names[0]
				  ? " (synonym for $names[0])" : ""));
		    @{$target} = @{$val};
		    last;
		}
		elsif ( $type =~ m{\%$} ) {
		    ref $target eq "HASH" or
			croak("$opt: hash options must be assigned "
			      ."to a hash ref, not `$target'");

		    ref $val eq "HASH"
			or barf("hash specified in config options, "
				."but `$val' found in config file "
				." $filename for option $name"
				.($name ne $names[0]
				  ? " (synonym for $names[0])" : ""));
		    %{$target} = %{$val};
		    last;
		}

		# check its type
		elsif ( $type =~ m{^=s} ) {
		    # nominally a string, but actually allow anything.
		}
		elsif ( $type =~ m{^=i} ) {
		    $val =~ m/^\d+$/ or barf
			("option `$name' in config file $filename "
			 ."must be an integer, not `$val'");
		}
		elsif ( $type =~ m{^=f} ) {
		    $val =~ m/^[+-]?(\d+\.?|\d*\.)(\d+)/ or barf
			("option `$name' in config file $filename "
			 ."must be a real number, not `$val'");
		    $val += 0;
		}
		elsif ( $type =~ m{!} ) {

		    my ($is_true, $is_false) =
			($val =~ m/^(?:(y|yes|true|on|1|yang)
				  |(n|no|false|off|0|yin|))$/xi)
			    or barf
			("option `$name' in config file $filename "
			 ."must be yin or yang, not a suffusion of "
			 ."yellow");

		    $val = $is_true ? 1 : 0;

		} else {
		    $val = 1;
		}

		# process it
		croak("$opt: simple options must be assigned "
		      ."to a scalar or code ref, not `$target'")
		    unless (ref $target and
			    (ref $target)=~ /CODE|SCALAR|REF/);

		if ( ref $target eq "CODE" ) {
		    $target->($names[0], $val);
		} else {
		    $$target = $val;
		}

		last;
	    }
	}
    }

    if ( wantarray ) {
	return @save__;
    } else {
	return $conf_obj
    }
}

#---------------------------------------------------------------------
#  helpers for running commands and/or capturing their output
#---------------------------------------------------------------------
our (@output, $next_cmd_no_hide, $next_cmd_capture);

# use Shell::QuoteEscape?  nah :-)
my %map = ((map { chr($_) => sprintf("\\%.3o",$_) } (0..31, 127..255)),
           " "=>" ","\t"=>"\\t","\r"=>"\\r","\n"=>"\\n",
           "\""=>"\\\"");
sub shellquote {
    return join(" ",map { m/[\s\']/ && do {
        s/[\0-\031"\s\177-\377]/$map{$&}/eg;
        $_ = "\"$_\"";
    }; $_ } map { $_ } @_);
}

=item B<run("command", "arg1", "arg2")>

Runs a command or closure, barf's with a relevant error message if
there is a problem.  Program output is suppressed unless running in
verbose mode.

=cut

sub run {
    &run_err(@_);
    my $start = $#output - 10;
    chomp($output[$#output]) if @output;
    $start = 0 if $start < 0;
    barf(
         (ref $_[0] ? "Sub-process " : "Command `".shellquote(@_)."' ").
         (($? >> 8)
          ? "exited with error code ".($?>>8)
          : "killed by signal $?")
         .(($VERBOSE >= 1 or $next_cmd_no_hide) ? ""
           : (($start != 0
	       ? "\nlast lines of output:\n"
	       : "\nprogram output:\n")
              .join("", @output[$start .. $#output])
	      .($start != 0
		? "(use -v to show complete program output)"
		: "")))
        ) if ($?);
}

sub do_fork {
    @output = ();
    if (not $next_cmd_capture and
	( $VERBOSE >= 1 or $next_cmd_no_hide )) {
        return fork()
    } else {
        my $pid = open CHILD, "-|";
        if (defined($pid) && !$pid) {
            open STDERR, ">&STDOUT";
        }
        return $pid;
    }
}

sub _waitpid {
    my $pid = shift;

    if (not $next_cmd_capture and
	($VERBOSE >= 1 or $next_cmd_no_hide)) {
        waitpid($pid, 0);
    } else {
        while (<CHILD>) {
            push @output, $_;
        }
        close CHILD;
    }
}

=item B<run_err("command", "arg2", "arg1")>

Same as run, but returns the error code rather than assuming that the
command will successfully complete.  Again, output it suppressed.

=cut

sub _load_hires {
    return if defined &gettimeofday;
    eval "use Time::HiRes qw(gettimeofday tv_interval)";
    *gettimeofday = sub { return time() }
	unless defined &gettimeofday;
    *tv_interval = sub { return ${$_[0]} - ${$_[1]} }
	unless defined &tv_interval;
}

sub run_err {
    mutter("running `".shellquote(@_)."'"
	   .($next_cmd_capture
	     ? " (captured)"
	     : "")) unless ref($_[0]);
    _load_hires;

    my $start = start_timer();
    my $output;

    if (my $pid = do_fork) {

        local $SIG{INT} = sub { kill 2, $pid };
        $output = &_waitpid($pid);

    } else {
        barf "Fork failed; $!" if not defined $pid;
        if (ref $_[0]) {
            my $code = shift;
            $code->(@_);
            exit(0);
        } else {
            exec(@_) ||
            barf "exec failed; $!";
        }
    }
    mutter sprintf("command completed in ".show_elapsed($start))
	if $VERBOSE > 0;

    return $?

}

=item B<capture("command", "1gra", "2gra")>

runs a command, capturing its output, barfs if there is a problem.
Returns the output of the command as a list.

=cut

sub capture {
    local($next_cmd_capture) = 1;
    run(@_);
    return @output;
}

=item B<capture_err("command", "foo")>

Works as B<capture>, but the first returned item is the error code of
the command ($?) rather than the first line of its output.

Usage:

   my ($rc, @output) = capture_err("somecommand", @args);

=cut

sub capture_err {
    local($next_cmd_capture) = 1;
    my $rv = run_err(@_);
    return ($rv, @output)
}

=item B<capture2("command", "--opt")>

Like B<capture>, but returns two strings - one the standard output
stream of the program, and one the standard error.  Normally the two
streams are combined.  Currently unimplemented - contact the author to
contribute an implementation.

=cut

sub capture2 {
    die "capture2 not implemented yet"
}

=item B<start_timer()>

=item B<show_delta()>

=item B<show_elapsed()>

These three little functions are for printing run times in your
scripts.  Times are displayed for running external programs with
verbose mode normally, but this will let you display running times for
your main program easily.

=item B<sci_unit($num, [$unit, $precision])>

Returns a number, scaled using normal scientific prefixes (from atto
to exa).  Optionally specify a precision which is passed to sprintf()
(see L<perldoc/sprintf>).  The default is three significant figures.

The scripts assumes an ISO-8559-1 encoding on output, and so will
print a MU character (\265) to mean micro.

=item B<foo()>

If you've got a short little Perl function that implements something
useful for people writing Shell scripts in Perl, then please feel free
to contribute it.  And if it really is scriptalicious, you can bet
your momma on it getting into this module!

=back

=head1 SEE ALSO

Simon Cozen's L<Getopt::Auto> module does a very similar thing to this
module, in a quite different way.  However, it is missing C<say>,
C<run>, etc.  So you'll need to use some other module for those.  But
it does have some other features you might like and is probably
engineered better.

There's a template script at L<Getopt::Long/Documentation and help
texts> that contains a script template that demonstrates what is
necessary to get the basic man page / usage things working with the
traditional L<Getopt::Long> and L<Pod::Usage> combination.

L<Getopt::Plus> is a swiss army chainsaw of Getopt::* style modules,
contrasting to this module's approach of elegant simplicity (quiet in
the cheap seats!).

If you have solved this problem in a new and interesting way, or even
rehashed it in an old, boring and inelegant way and want your module
to be listed here, please contact the

=head1 AUTHOR

Sam Vilain, samv@cpan.org

=cut

our ($AUTOLOAD, $l);sub AUTOLOAD{croak"No such function $AUTOLOAD"if
$l;(undef,my($f,$n))=ll();$n+=2;my$es="";while(<DATA>){$es.=$_;}close
DATA;eval"# line $n \"$f\"\n$es";$@&&die"Error in autoload: $@";
$l=1;goto &{$AUTOLOAD};}sub ll{sub{caller()}->();}     "P E A C E";
__DATA__

our ($NAME, $SHORT_DESC, $SYNOPSIS, $DESCRIPTION, @options);

#---------------------------------------------------------------------
#  calls Pod::Constants to get the synopsis, etc, from the calling
#  script.
#---------------------------------------------------------------------
sub _get_pod_usage {
    return if $SYNOPSIS;
    (undef, my ($fn, $line)) = sub{caller()}->();
    eval "# line $line \"$fn\"\npackage main;\n".q{
        our $level;
        use Pod::Constants;
        Pod::Constants::import_from_file($0, -trim => 1,
            'NAME' => sub {
	my @m;
	( @m = m/(\S+) - (.*)/ ) &&
	    do { $Scriptalicious::PROGNAME = $m[0];
		 $Scriptalicious::SHORT_DESC = $m[1]; }
	},
            'SYNOPSIS' => \$Scriptalicious::SYNOPSIS,
            'DESCRIPTION' => \$Scriptalicious::DESCRIPTION,
            'COMMAND LINE OPTIONS' => sub {
	        &Pod::Constants::add_hook
		   ('*item' => sub {
                     return unless $level == 1;
		     my ($switches, $description) =
			 m/^(.*?)\n\n(.*)/s;
                     $switches =~ s{[BCI]<([^>]*)>}{$1}g;
		     my (@switches, $longest);
		     $longest = "";
		     for my $switch
			 ($switches =~ m/\G
					 ((?:-\w|--\w+))
					 (?:,\s*)?
					 /gx) {
			     push @switches, $switch;
			     if ( length $switch > length $longest) {
				 $longest = $switch;
			     }
			 }
		     $longest =~ s/^-*//;
		     push @options,
			 $longest, {
				    options => \@switches,
				    description => $description,
				   };
                     });
                &Pod::Constants::add_hook
                   ("*over" => sub { $level++ });
                &Pod::Constants::add_hook
                   ("*back" => sub {
                       --$level or do {
                           &Pod::Constants::delete_hook($_)
                               foreach qw(*over *back *item);
                       };
                    });
            }
        );
    };

    if ( $@ ) {
	$SYNOPSIS = "(error: Pod::Constants failed to load)";
    } else {
	foreach ( $SYNOPSIS, $SHORT_DESC, $DESCRIPTION ) {
	    $_ ||= "(no text found, no POD given?)";
	}
    }
}

sub short_usage {
    _get_pod_usage;
    return ("Usage: $SYNOPSIS\n"
	    ."Try "
	    .($SHORT_DESC
	      ? "`$PROGNAME --help' for a summary of options."
	      : "`perldoc $0' for more information")
	    ."\n");
}

sub usage {
    _get_pod_usage;
    if ( !$SHORT_DESC ) {
	moan("failed to extract usage information from POD; calling "
	    ."perldoc");
	exec("perldoc", $0) ||
	barf "exec failed; $!";
    }

    eval "use Text::Wrap qw(wrap fill)";
    *wrap = sub { return join "", @_ } unless defined &wrap;
    *fill = sub { return join "", @_ } unless defined &fill;

    my $TOTAL_WIDTH;
    eval "use Term::ReadKey;";
    if ( defined &GetTerminalSize ) {
	$TOTAL_WIDTH = (GetTerminalSize())[0] - 10;
    }
    $TOTAL_WIDTH ||= 70;

    my $options_string;
    my $OPTIONS_INDENT = 2;
    my $OPTIONS_WIDTH = 20;
    my $OPTIONS_GAP = 2;

    my $DESCRIPTION_WIDTH = ($TOTAL_WIDTH - $OPTIONS_GAP -
			     $OPTIONS_INDENT - $OPTIONS_WIDTH);

    # go through each option, and format it for the screen

    for ( my $i = 0; $i < (@options>>1); $i ++ ) {
	my $option = $options[$i*2 + 1];

	$Text::Wrap::huge = "overflow";
	$Text::Wrap::columns = $OPTIONS_WIDTH;
	my @lhs = map { split /\n/ }
	    wrap("","",join ", ",
		 sort { length $a <=> length $b }
		 @{$option->{options}});

	$Text::Wrap::huge = "wrap";
	$Text::Wrap::columns = $DESCRIPTION_WIDTH;
	my @rhs = map { split /\n/ }
	    fill("","",$option->{description});

	while ( @lhs or @rhs ) {
	    my $left = shift @lhs;
	    my $right = shift @rhs;
	    $left ||= "";
	    $right ||= "";
	    chomp($left);
	    $options_string .= join
		("",
		 " " x $OPTIONS_INDENT,
		 $left . (" " x ($OPTIONS_WIDTH - length $left)),
		 " " x $OPTIONS_GAP,
		 $right,
		 "\n");
	}
    }

    $Text::Wrap::huge = "overflow";
    $Text::Wrap::columns = $TOTAL_WIDTH;

    $DESCRIPTION =~ s{\n\n}{\n\n<-->\n\n}gs;
    $DESCRIPTION = fill("  ", " ", $DESCRIPTION);
    $DESCRIPTION =~ s{^.*<-->.*$}{}mg;

    return (fill("","",$PROGNAME . " - " . $SHORT_DESC)
	    ."\n\n"
	    ."Usage: ".$SYNOPSIS."\n\n"
	    .$DESCRIPTION."\n\n"
	    .fill("","  ","Command line options:")
	    ."\n\n"
	    .$options_string."\n"
	    ."See `perldoc $0' for more information.\n\n");

}

sub show_usage {
    print STDERR &short_usage;
    exit(1);
}

sub show_version {
    print "This is ".$PROGNAME.", "
	.( defined($main::VERSION)
	   ? "version ".$main::VERSION."\n"
	   : "with no version, so stick it up your source repository!\n" );

    exit(0);
}

sub show_help {
    print &usage;
    exit(0);
}

my ($start, $last);
sub start_timer {
    _load_hires();

    if ( !defined wantarray ) {
	$last = $start = [gettimeofday()];
    } else {
	return [gettimeofday()];
    }
}

sub show_elapsed {
     my $e = tv_interval($_[0]||$start, [gettimeofday()]);

     return sci_unit($e, "s", 3);
}

sub show_delta {
    my $now;
    my $e = tv_interval($_[0]||$last, $now = [gettimeofday()]);
    $last = $now;
    return sci_unit($e, "s", 3);
}

use POSIX qw(ceil);

my %prefixes=(18=>"E",15=>"P",12=>"T",9=>"G",6=>"M",3=>"k",0=>"",
	      -3=>"m",-6=>"\265",-9=>"n",-12=>"p",-15=>"f",-18=>"a");

sub sci_unit {
    my $scalar = shift;
    my $unit = (shift) || "";
    my $d = (shift) || 4;
    my $e = 0;
    #scale value
    while ( abs($scalar) > 1000 ) { $scalar /= 1000; $e += 3; }
    while ( $scalar and abs($scalar) < 1 ) {$scalar*=1000;$e-=3}

    # round the number to the right number of digits with sprintf
    if (exists $prefixes{$e}) {
	$d -= ceil(log($scalar)/log(10));
	$d = 0 if $d < 0;
	my $a = sprintf("%.${d}f", $scalar);
	return $a.$prefixes{$e}.$unit;
    } else {
	return sprintf("%${d}e", $scalar).$unit;
    }

}

