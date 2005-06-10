
package Scriptalicious;

use 5.006;
use strict;
use warnings;
use Carp qw(croak);

our $VERSION = "1.02";

=head1 NAME

Scriptalicious - Delicious scripting goodies

=head1 SYNOPSIS

 use Scriptalicious
      -progname => "pu";
 
 our $VERSION = "1.00";
 
 my $url = ".";
 getopt("u|url" => \$url);
 
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

=item B<getopt(@getopt_args)>

This just calls Getopt::Long::GetOptions (see L<Getopt::Long> for
details).  It automatically adds to the arguments you give it some
"standard" command line options.

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
		     capture capture_err getopt $VERBOSE $PROGNAME);
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
sub barf { _err_say "ERROR: @_"; exit(1); }

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
    my $start = [gettimeofday()];
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
    my $finish = [gettimeofday()];
    whisper sprintf("Command completed in %.3fs",
                    tv_interval($start,$finish));
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

