
package Scriptalicious;

use 5.006;
use strict;
use warnings;
use Carp qw(croak);

our $VERSION = "1.09";

use Getopt::Long;
use base qw(Exporter);

BEGIN {
    # export groups, phtoey!

    our @EXPORT = qw(say mutter whisper abort moan barf run run_err
		     capture capture_err getopt $VERBOSE $PROGNAME
		     start_timer show_delta show_elapsed getconf
		     getconf_f sci_unit prompt_for prompt_passwd
		     prompt_yn prompt_Yn prompt_yN prompt_string
		     prompt_int tsay anydump
		    );
}

# define this in subclasses where appropriate
sub __package__ { __PACKAGE__ }

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

# automatically guess the program name if called for
(our $PROGNAME = $0) =~ s{.*/}{} unless $PROGNAME;

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

sub say { print "$PROGNAME: @_\n" unless $VERBOSE < 0 }
sub mutter { say @_ if $VERBOSE }
sub whisper { say @_ if $VERBOSE > 1 }
sub _err_say { print STDERR "$PROGNAME: @_\n" }
sub abort { _err_say "aborting: @_"; &show_usage; }
sub moan { _err_say "warning: @_" }
sub barf { if($^S){die @_}else{ _err_say "ERROR: @_"; exit(1); } }

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

our @last_cmd;
sub run {
    &run_err(@_);
    @_ = @last_cmd;
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

sub _load_hires {
    return if defined &gettimeofday;
    eval "use Time::HiRes qw(gettimeofday tv_interval)";
    *gettimeofday = sub { return time() }
	unless defined &gettimeofday;
    *tv_interval = sub { return ${$_[0]} - ${$_[1]} }
	unless defined &tv_interval;
}

sub run_err {
    my %fds;
    my $fd_desc = "";
    while ( $_[0] and !ref $_[0] and $_[0]=~/^-(in|out|rw)(\d+)?$/ ) {
	shift;
	my $mode = ($1 eq "in" ? "<" : ($1 eq "out" ? ">" : "+<") );
	my $fd = $2 || ($1 eq "out" ? 1 : 0);
	$fds{"$fd"} = [ $mode, shift ];
	$fd_desc .= ($fd_desc ? ", " : "") . "fd$fd=$mode$fds{$fd}";
    }
    @last_cmd = @_;
    mutter("running `".shellquote(@last_cmd)."'"
	   .($next_cmd_capture
	     ? " (captured)"
	     : "")
	   .($fd_desc?"($fd_desc)":"")
	  ) unless ref($_[0]);
    _load_hires;

    my $start = start_timer();
    my $output;

    if (my $pid = do_fork) {

        local $SIG{INT} = sub { kill 2, $pid };
        $output = &_waitpid($pid);

    } else {
        barf "Fork failed; $!" if not defined $pid;
	setup_fds(\%fds) if $fd_desc;
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

sub capture {
    local($next_cmd_capture) = 1;
    run(@_);
    return (wantarray ? @output : join("", @output));
}

sub capture_err {
    local($next_cmd_capture) = 1;
    my $rv = run_err(@_);
    return ($rv, @output)
}

sub capture2 {
    die "capture2 not implemented yet"
}

our $DATA = join "", <DATA>;  close DATA;
our ($AUTOLOAD, $l);sub AUTOLOAD{croak"No such function $AUTOLOAD"if
$l;(undef,my($f,$n))=ll();$n+=1;eval"package ".__PACKAGE__.";\n"
."# line $n \"$f\"\n$DATA"; $@&&die"Error in autoload: $@";
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
	      -3=>"m",-6=>"u",-9=>"n",-12=>"p",-15=>"f",-18=>"a");

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
		($0, "DEFAULT CONFIG FILE" => \$conf);
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

our $term;

sub term {
    #print "PACKAGE is ".__PACKAGE__."\n";
    $term ||= do {
	eval { -t STDIN or die;
	       require Term::ReadLine;
	       Term::ReadLine->new(__PACKAGE__)
	       } || (bless { IN => \*STDIN,
			     OUT => \*STDOUT }, __PACKAGE__);
    };
    #print "TERM is $term\n";
    return $term;
}

sub OUT { $_[0]->{OUT} }
sub IN  { $_[0]->{IN} }

sub readline {
    my $self = shift;
    my $prompt = shift;

    my $OUT = $self->OUT;
    my $IN = $self->IN;

    print $OUT "$prompt? ";
    my $res = readline $IN;
    chomp($res);

    return $res;
}

sub prompt_passwd {
    my $prompt = shift || "Password: ";

    eval {
	require Term::ReadKey;
    };
    barf "cannot load Term::ReadKey" if $@;

    Term::ReadKey::ReadMode('noecho');
    my $passwd;
    eval { $passwd = prompt_regex($prompt, @_) };
    Term::ReadKey::ReadMode('restore');
    die $@ if $@;
    $passwd;
}

sub prompt_regex {
    my $prompt = shift;
    # I'm a whitespace nazi! :)
    $prompt =~ s{$}{ } unless $prompt =~ /\s$/;
    my $sub = shift;
    my $moan = shift;
    while ( defined ($_ = term->readline($prompt)) ) {
	if ( $sub ) {
	    if ( defined(my $res = $sub->($_)) ) {
		return $res;
	    } else {
		moan ($moan || "bad response `$_'");
	    }
	} else {
	    return $_;
	}
    }
    barf "EOF on input";
}

sub prompt_for {
    my $type;
    if (@_ > 1 and $_[0]=~/^-(.*)/) { $type = $1; shift; };
    $type ||= "string";
    my $ref = __package__->can("prompt_$type")
	or croak "don't know how to prompt for $type";

    my $what = shift;
    my $default = shift;
    $ref->( "Value for $what:", $default, ),
}

sub prompt_string {
    my $prompt = shift;
    my $default = shift;
    prompt_regex($prompt.(defined($default)?" [$default]":""),
		 sub { $_ || $default });
}

sub prompt_int {
    my $prompt = shift;
    my $default = shift;
    prompt_regex($prompt.(defined($default)?" [$default]":""),
		 sub { my($i) = /^(\d+)$/;
		       defined ($i) ? $i : (length($_)?undef:$default) });
}

sub prompt_nY { prompt_Yn(@_) }
sub prompt_Yn {
    prompt_regex ($_[0]." [Yn]",
		  sub {( /^\s*(?: (?:(y.*))? | (n.*))\s*$/ix &&
			 ($2 ? 0 : (defined($1) ? 1 : undef)) 
		       )} );
}
sub prompt_yn {
    prompt_regex ($_[0]." [yn]",
		  sub {( /^\s*(?: (y.*) | (n.*))\s*$/ix &&
			 ($2 ? 0 : ($1 ? 1 : undef)) 
		       )},
		  "please enter `yes', or `no'" );
}
sub prompt_Ny { prompt_yN(@_) }
sub prompt_yN {
    prompt_regex ($_[0]." [Ny]",
		  sub {( /^\s*(?: (y.*)? | (?:(n.*))? )\s*$/ix &&
			 ($1 ? 1 : (defined($2) ? 0 : undef)) 
		       )} );
}

no strict 'refs';

# sets up file descriptors for `run' et al.
sub setup_fds {
    my $fdset = shift;

    my (@fds) = sort { $a <=> $b } keys %$fdset;
    $^F = $fds[$#fds] if $fds[$#fds] > 2;

    # there is a slight problem with this - for instance, if the user
    # supplies a closure that is reading from a file, and that file
    # happens to be opened on a filehandle that they want to use, then
    # it will be closed and the code break.  Ho hum.
    for ( 3..$fds[$#fds] ) {
	open BAM, "<&=$_";
	if ( fileno(BAM) ) {
	    close BAM;
	} else {
	    open BAM, ">&=$_";
	    if ( fileno(BAM) ) {
		close BAM;
	    }
	}
    }

    while ( my ($fnum, $spec) = each %$fdset ) {
	my ($mode, $where) = @$spec;
	my $fd;

	if ( !ref $where ) {
	    open($fd, "$mode$where")
		or barf "failed to re-open fd $fnum $mode$where; $!";
	}
	elsif ( ref $where eq "GLOB" ) {
	    open($fd, "$mode&".fileno($where))
		or barf "failed to re-open fd $fnum $mode &fd(".fileno($where)."; $!";
	}
	elsif ( ref $where eq "CODE" ) {
	    pipe(\*{"FD${fnum}_R"}, \*{"FD${fnum}_W"});

	    if ( my $pid = fork ) {
		my $rw = ($mode eq ">" ? "W" : "R");
		open($fd, "$mode&FD${fnum}_$rw")
		    or barf "failed to re-open fd $fnum $mode CODE; $!";
	    } elsif ( !defined $pid ) {
		barf "fork failed; $!";
	    } else {
		if ( $mode eq "<" ) {
		    close STDOUT;
		    open STDOUT, ">&FD${fnum}_W";
		    select STDOUT;
		    $| = 1;
		}
		else {
		    close STDIN;
		    open STDIN, "<&FD${fnum}_R";
		}
		$where->();
		exit(0);
	    }
	}
	else {
	    barf "bad spec for FD $fnum";
	}

	# don't use a lex here otherwise it gets auto-closed
	open (\*{"FD${fnum}"}, "$mode&=$fnum");
	open \*{"FD${fnum}"}, "$mode&".fileno($fd);
	fileno(\*{"FD${fnum}"}) == $fnum
	    or do {
		barf ("tried to setup on FD $fnum, but got "
		      .fileno(\*{"FD$fnum"})."(spec: $mode $where)");
	    };
    }
}

sub tsay {
    my $template = shift;
    my $data = shift;

    eval {
	&templater->process($template, $data);
    };

    if ( $@ ) {
	moan "Error trying template response using template `$template'; $@";
	say "template variables:";
	print anydump $data
    }
}

our $provider;
our $templater;

sub templater {
    $provider ||= bless { }, "Scriptalicious::DataLoad";
    our $templater ||= Scriptalicious::Template->new
	({ INTERPOLATE => 1,
	   POST_CHOMP => 0,
	   EVAL_PERL => 1,
	   TRIM => 0,
	   RECURSION => 1,
	   LOAD_TEMPLATES => [ $provider ],
	 });
}

sub anydump {
    my $var = shift;
    eval {
	eval "use YAML"; die $@ if $@;
	local $YAML::UseHeader = 0
	    unless (!ref $var or ref $var !~ m/^(ARRAY|HASH)$/);
	local $YAML::UseVersion = 0;
	return YAML::Dump($var);
    } || do {
	eval "use Data::Dumper"; die $@ if $@;
	local $Data::Dumper::Purity = 1;
	return Data::Dumper->Dump([$var], ["x"]);
    }
}

package Scriptalicious::Template;

our $template_ok;
our @ISA;

sub new {
    my $class = shift;
    eval "use Template";
    if ( !$@ ) {
	@ISA = qw(Template);
	@Scriptalicious::DataLoad::ISA = qw(Template::Provider);
	$_[0]->{LOAD_TEMPLATES} = Scriptalicious::DataLoad->new();
	$template_ok = 1;
	return $class->SUPER::new(@_);
    } else {
	Scriptalicious::moan "install Template Toolkit for prettier messages";
	return bless shift, $class;
    }
}

sub process {
    my $self = shift;
    if ($template_ok) {
	no strict 'refs';
	Scriptalicious::_get_pod_usage();
	my $template = shift;
	my $vars = shift;
	$vars||={};
	$vars->{$_} = ${"Scriptalicious::$_"}
	    foreach qw(PROGNAME VERSION VERBOSE NAME SYNOPSIS DESCRIPTION);
	return $self->SUPER::process($template, $vars, @_);
    };

    my $template = shift;
    my $vars = shift;
    my $provider = eval { $self->{LOAD_TEMPLATES}[0] }
	|| bless { }, "Scriptalicious::DataLoad";

    my ($data, $rc) = $provider->fetch($template);
    if ( !$rc ) {
	Scriptalicious::say "----- Template `$template' -----";
	print $data;
    }
    Scriptalicious::say "------ Template variables ------";
    print Scriptalicious::anydump $vars;
    Scriptalicious::say "-------- end of message --------";
}

package Scriptalicious::DataLoad;

our @ISA;

sub fetch {
    my ($self, $name, $alias) = @_;

    # get the source file/template
    my $section = shift;

    my $found = 0;
    my @data;
    if ( open(my $script, $0) ) {
	"" =~ m{()};  # clear $1
	while ( <$script> ) {
	    if ( m{^__\Q$name\E__$} .. m{^__(?!\Q$name\E)(\w+)__$} ) {
		$found++ or next;
		next if $1;
		push @data, $_;
	    }
	}
	close $script;
    }
    if ( !$found and -e $name ) {
	$found = 1;
	if (open TEMPLATE, $name) {
	    @data = <TEMPLATE>;
	    close TEMPLATE;
	} else {
	    Scriptalicious::moan "failed to open template `$name' for reading; $!";
	    $found = 0;
	}
    }

    if ( @ISA ) {
	#print STDERR "Returning for template `$name':\n", @data,"...\n";
	return $self->SUPER::fetch(\(join "", @data));
    } else {
	return ((join "", @data), $found ? 0 : 255 );
    }
    #return (, ($found ? $ok : $error) );
}

1;

