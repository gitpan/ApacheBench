package HTTPD::Bench::ApacheBench;

use strict;
use vars qw($VERSION @ISA);

require DynaLoader;

$HTTPD::Bench::ApacheBench::VERSION = '0.60';
@HTTPD::Bench::ApacheBench::ISA =
  qw(DynaLoader HTTPD::Bench::ApacheBench::Regression);

bootstrap HTTPD::Bench::ApacheBench $VERSION;

##################################################
## the constructor                              ##
##################################################
sub new {
    my ($this, $self) = @_;
    my $class = ref($this) || $this;
    if (ref($self) ne "HASH") {	$self = {} }
    bless $self, $class;
    $self->initialize;
    return $self;
}

##################################################
## initialize defaults                          ##
##################################################
sub initialize {
    my ($self) = @_;
    $self->{runs} = [] if ref $self->{runs} ne "ARRAY";
    $self->{concurrency} = 1 unless $self->{concurrency};
    $self->{repeat} = 1 unless $self->{repeat};
    $self->{priority} = "equal_opportunity" unless $self->{priority};
    $self->{buffersize} = 16384 unless $self->{buffersize};
    $self->{memory} = 1 unless defined $self->{memory};
}


##################################################
## configure the global parameters              ##
##################################################
sub config {
    my ($self, $opt) = @_;
    foreach (qw(concurrency priority buffersize repeat memory)) {
	$self->{$_} = $opt->{$_} if defined $opt->{$_};
    }
}

sub concurrency {
    my ($self, $arg) = @_;
    $self->{concurrency} = $arg if $arg;
    return $self->{concurrency};
}

sub priority {
    my ($self, $arg) = @_;
    $self->{priority} = $arg if $arg;
    return $self->{priority};
}

sub memory {
    my ($self, $arg) = @_;
    $self->{memory} = $arg if defined $arg;
    return $self->{memory};
}

sub repeat {
    my ($self, $arg) = @_;
    $self->{repeat} = $arg if $arg;
    return $self->{repeat};
}

sub buffersize {
    my ($self, $arg) = @_;
    $self->{buffersize} = $arg if $arg;
    return $self->{buffersize};
}

sub total_requests {
    my ($self) = @_;
    return 0 unless ref $self->{runs} eq "ARRAY";
    my $total = 0;
    foreach my $run (@{$self->{runs}}) {
	my $repeat = $run->{repeat} ? $run->{repeat} : $self->{repeat};
	$total += ($#{$run->{urls}} + 1) * $repeat
	  if ref $run->{urls} eq "ARRAY";
    }
    return $total;
}


##################################################
## verify configuration of runs and execute     ##
##################################################
sub execute {
    my ($self) = @_;
    my %altered;

    # fail if they have not added any runs
    return undef unless ref $self->{runs} eq "ARRAY";

    # pre execute initialization of each run
    foreach my $run_no (0..$#{$self->{runs}}) {
	return undef if ref $self->{runs}->[$run_no]->{urls} ne "ARRAY";
	my $runobj = $self->{runs}->[$run_no];
	unless ($runobj->{repeat}) {
	    $runobj->{repeat} = $self->{repeat};
	    $altered{$run_no}->{repeat} = 1;
	}
	unless (defined $runobj->{memory}) {
	    $runobj->{memory} = $self->{memory};
	    $altered{$run_no}->{memory} = 1;
	}
	foreach my $param (qw(postdata content_types)) {
	    $runobj->{$param} = [ map {undef} @{$runobj->{urls}} ]
	      if ref $runobj->{$param} ne "ARRAY";
	}
    }

    # call the XS code and store regression data
    $self->{'regression'} = $self->ab;

    # post execute polishing of each run
    foreach my $run_no (0..$#{$self->{runs}}) {
	my $runobj = $self->{runs}->[$run_no];
	$runobj->{'run_no'} = $run_no;
	$runobj->{'regression'} = $self->{'regression'};
	foreach my $param (qw(repeat memory)) {
	    delete $runobj->{$param} if
	      (ref $altered{$run_no} and $altered{$run_no}->{$param});
	}
    }
    return HTTPD::Bench::ApacheBench::Regression->new
      ({ 'regression' => $self->{'regression'} });
}


##################################################
## run accessors                                ##
##################################################
sub run {
    my ($self, $run_no, $run) = @_;
    return undef unless
      (ref $self->{runs} eq "ARRAY" &&
       ref $self->{runs}->[$run_no] eq "HTTPD::Bench::ApacheBench::Run");
    if (ref $run eq "HTTPD::Bench::ApacheBench::Run") {
	my $replaced_run = $self->{runs}->[$run_no];
	$self->{runs}->[$run_no] = $run;
	return $replaced_run;
    }
    return $self->{runs}->[$run_no];
}

sub add_run {
    my ($self, $newrun) = @_;
    return undef unless (ref $self->{runs} eq "ARRAY" and
			 ref $newrun eq "HTTPD::Bench::ApacheBench::Run");
    push(@{$self->{runs}}, $newrun);
    return $#{$self->{runs}};
}

sub delete_run {
    my ($self, $run_no) = @_;
    return undef unless ref $self->{runs} eq "ARRAY";
    my $deleted_run = $self->{runs}->[$run_no];
    $self->{runs} = [ @{$self->{runs}}[0..$run_no-1],
		      @{$self->{runs}}[$run_no+1..$#{$self->{runs}}] ];
    return $deleted_run;
}



package HTTPD::Bench::ApacheBench::Run;

use strict;
use vars qw($VERSION @ISA);

$HTTPD::Bench::ApacheBench::Run::VERSION = $HTTPD::Bench::ApacheBench::VERSION;
@HTTPD::Bench::ApacheBench::Run::ISA =
  qw(HTTPD::Bench::ApacheBench::Regression);

sub new {
    my ($this, $self) = @_;
    my $class = ref($this) || $this;
    if (ref($self) ne "HASH") {	$self = {} }
    bless $self, $class;
    $self->initialize;
    return $self;
}

sub initialize {
    my ($self) = @_;
    $self->{order} = "breadth_first" unless $self->{order};
    $self->{cookies} = [undef] unless ref $self->{cookies} eq "ARRAY";
}


sub repeat {
    my ($self, $arg) = @_;
    $self->{repeat} = $arg if $arg;
    return $self->{repeat};
}

sub order {
    my ($self, $arg) = @_;
    $self->{order} = $arg if $arg;
    return $self->{order};
}

sub memory {
    my ($self, $arg) = @_;
    $self->{memory} = $arg if defined $arg;
    return $self->{memory};
}

sub buffersize {
    my ($self, $arg) = @_;
    $self->{buffersize} = $arg if $arg;
    return $self->{buffersize};
}

sub cookies {
    my ($self, $arg) = @_;
    $self->{cookies} = $arg if $arg;
    return $self->{cookies};
}

sub urls {
    my ($self, $arg) = @_;
    $self->{urls} = $arg if $arg;
    return $self->{urls};
}

sub postdata {
    my ($self, $arg) = @_;
    $self->{postdata} = $arg if $arg;
    return $self->{postdata};
}

sub content_types {
    my ($self, $arg) = @_;
    $self->{content_types} = $arg if $arg;
    return $self->{content_types};
}

sub append {
    my ($self, $opt) = @_;
    my @postdata;
    if (ref $opt->{postdata} eq "ARRAY") {
	@postdata = @{$opt->{postdata}};
    } else {
	@postdata = map {undef} @{$opt->{urls}};
    }
    push(@{$self->{urls}}, @{$opt->{urls}});
    push(@{$self->{postdata}}, @postdata);
}

sub total_requests {
    my ($self) = @_;
    return 0 unless (ref $self eq "HTTPD::Bench::ApacheBench::Run" and
		     ref $self->{urls} eq "ARRAY" and $self->{repeat});
    return ($#{$self->{urls}} + 1) * $self->{repeat};
}

sub iteration {
    my ($self, $iter_no) = @_;
    $iter_no = 0 unless defined $iter_no;
    return HTTPD::Bench::ApacheBench::Regression->new
      ({ 'regression'  => $self->{'regression'},
	 'run_no'      => $self->{'run_no'},
	 'iter_no'     => $iter_no });
}



package HTTPD::Bench::ApacheBench::Regression;

use strict;
use vars qw($VERSION);

$HTTPD::Bench::ApacheBench::Regression::VERSION =
  $HTTPD::Bench::ApacheBench::VERSION;

sub new {
    my ($this, $self) = @_;
    my $class = ref($this) || $this;
    if (ref($self) ne "HASH") {	$self = {} }
    bless $self, $class;
    return $self;
}

sub get_regression_hash {
    my ($self) = @_;
    return
      (ref $self->{'regression'} eq "HASH" ? $self->{'regression'} : undef);
}

sub run {
    my ($self, $run_no) = @_;
    $self->{'run_no'} = $run_no if defined $run_no;
    return $self;
}

sub iteration {
    my ($self, $iter_no) = @_;
    $self->{'iter_no'} = $iter_no
      if defined $self->{'run_no'} and defined $iter_no;
    return $self;
}

##################################################
## regression data accessors                    ##
##################################################
sub total_requests_sent {
    my ($self) = @_;
    # temporary hack, until {completed,failed}_requests works in ApacheBench.xs
    return undef unless $self->get_regression_hash;
    return $#{$self->{urls}} + 1
      if (defined $self->{'run_no'} and ref $self->{urls} eq "ARRAY");
    my $total = 0;
    foreach my $run (@{$self->{runs}}) {
	my $repeat = $run->{repeat} ? $run->{repeat} : $self->{repeat};
	$total += ($#{$run->{urls}} + 1) * $repeat
	  if ref $run->{urls} eq "ARRAY";
    }
    return $total;
}

sub total_responses_received {
    my ($self) = @_;
    return $self->total_requests_sent;
}

sub total_time {
    my ($self) = @_;
    return undef unless (my $reg = $self->get_regression_hash and
			 !defined $self->{'run_no'});
    return $reg->{'total_time'};
}

sub bytes_received {
    my ($self) = @_;
    return undef unless (my $reg = $self->get_regression_hash and
			 !defined $self->{'run_no'});
    return $reg->{'bytes_received'};
}

sub warnings {
    my ($self) = @_;
    return undef unless (my $reg = $self->get_regression_hash and
			 !defined $self->{'run_no'});
    return $reg->{'warnings'};
}


sub iteration_value {
    my ($self, $value, $expect_ref) = @_;
    return undef unless (my $reg = $self->get_regression_hash);
    return undef unless defined $self->{'run_no'};
    my $iter_no = defined $self->{'iter_no'} ? $self->{'iter_no'} : 0;
    my $iter = $reg->{'run'.$self->{'run_no'}}->[$iter_no];
    return undef unless (ref $iter eq "HASH" and
			 (!$expect_ref or ref $iter->{$value} eq $expect_ref));
    return $iter->{$value};
}


sub connect_times {
    my ($self) = @_;
    return $self->iteration_value('connect_time', "ARRAY");
}

sub min_connect_time {
    my ($self) = @_;
    return $self->iteration_value('min_connect_time');
}

sub max_connect_time {
    my ($self) = @_;
    return $self->iteration_value('max_connect_time');
}

sub avg_connect_time {
    my ($self) = @_;
    return $self->iteration_value('average_connect_time');
}

sub sum_connect_time {
    my ($self) = @_;
    return $self->iteration_value('total_connect_time');
}

sub request_times {
    my ($self) = @_;
    return $self->iteration_value('request_time', "ARRAY");
}

sub min_request_time {
    my ($self) = @_;
    return $self->iteration_value('min_request_time');
}

sub max_request_time {
    my ($self) = @_;
    return $self->iteration_value('max_request_time');
}

sub avg_request_time {
    my ($self) = @_;
    return $self->iteration_value('average_request_time');
}

sub sum_request_time {
    my ($self) = @_;
    return $self->iteration_value('total_request_time');
}

sub response_times {
    my ($self) = @_;
    return $self->iteration_value('response_time', "ARRAY");
}

sub min_response_time {
    my ($self) = @_;
    return $self->iteration_value('min_response_time');
}

sub max_response_time {
    my ($self) = @_;
    return $self->iteration_value('max_response_time');
}

sub avg_response_time {
    my ($self) = @_;
    return $self->iteration_value('average_response_time');
}

sub sum_response_time {
    my ($self) = @_;
    return $self->iteration_value('total_response_time');
}

sub bytes_posted {
    my ($self) = @_;
    return $self->iteration_value('bytes_posted', "ARRAY");
}

sub sum_bytes_posted {
    my ($self) = @_;
    return $self->iteration_value('total_bytes_posted');
}

sub bytes_read {
    my ($self) = @_;
    return $self->iteration_value('bytes_read', "ARRAY");
}

sub sum_bytes_read {
    my ($self) = @_;
    return $self->iteration_value('total_bytes_read');
}

sub response_headers {
    my ($self) = @_;
    return $self->iteration_value('headers', "ARRAY");
}

sub response_body {
    my ($self) = @_;
    return $self->iteration_value('page_content', "ARRAY");
}

sub response_body_lengths {
    my ($self) = @_;
    return $self->iteration_value('doc_length', "ARRAY");
}



1;

__END__

=head1 NAME

HTTPD::Bench::ApacheBench - Perl API for Apache benchmarking and regression testing.

=head1 SYNOPSIS

  use HTTPD::Bench::ApacheBench;

  my $b = HTTPD::Bench::ApacheBench->new;

  # global configuration
  $b->concurrency(5);
  $b->priority("run_priority");

  # add HTTP request sequences (aka: runs)
  my $run1 = HTTPD::Bench::ApacheBench::Run->new
    ( urls => ["http://localhost/one", "http://localhost/two"] );
  $b->add_run($run1);

  my $run2 = HTTPD::Bench::ApacheBench::Run->new
    ( urls    => ["http://localhost/three", "http://localhost/four"],
      cookies => ["Login_Cookie=b3dcc9bac34b7e60;"],
      order   => "depth_first",
      repeat  => 10,
      memory  => 2 );
  $b->add_run($run2);

  # send HTTP request sequences to server and time responses
  my $ro = $b->execute;

  # calculate hits/sec
  print (1000*$b->total_requests/$b->total_time)." req/sec\n";

  # show request times (in ms) for $run1, 1st repetition
  print join(', ', @{$run1->request_times}) . "\n";

  # show response times (in ms) for $run2, 7th repetition
  print join(', ', @{$run2->iteration(6)->response_times}) . "\n";

  # dump the entire regression object (WARNING, this could be a LOT OF DATA)
  use Data::Dumper;
  my $d = Data::Dumper->new([$ro]);
  print $d->Dumpxs;


=head1 GOALS

This project is meant to be the foundation of a complete benchmarking
and regression testing suite for an advanced, transaction-based mod_perl
site.  We need to be able to stress our server to its limit while also
having a way to verify the HTTP responses for correctness.  Since our site
is transaction-based (as opposed to content-based), we needed to extend
the single-URL ab model to a multiple-URL sequence model.

ApacheBench is based on the Apache 1.3.12 ab code (src/support/ab.c).

Note: although this tool was designed to be used on an Apache mod_perl
site, it is generally applicable to any HTTP-compliant server.  Beware,
however, that it sends a high volume of HTTP requests in a very short period
of time, which may overwhelm some weaker HTTP server platforms like NT/IIS.

=head1 DESCRIPTION

ApacheBench sends sequences of HTTP requests to an HTTP server and keeps
track of the time taken to receive a response, the data that was returned,
the size of the data that was returned, and various other bits of information.

Since it is implemented in C, it sends HTTP requests in a tight loop which
can stress your server to 100% capacity, especially if invoked in multiple
concurrent instances.  It gives accurate time measurements down to the
millisecond for each HTTP request-response interval.

Included is a simplified re-implementation of ab using the ApacheBench
Perl API.  This should help get you started with ApacheBench.

=head1 CONFIGURATION METHODS

You need to tell ApacheBench what the requests will be, in what order to
send them, and how to prioritize sending them.  ApacheBench was designed
to simulate many users logged in simultaneously, each of whom may be doing
many different transactions at once.

=head2 Global configuration

Global configuration methods apply to all benchmarking runs associated
with this ApacheBench object.

=over 4

=item $b = HTTPD::Bench::ApacheBench->new()

Constructor.  Takes no arguments.

=item $b->concurrency( $concur_level )

Number of requests to send simultaneously (default: B<1>)

=item $b->priority( $priority )

$priority can be either "B<equal_opportunity>" or "B<run_priority>".

If set to "B<equal_opportunity>", all benchmark runs that are configured
(see below) under this ApacheBench object are given equal access to
the concurrency level.  This means requests are taken from each run
and sent in parallel (the level of parallelism defined by concurrency()).

If set to "B<run_priority>", the benchmark runs that are configured first
get the highest priority.  This means all requests in $b->run(0) will
be sent before any requests in $b->run(1) are sent, if
$b->run(0)->order eq "B<breadth_first>" (see below).

(default: "B<equal_opportunity>")

=item $b->repeat( $n )

The number of times to repeat the request sequence in each run.
This can be overridden on a per-run basis (see below).

(default: B<1>)

=item $b->buffersize( $bufsz )

The maximum size of the buffer in which to store HTTP response bodies.
If an HTTP response is received with a size larger than this limit,
the content is truncated at length $bufsz, and a warning is issued.
This method has no effect if $b->memory() < 3.
This can be overridden on a per-run basis (see below).

(default: B<16384>)

=item $b->memory( $memlvl )

The memory level.  Controls how much data ApacheBench will remember and
return in the regression object.  This can be overridden on a per-run basis
(see below).

(default: B<1>)

Key:
 $memlvl => Description

  0  =>  Remember nothing. (actually still keeps global
         regression data: total_time, bytes_received,
         and warnings)

  1  =>  Remember connect/response times and minimal
         summary information about size of requests and
         responses.

  2  =>  Remember connect/response times and all
         information about size of requests and responses.
         Also keeps an array of all HTTP response
         headers returned by the server for each request.

  3  =>  Remember connect/response times, all information
         about request/response sizes, HTTP response
         headers, and also all content of every HTTP
         response returned by the server.  Warning:
         this can quickly eat up all available main
         memory if used with large runs.

=item $b->add_run( $run_object )

Schedule a run for this ApacheBench object.  Returns the run number where
this object was inserted, or undef on failure.  See below for details on
$run_object.

=item $r = $b->run( $run_no, [$run_object] )

Returns the run object stored in location $run_no.  If a run object is passed
as the optional second argument, it is stored in location $run_no, displacing
whatever was there.  The displaced run object is then returned.

=item $b->delete_run( $run_no )

Delete the run object stored in location $run_no.  The deleted run object
is returned to the caller for safety sake.

=back

=head2 Run configuration methods

You need to configure one or more benchmark runs.  Each run is defined
as an ordered sequence of HTTP requests to be sent to the server, which can
be repeated multiple times, and scheduled to be sent in different orders.

=over 4

=item $r = HTTPD::Bench::ApacheBench::Run->new( { urls => [@url_list] } )

Construct a run object with the ordered sequence of HTTP requests
in @url_list.

=item $r->repeat( $n )

Number of times to repeat this request sequence.

(default: B<1>, or whatever is specified in global configuration)

=item $r->cookies( \@cookies )

Set the HTTP Cookie: header for each B<repetition> of this run.
Length of @cookies should equal $n (whatever you set $r->repeat to).
If this option is omitted, no cookies will be sent in any of the
requests for this run.

Example usage:  You could simulate $n users all doing the same transaction
simultaneously by giving $n different login cookies here.  Say you have
login cookies in an array called @login of length $n.  Set $r->repeat($n),
$r->order("breadth_first"), $r->cookies([map {$login[$_]} 0..$n]), and
ApacheBench will perform the transaction sequence set by $r->urls $n times
each with a separate login cookie.

=item $r->urls( \@url_list )

Set the HTTP request URLs for this run.  A @url_list B<must> be given for
each run, otherwise the run will not execute.  Typically @url_list is set
using the constructor.

=item $r->postdata( \@postdata )

Set the HTTP POST request body contents.  If an undef value is encountered
in @postdata, that request will be a GET request.  If this option
is omitted, all requests for this run will be GET requests.
Length of @postdata should equal the length of @url_list.

=item $r->content_types( \@ctypes )

Set the Content-type: header for each POST request in this run.  Default
is "application/x-www-form-urlencoded" which will be used if an undef
value is encountered in @ctypes.  Length of @ctypes should equal the
length of @url_list.

=item $r->order( $order )

Either "B<depth_first>" or "B<breadth_first>"

"B<breadth_first>" mode sends $n of the first request in the
@url_list, then $n of the second request in the @urls_list,
then $n of the third... and so on.  (e.g. If $n == 3 and @url_list
contains two requests, then ApacheBench would send the first
request 3 times, and then the second request 3 times.)

"B<depth_first>" mode ensures that HTTP requests in the sequence are
sent in order, completing a full sequence before starting again for the
next repeat() iteration.  (e.g. If $n == 3 and @url_list contains two
requests, then ApacheBench would send the @url_list sequence in order,
then send it again, and then again.  A total of six requests would be sent.)

(default: "B<breadth_first>")

B<Note:> if $r->repeat() == 1, or the length of $r->urls() is 1, then the
B<order> option has no effect

=item $r->buffersize( $bufsz )

The maximum size of the buffer in which to store HTTP response bodies.
If an HTTP response is received with a size larger than this limit,
the content is truncated at length $bufsz, and a warning is issued.
This method has no effect if $r->memory() < 3.

(default: B<16384>, or whatever is specified in global configuration)

=item $r->memory( $memlvl )

The memory level.  Controls how much data ApacheBench will remember and
return in the regression object for this run.  See global configuration
method of same name for detailed description.

(default: B<1>, or whatever is specified in global configuration)

=back

=head1 EXECUTION METHODS

=over 4

=item $b->execute

Send all scheduled runs to their respective destinations, and record
response data.

=back

=head1 REGRESSION METHODS

All of the following methods will return B<undef> unless the underlying
ApacheBench object has been execute()'d at least once.

=head2 Global regression methods

=over 4

=item $b->total_time

Total time, in milliseconds, between the start of the first request in the
first run, and the end of the final response in the final run.

=item $b->bytes_received

Total bytes received from all responses in all runs.

=item $b->warnings

Various warning messages.

=back

=head2 Run regression methods

=over 4

=item $i = $r->iteration( [$iter_no] )

Return a regression object specific to iteration $iter_no of this run.
If $iter_no is not given, it assumes 0, or the first iteration of the run.

=item $i->connect_times

Return a reference to an array the same length as $r->urls() which contains
connection times, in milliseconds, for each HTTP request in this iteration
of the sequence.

=item $i->min_connect_time

The minimum connect time of all requests in the sequence.

=item $i->max_connect_time

The maximum connect time of all requests in the sequence.

=item $i->avg_connect_time

The average connect time of all requests in the sequence.

=item $i->sum_connect_time

The total connect time of all requests in the sequence (equal to the
summation of all elements of $i->connect_times).

=item $i->request_times

A reference to an array the same length as $r->urls() which contains the total
times taken to send a request to the server, in milliseconds,
for each HTTP request in the sequence.

=item $i->min_request_time

The minimum request time of all requests in the sequence.

=item $i->max_request_time

The maximum request time of all requests in the sequence.

=item $i->avg_request_time

The average request time of all requests in the sequence.

=item $i->sum_request_time

The total request time of all requests in the sequence (equal to the
summation of all elements of $i->request_times).

=item $i->response_times

A reference to an array the same length as $r->urls() which contains the total
times taken to receive a response from the server, in milliseconds,
for each HTTP request in the sequence.

=item $i->min_response_time

The minimum response time of all requests in the sequence.

=item $i->max_response_time

The maximum response time of all requests in the sequence.

=item $i->avg_response_time

The average response time of all requests in the sequence.

=item $i->sum_response_time

The total response time of all requests in the sequence (equal to the
summation of all elements of $i->response_times).

=item $i->bytes_posted

A reference to an array the same length as $r->urls() which contains the
number of bytes posted to the server for each HTTP request in the sequence.

(return value will be undefined if B<memory> < 2)

=item $i->sum_bytes_posted

The total number of bytes posted to the server in HTTP requests over this
iteration of the request sequence.

=item $i->bytes_read

A reference to an array the same length as $r->urls() which contains the total
number of bytes read from the server in each HTTP response in the sequence.

(return value will be undefined if B<memory> < 2)

=item $i->sum_bytes_read

The total number of bytes read from the server in HTTP responses over this
iteration of the request sequence.

=item $i->response_headers

A reference to an array the same length as $r->urls() which contains the HTTP
response headers returned by the server for each URL in the sequence.

(return value will be undefined if B<memory> < 2)

=item $i->response_body

A reference to an array the same length as $r->urls() which contains the full
page content (including HTTP headers) returned by the server for each
URL in the sequence.

(return value will be undefined if B<memory> < 3)

=item $i->response_body_lengths

A reference to an array the same length as $r->urls() which contains the length
of the document (in bytes) returned from the server in each HTTP response in
the sequence.  This should be equivalent to the Content-Length: line in the
response headers, if the server set them correctly.  The following should
also be true, for 1 <= $j <= size($r->urls()):

( $i->response_body_lengths()->[$j] == $i->bytes_read()->[$j] - length($i->headers()->[$j]) )

(return value will be undefined if B<memory> < 2)

=back

=head1 EXAMPLES

The following examples of ApacheBench usage are paired with the resulting
output from an Apache access_log.  This should give you a feel for how the
global $b->priority() method and the per-run $r->order() method affect how
HTTP requests are sent.

First, let's set $b->priority("B<equal_opportunity>") (its default).

  my $b = HTTPD::Bench::ApacheBench->new;
  $b->concurrency(1);
  $b->priority("equal_opportunity");

Add a single run and execute, then look at what gets sent to Apache.

  my $r = HTTPD::Bench::ApacheBench::Run->new
    ({
       repeat    => 3,
       urls      => [ "http://localhost/",
                      "http://localhost/server-status" ],
       order     => "breadth_first",
     });
  $b->add_run($r);
  $b->execute;

Apache access_log output:

  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET /server-status HTTP/1.0" 200 5294

Let's add another run and execute, and see what Apache sees.

  my $r2 = HTTPD::Bench::ApacheBench::Run->new
    ({
       repeat       => 3,
       urls         => [ "http://localhost/perl-status",
                         "http://localhost/proxy-status" ],
       order        => "breadth_first",
     });
  $b->add_run($r2);
  $b->execute;

Notice that both the first and second runs get equal opportunity.
Apache access_log output:

  127.0.0.1 - - [20/Sep/2000:18:49:10 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /proxy-status HTTP/1.0" 200 5886
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /proxy-status HTTP/1.0" 200 5888
  127.0.0.1 - - [20/Sep/2000:18:49:11 -0400] "GET /proxy-status HTTP/1.0" 200 5889

Now let's set $b->priority("B<run_priority>").

  $b->priority("run_priority");
  $b->execute;

Notice that now ApacheBench completes the entire first run before it starts
the second.  Here's the Apache access_log output:

  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /proxy-status HTTP/1.0" 200 5858
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /proxy-status HTTP/1.0" 200 5861
  127.0.0.1 - - [20/Sep/2000:18:52:47 -0400] "GET /proxy-status HTTP/1.0" 200 5864

Let's now set our runs to $r->order("B<depth_first>") instead of
"B<breadth_first>".  With "B<depth_first>", $b->priority() has no effect,
since each run can only use a maximum of one concurrent server
(by definition of "B<depth_first>", it can only be sending one request
at a time).

  $b->run(0)->order("depth_first");
  $b->run(1)->order("depth_first");
  $b->execute;

Notice each sequence gets sent in full before it repeats.
Apache access_log output:

  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /proxy-status HTTP/1.0" 200 5858
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /proxy-status HTTP/1.0" 200 5860
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:19:02:01 -0400] "GET /proxy-status HTTP/1.0" 200 5860

Now let's send the same runs, but with a higher concurrency level.

  $b->concurrency(2);
  $b->execute;

Notice that ApacheBench sends requests from all runs in order to fill up
the specified level of concurrent requests.  Apache access_log output:

  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /proxy-status HTTP/1.0" 200 5891
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /proxy-status HTTP/1.0" 200 5878
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /perl-status HTTP/1.0" 200 848
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:19:04:38 -0400] "GET /proxy-status HTTP/1.0" 200 5878

Let's take a look at the regression data from that last execute() call.

  foreach my $run (0..1) {
    foreach my $repeat (0..2) {
      print "response times (in ms) for run $run, iteration ".($repeat+1).":\n  ";
      print join("\n  ", @{$b->run($run)->iteration($repeat)->response_times});
      print "\n";
    }
  }

Perl output:

  response times (in ms) for run 0, iteration 1:
    69
    39
  response times (in ms) for run 0, iteration 2:
    67
    39
  response times (in ms) for run 0, iteration 3:
    65
    41
  response times (in ms) for run 1, iteration 1:
    67
    40
  response times (in ms) for run 1, iteration 2:
    66
    39
  response times (in ms) for run 1, iteration 3:
    65
    39


=head1 BUGS

Error checking is getting better but is still not great.
If you do not configure correctly, you may experience a
segmentation fault on execute().

The response body of any response which is larger than the B<buffersize>
applicable to it will be truncated to zero length.  This is contrary to
what the documentation says above.  This should be fixed soon.  For now,
just set your $r->buffersize() big enough for the largest page you anticipate
receiving.

Methods total_requests(), total_requests_sent(), and total_responses_received()
are temporary hacks that are correct most of the time, but not always.

If you are running in perl's taint-checking mode, and you pass tainted data
to ApacheBench (e.g. a tainted URL), it will barf.  Don't ask me why.

It has not been tested on any platforms other than Linux / Apache.
Please send ports to other platforms to me (Adi Fairbank).

=head1 AUTHORS

The ApacheBench Perl API is based on code from
Apache 1.3.12 ab (src/support/ab.c), by the Apache group.

The ApacheBench Perl API was originally written by Ling Wu <ling@certsite.com>

Rewritten and currently maintained by Adi Fairbank <adi@certsite.com>

Please e-mail either Adi or Ling with bug reports, or preferably patches.

=head1 LICENSE

This package is free software and is provided AS IS without express or
implied warranty.  It may be used, redistributed and/or modified under the
terms of the Perl Artistic License
(http://www.perl.com/perl/misc/Artistic.html)

=head1 LAST MODIFIED

Feb 22, 2001

=cut
