package HTTPD::Bench::ApacheBench;

use strict;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);

require Exporter;
require DynaLoader;
require AutoLoader;

@ISA = qw(Exporter DynaLoader);
@EXPORT = qw(
	
);
$VERSION = '0.51';

bootstrap HTTPD::Bench::ApacheBench $VERSION;

##################################################
## the constructor                              ##
##################################################
sub new {
    my ($this, $self) = @_;
    my $class = ref($this) || $this;
    if (ref($self) ne "HASH") {	$self = {} }
    $self->{runs} = [] if ref $self->{runs} ne "ARRAY";
    $self->{concurrency} = 1 if !exists $self->{concurrency};
    $self->{repeat} = 1 if !exists $self->{repeat};
    $self->{priority} = "equal_opportunity" if !exists $self->{priority};
    $self->{filesize} = 16556 if !exists $self->{filesize};
    bless $self, $class;
    return $self;
}

##################################################
## configurate the concurrency level            ##
##################################################
sub config {
   my ($self,$data_hash) = @_;
   if($data_hash->{concurrency}){$self->{concurrency} = $data_hash->{concurrency}};
   if($data_hash->{priority}){$self->{priority} = $data_hash->{priority}};
   if($data_hash->{filesize}){$self->{filesize} = $data_hash->{filesize}};
   if($data_hash->{repeat}){$self->{repeat} = $data_hash->{repeat}};
}


##################################################
## add a run with specific parameters           ##
##################################################
sub add {
   my ($self,$data_hash) = @_;
   $data_hash->{cookie} = [ undef ] if ref $data_hash->{cookie} ne "ARRAY";
   $data_hash->{postdata} = [ map {undef} @{$data_hash->{urls}} ]
     if ref $data_hash->{postdata} ne "ARRAY";
   push(@{$self->{runs}},$data_hash);
}


##################################################
## call the main 'c' program                    ##
##################################################
sub execute {
   my $self = shift;
   return $self->ab;
}


1;

__END__

=head1 NAME

HTTPD::Bench::ApacheBench - Perl API for Apache benchmarking and regression testing.

=head1 SYNOPSIS

  use HTTPD::Bench::ApacheBench;

  my $b = HTTPD::Bench::ApacheBench->new;

  # global configuration
  $b->config({
	      concurrency  => 5,
	      priority     => "run_priority",
	     });

  # add sequence(s) of URLs to request
  $b->add({
	   repeat    => 10,
	   cookie    => ["Login_Cookie=b3dcc9bac34b7e60;"],
	   urls      => ["http://localhost/one", "http://localhost/two"],
	   postdata  => [undef, undef],
	   order     => "depth_first",
	  });

  my $regress = $b->execute;

  # calculate hits/sec == ($#urls+1)*$n*1000 / total_time
  print (2*10*1000/$regress->{"total_time"})." req/sec\n";

  # dump the entire regression hash (WARNING, this could be a LOT OF DATA)
  use Data::Dumper;
  my $d = Data::Dumper->new([$regress]);
  print $d->Dumpxs;


=head1 GOALS

This project is meant to be the foundation for a complete benchmarking
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

=head1 METHODS

=over 4

=item new()

The constructor.  It takes no arguments.

=item config({ %global_params })

Global configuration method.  Should only be invoked once, else previous
configuration parameters will be clobbered.  See the global configuration
section for details on how %global_params should be structured.

=item add({ %run_params })

Run configuration method.  Can be invoked multiple times.  Each invocation
will register a new benchmark run to be executed.  See the run configuration
section for details on how %run_params should be structured.

=item execute()

The execute method takes no arguments.  It will send the HTTP requests
and return a hash reference to the regression data.

=back

=head1 CONFIGURING

You need to tell ApacheBench what the requests will be, in what order to
send them, and how to prioritize sending them.  ApacheBench was designed
to simulate many users logged in simultaneously, each of whom may be doing
many different transactions at once.

=head2 Object constructor

First, you will need to construct an ApacheBench object.

B<Example:>

  use HTTPD::Bench::ApacheBench;
  my $b = HTTPD::Bench::ApacheBench->new;

=head2 Global configuration

Next, you need to setup global configuration parameters.  These apply to all
benchmarking runs associated with this ApacheBench object.

Global configuration is done by calling the config() method with a
reference to a hash containing the configuration parameters.  The global
configuration parameters are: B<concurrency>, B<priority>, B<repeat>, and
B<filesize>.

=over 4

=item B<concurrency>

Number of requests to send simultaneously (default: B<1>)

=item B<priority>

Either B<equal_opportunity> or B<run_priority>.

If set to B<equal_opportunity>, all benchmark runs that are configured
(see below) under this ApacheBench object are given equal access to
the concurrency level.  This means requests are taken from each run
and sent in parallel (the level of parallelism defined by B<concurrency>).

If set to B<run_priority>, the benchmark runs that are configured first
get the highest priority.  This means all requests in the first run will
be sent before any requests in the second run are sent, if the first run
is configured as B<order> => B<breadth_first> (see below).

(default: B<equal_opportunity>)

=item B<repeat>

The number of times to repeat the request sequence in each run.
This can be overridden on a per-run basis (see below).

(default: B<1>)

=item B<filesize>

The maximum size of the buffer to store HTTP responses.  If an HTTP
response is received with a size larger than this limit, the content
is truncated at length B<filesize>, and a warning is issued.
This can be overridden on a per-run basis (see below).

(default: B<16556>)

=back

B<Example:>

  $b->config({
              concurrency => 4,
              priority    => "run_priority",
             });

=head2 Run configuration

Finally, you need to setup one or more benchmark runs.  Each run is defined
as an ordered sequence of HTTP requests to be sent to the server, which can
be repeated multiple times, and scheduled to be sent in different ways.
For each run you want to configure, call the add() method with a hash
reference containing the following configuration parameters:

=over 4

=item B<repeat>

Number of times to repeat this request sequence.

(default: B<1>, or whatever is specified in global config())

=item B<cookie>

An array reference of length B<repeat> containing HTTP cookies to send.
This is meant to be used for login IDs.  You could simulate N users all
doing the same transaction simultaneously by giving N different login
cookies here.  If this option is omitted, no cookies will be sent in
any of the requests for this run.

=item B<urls>

An array reference containing the URLs in the sequence

=item B<postdata>

An array reference containing data to POST to the corresponding URL in the
B<urls> sequence.  For GET requests use B<undef>.  If this option is
omitted, all requests for this run will be GET requests.

=item B<order>

Either B<depth_first> or B<breadth_first>

B<breadth_first> mode sends B<repeat> of the first request in the
B<urls> list, then B<repeat> of the second request in the
B<urls> list, then B<repeat> of the third... and so on.
(e.g. If B<repeat> == 3 and B<urls> contains two requests, then
ApacheBench would send the first request 3 times, and then the
second request 3 times.)

B<depth_first> mode ensures that HTTP requests in the sequence are
sent in order, completing a full sequence before starting again for the
next B<repeat> iteration.
(e.g. If B<repeat> == 3 and B<urls> contains two requests, then
ApacheBench would send the B<urls> sequence in order, then send it again,
and then again.  A total of six requests would be sent.)

(default: B<breadth_first>)

B<Note:> if B<repeat> == 1, or the length of B<urls> is 1, then the
B<order> option has no effect

=item B<filesize>

The maximum size of the buffer to store HTTP responses.  If an HTTP
response is received with a size larger than this limit, the content
is truncated at length B<filesize>, and a warning is issued.

(default: B<16556>, or whatever is specified in global config())

=back

B<Example:>

  $b->add({
           repeat    => 10,
           cookie    => ["LoginID=b3dcc9bac34b7e60;"],
           urls      => ["http://localhost/one",
                         "http://localhost/two"],
           postdata  => [undef, undef],
           order     => "depth_first",
          });

=head1 EXECUTING

Simply call the execute() method, which returns a single hash
reference after all runs are completed.

B<Example:>

  my $regress = $b->execute;

=head1 REGRESSION

All data about the benchmark runs are stored in the hash reference
returned by the execute() method.

=head2 Top-level of regression hash

The following global regression data is returned in the top-level of the hash.

=over 4

=item B<total_time>

Total time, in milliseconds, between the start of the first request in the
first run, and the end of the final response in the final run.

=item B<bytes_received>

Total bytes received from all responses in all runs.

=item B<warn>

Various warning messages.

=item B<run0 .. runN>

Hash reference to regression data for each run (see below).

=back

=head2 Regression data for each run

Regression data for each run are separated into individual hashes
labelled B<runI> where B<I> is the run number.  Run numbers start at
zero, so B<run0> will always exist.  Each B<runI> hash has the following
keys: B<threads>, and a value for each individual URL in the B<urls>
sequence.

=over 4

=item B<threads>

This is an array reference containing data about each iteration of the
URL sequence.  A thread refers to a single iteration of the URL sequence
for this run.

Each element in the B<threads> array reference is a hash reference
containing the following data:

=over 8

=item connect_time

An array reference the same length as B<urls> which contains connection
times, in milliseconds, for each URL in the sequence.

=item total_time

An array reference the same length as B<urls> which contains the total
times taken to receive a response from the server, in milliseconds,
for each URL in the sequence.

=item headers

An array reference the same length as B<urls> which contains the HTTP
response headers returned by the server for each URL in the sequence.

=item page_content

An array reference the same length as B<urls> which contains the full
page content (including HTTP headers) returned by the server for each
URL in the sequence.

B<page_content> is very useful for regression testing.  You can
use a parser relevant to your application (e.g. HTML::Parser, XML::Parser)
to verify that the HTTP response is correct and expected for each step
in the request sequence.

=back

=item B<http://url.first/ .. http://url.last/>

Each individual URL in the B<urls> sequence is a hash containing
the following keys:

=over 8

=item hostname

The hostname this request was sent to.

=item port

The port this request was sent to.

=item software

The Server: line from the HTTP response headers.

=item doc_length

The length of the HTTP response document (not including headers).
This should be equivalent to the Content-Length: line in the response
headers, if the headers were set correctly.

=item total_read

Total bytes read from the server for all repetitions of this URL.

=item header

The HTTP response headers.

=item page_content

The full HTTP response, including headers.

=item completed_requests

Number of requests for this URL that completed successfully.

=item failed_requests

Number of requests for this URL that failed miserably.

=item min_time

The minimum response time of all repetitions of this URL.

=item max_time

The maximum response time of all repetitions of this URL.

=item average_time

The average response time of all repetitions of this URL.

=item min_connect_time

The minimum connect time of all repetitions of this URL.

=item max_connect_time

The maximum connect time of all repetitions of this URL.

=item average_connect_time

The average connect time of all repetitions of this URL.

=back

=back


=head1 EXAMPLES

The following examples of ApacheBench usage are paired with the resulting
output from an Apache access_log.  This should give you a feel for how the
global B<priority> parameter and the per-run B<order> parameter affect how
HTTP requests are sent.

First, let's set global B<priority> to its default B<equal_opportunity>.

  my $b = HTTPD::Bench::ApacheBench->new;
  $b->config({
              concurrency => 1,
              priority    => "equal_opportunity",
             });

Add a single run and execute, then look at what gets sent to Apache.

  $b->add({
           repeat       => 3,
           urls         => [ "http://localhost/",
                             "http://localhost/server-status" ],
           order        => "breadth_first",
          });
  $b->execute;

Apache access_log output:

  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET / HTTP/1.0" 200 5565
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET /server-status HTTP/1.0" 200 5294
  127.0.0.1 - - [20/Sep/2000:18:43:32 -0400] "GET /server-status HTTP/1.0" 200 5294

Let's add another run and execute, and see what Apache sees.

  $b->add({
           repeat       => 3,
           urls         => [ "http://localhost/perl-status",
                             "http://localhost/proxy-status" ],
           order        => "breadth_first",
          });
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

Now let's set global B<priority> to B<run_priority>.

  $b->config({
              concurrency => 1,
              priority    => "run_priority",
             });
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

Let's now create a new ApacheBench object with runs set to B<depth_first>
instead of B<breadth_first>.  With B<depth_first>, the global B<priority>
option has no effect, since each run can only use a maximum of one
concurrent server (by definition, it can only be sending one request
at a time).  So we'll just leave it set to B<run_priority>.

  my $b = HTTPD::Bench::ApacheBench->new;
  $b->config({
              concurrency => 1,
              priority    => "run_priority",
             });
  $b->add({
           repeat       => 3,
           urls         => [ "http://localhost/",
                             "http://localhost/server-status" ],
           order        => "depth_first",
          });
  $b->add({
           repeat       => 3,
           urls         => [ "http://localhost/perl-status",
                             "http://localhost/proxy-status" ],
           order        => "depth_first",
          });
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

  $b->config({
              concurrency => 2,
              priority    => "run_priority",
             });
  my $regress = $b->execute;

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

We captured the regression data on that last execute() call, so let's
take a look at it.

  print "response times (in ms) for run 0, 1st iteration:\n  ";
  print join("\n  ", @{$regress->{'run0'}->{'threads'}->[0]->{'total_time'}});
  print "\n";

  print "response times (in ms) for run 0, 2nd iteration:\n  ";
  print join("\n  ", @{$regress->{'run0'}->{'threads'}->[1]->{'total_time'}});
  print "\n";

  print "response times (in ms) for run 0, 3rd iteration:\n  ";
  print join("\n  ", @{$regress->{'run0'}->{'threads'}->[2]->{'total_time'}});
  print "\n";

  print "response times (in ms) for run 1, 1st iteration:\n  ";
  print join("\n  ", @{$regress->{'run1'}->{'threads'}->[0]->{'total_time'}});
  print "\n";

  print "response times (in ms) for run 1, 2nd iteration:\n  ";
  print join("\n  ", @{$regress->{'run1'}->{'threads'}->[1]->{'total_time'}});
  print "\n";

  print "response times (in ms) for run 1, 3rd iteration:\n  ";
  print join("\n  ", @{$regress->{'run1'}->{'threads'}->[2]->{'total_time'}});
  print "\n";

Perl output:

  response times (in ms) for run 0, 1st iteration:
    69
    39
  response times (in ms) for run 0, 2nd iteration:
    67
    39
  response times (in ms) for run 0, 3rd iteration:
    65
    41
  response times (in ms) for run 1, 1st iteration:
    67
    40
  response times (in ms) for run 1, 2nd iteration:
    66
    39
  response times (in ms) for run 1, 3rd iteration:
    65
    39


=head1 BUGS

Error checking is a bit poor, so if you call the config() or add() methods
incorrectly (e.g. with insufficient configuration parameters) you may
experience a segmentation fault on execute().

The B<page_content> of any response which is larger than the B<filesize>
applicable to it will be truncated to zero length.  This is contrary to
what the documentation says above.  This should be fixed ASAP.  For now,
just set your B<filesize> big enough for the largest page you anticipate
receiving in any run.

ApacheBench may consume quite a lot of memory in some cases, depending
on how big your runs are, because it stores all HTTP response data in memory.

It has not been tested on any platforms other than Linux / Apache.
Please send ports to other platforms to me (Adi Fairbank).

=head1 AUTHOR

The ApacheBench Perl API was written by Ling Wu <ling@certsite.com>
with guidance and moral support from Adi Fairbank <adi@certsite.com>.

The ApacheBench Perl API is based on code from
Apache 1.3.12 ab (src/support/ab.c), by the Apache group.

The simplified re-implementation of ab, included with this distribution,
was written by Adi Fairbank.

Documentation for the ApacheBench Perl API was written by Adi Fairbank.

Please e-mail either Adi or Ling with bug reports, or preferably patches.

=head1 LICENSE

This package is free software and is provided AS IS without express or
implied warranty.  It may be used, redistributed and/or modified under the
terms of the Perl Artistic License
(http://www.perl.com/perl/misc/Artistic.html)

=head1 LAST MODIFIED

Sep 20, 2000

=cut
