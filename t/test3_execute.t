#!/usr/bin/perl

use strict;
use Test;

BEGIN { plan tests => 12 }

use Term::ReadKey;
use HTTPD::Bench::ApacheBench;

my $b = HTTPD::Bench::ApacheBench->new;
ok(ref $b, "HTTPD::Bench::ApacheBench");

ReadMode 1;
print STDERR "\n\nIt is rude to blast other people's servers, please enter local URLs\n";

my (@urls, $url);
do {
    print STDERR "Current \@urls: [" . join(", ", @urls) . "]\n";
    print STDERR 'Type an HTTP URL to push, "P" to pop, "ENTER" when finished: ';
    $url = ReadLine(0);
    chomp $url;
    if (lc($url) eq 'p') {
	pop(@urls);
    } elsif ($url) {
	push(@urls, $url);
    }
} while ($url);

print STDERR "How many times to repeat this sequence? ";
my $n = ReadLine(0);
chomp $n;

my $run = HTTPD::Bench::ApacheBench::Run->new
  ({ repeat   => $n,
     urls     => [ @urls ],
     order    => "depth_first" });
ok(ref $run, "HTTPD::Bench::ApacheBench::Run");

$b->add_run($run);
ok($b->run(0), $run);
if ($n) {
    ok($b->run(0)->repeat, $n);
} else {
    skip(1, 1);
}
my $run0urls = $b->run(0)->urls;
ok(ref $run0urls, "ARRAY");
ok($#$run0urls, $#urls);
ok($b->run(0)->order, "depth_first");

if (@urls) {
    print STDERR "\nSending HTTP requests...\n\n";
    my $rg = $b->execute;

    print STDERR ($b->bytes_received." bytes, ".$b->total_responses_received.
		  " responses received in " . $b->total_time . " ms\n");
    print STDERR ($b->total_responses_received*1000/$b->total_time)." req/sec\n";
    print STDERR ($b->bytes_received*1000/1024/$b->total_time) . " kb/sec\n";

    ok(defined $b->total_responses_received);
    ok(defined $b->total_time);
    ok(defined $b->bytes_received);
    ok(!defined $b->response_times);
    ok(ref $rg->run(0)->iteration(0)->response_times, "ARRAY");
} else {
    foreach (1..5) { skip(1, 1) }
}
