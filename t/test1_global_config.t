#!/usr/bin/perl

use strict;
use Test;

BEGIN { plan tests => 5 }

use HTTPD::Bench::ApacheBench;

my $b = HTTPD::Bench::ApacheBench->new;
ok(ref $b, "HTTPD::Bench::ApacheBench");

$b->config({
	    concurrency  => 2,
	    priority     => "run_priority",
	   });
ok(exists $b->{filesize});
ok(exists $b->{repeat});
ok($b->{concurrency}, 2);
ok($b->{priority}, "run_priority");
