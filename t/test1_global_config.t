#!/usr/bin/perl

use strict;
use Test;

BEGIN { plan tests => 5 }

use ApacheBench;

my $b = ApacheBench->new;
ok(ref $b, "ApacheBench");

$b->config({
	    concurrency  => 2,
	    priority     => "run_priority",
	   });
ok(exists $b->{filesize});
ok(exists $b->{repeat});
ok($b->{concurrency}, 2);
ok($b->{priority}, "run_priority");
