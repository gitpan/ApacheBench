#!/usr/bin/perl

use strict;
use Test;

BEGIN { plan tests => 8 }

use HTTPD::Bench::ApacheBench;

my $b = HTTPD::Bench::ApacheBench->new;
ok(ref $b, "HTTPD::Bench::ApacheBench");

$b->add({
	 repeat       => 3,
	 urls         => [ "http://localhost/",
			   "http://localhost/server-status" ],
	 order        => "depth_first",
	});

ok($b->{runs}->[0]->{repeat}, 3);
ok(ref $b->{runs}->[0]->{urls}, "ARRAY");
ok($#{$b->{runs}->[0]->{urls}}, 1);
ok(ref $b->{runs}->[0]->{cookie}, "ARRAY");
ok(ref $b->{runs}->[0]->{postdata}, "ARRAY");
ok($#{$b->{runs}->[0]->{postdata}}, 1);
ok($b->{runs}->[0]->{order}, "depth_first");
