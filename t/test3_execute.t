#!/usr/bin/perl

use strict;
use Test;

BEGIN { plan tests => 7 }

use Term::ReadKey;
use ApacheBench;

my $b = ApacheBench->new;
ok(ref $b, "ApacheBench");

ReadMode 1;
print STDERR "\n\nIt is rude to blast other people's servers, please enter local URLs\n";

my (@urls, $url);
do {
    print STDERR "Current \@urls: [" . join(", ", @urls) . "]\n";
    print STDERR "Enter a URL to test, <ENTER> to quit: ";
    $url = ReadLine(0);
    chomp $url;
    push(@urls, $url) if $url;
} while ($url);

print STDERR "How many times to repeat this sequence? ";
my $n = ReadLine(0);
chomp $n;

$b->add({
	 repeat       => $n,
	 urls         => [ @urls ],
	 order        => "depth_first",
	});
ok($b->{runs}->[0]->{repeat}, $n);
ok($#{$b->{runs}->[0]->{urls}}, $#urls);
ok($b->{runs}->[0]->{order}, "depth_first");

my $re;
if (@urls) {
    print STDERR "\nSending HTTP requests...\n";
    $re = $b->execute;

    print STDERR "\n" . $re->{'bytes_received'} . " bytes in " . $n*($#urls+1) . " requests received in " . $re->{'total_time'} . " ms\n";
    print STDERR ($n*($#urls+1)*1000/$re->{'total_time'}) . " req/sec\n";
    print STDERR ($re->{'bytes_received'}*1000/$re->{'total_time'}/1024) . " kb/sec\n";
}

ok(ref $re, "HASH");
ok($re->{'total_time'});
ok($re->{'bytes_received'});
