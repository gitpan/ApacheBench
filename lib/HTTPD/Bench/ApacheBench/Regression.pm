package HTTPD::Bench::ApacheBench::Regression;

use strict;
use vars qw($VERSION);

use HTTPD::Bench::ApacheBench;

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
	my $repeat = $run->repeat ? $run->repeat : $self->{repeat};
	$total += ($#{$run->urls} + 1) * $repeat
	  if ref $run->urls eq "ARRAY";
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
    my ($self, $value, $expect_ref, $idx) = @_;
    return undef unless (my $reg = $self->get_regression_hash);
    return undef unless defined $self->{'run_no'};
    my $iter_no = defined $self->{'iter_no'} ? $self->{'iter_no'} : 0;
    my $iter = $reg->{'run'.$self->{'run_no'}}->[$iter_no];
    return undef unless (ref $iter eq "HASH" and
			 (!$expect_ref or ref $iter->{$value} eq $expect_ref));
    return $iter->{$value}->[$idx] if $expect_ref eq "ARRAY" and defined $idx;
    return $iter->{$value};
}


sub connect_times {
    my ($self, $idx) = @_;
    return $self->iteration_value('connect_time', "ARRAY", $idx);
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
    my ($self, $idx) = @_;
    return $self->iteration_value('request_time', "ARRAY", $idx);
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
    my ($self, $idx) = @_;
    return $self->iteration_value('response_time', "ARRAY", $idx);
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
    my ($self, $idx) = @_;
    return $self->iteration_value('bytes_posted', "ARRAY", $idx);
}

sub sum_bytes_posted {
    my ($self) = @_;
    return $self->iteration_value('total_bytes_posted');
}

sub bytes_read {
    my ($self, $idx) = @_;
    return $self->iteration_value('bytes_read', "ARRAY", $idx);
}

sub sum_bytes_read {
    my ($self) = @_;
    return $self->iteration_value('total_bytes_read');
}

sub request_body {
    my ($self, $idx) = @_;
    return $self->iteration_value('request_body', "ARRAY", $idx);
}

sub response_headers {
    my ($self, $idx) = @_;
    return $self->iteration_value('headers', "ARRAY", $idx);
}

sub response_body {
    my ($self, $idx) = @_;
    return $self->iteration_value('page_content', "ARRAY", $idx);
}

sub response_body_lengths {
    my ($self, $idx) = @_;
    return $self->iteration_value('doc_length', "ARRAY", $idx);
}


1;
