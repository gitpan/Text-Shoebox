# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# Time-stamp: "2000-09-05 02:04:01 MDT"

use strict;
use Test;
BEGIN { plan tests => 18 };
use Text::Shoebox 0.21;
ok 1;

my $temp = 'temp.sf';
my $other;
my $idem = [
 [ 'foo', 'bar', 'baz', "quux foo\n\t\tchacha", 'thwak', '' ],
 [ 'foo', 'sntrak', 'hoopa heehah', "\n  things um\n  stuff\n"],
];

sub idem {
  my $x = $_[0] || $other;
  return 0 unless @$x == @$idem;
  my($i,$j,$e1,$e2);
  for($i = 0; $i < @$x; ++$i) {
    $e1 =    $x->[$i];
    $e2 = $idem->[$i];
    return 0 unless @$e1 == @$e2;
    for($j = 0; $i < @$x; ++$i) {
      return 0 unless $e1->[$j] eq $e2->[$j];
    }
  }
  return 1;
}

$Text::Shoebox::Debug = 2;

ok( write_sf(to_file => $temp, from => $idem) );
ok( $other = read_sf(from_file => $temp) and idem );
ok( $other = read_sf(from_file => $temp, rs => $/) and idem );
#print `od -xc $temp`;

ok( write_sf(to_file => $temp, from => $idem, rs => "\cm") );
ok( $other = read_sf(from_file => $temp) and idem );
ok( $other = read_sf(from_file => $temp, rs => "\cm") and idem );
#print `od -xc $temp`;

ok( write_sf(to_file => $temp, from => $idem, rs => "\cm\cj") );
ok( $other = read_sf(from_file => $temp) and idem );
ok( $other = read_sf(from_file => $temp, rs => "\cm\cj") and idem );
#print `od -xc $temp`;

ok( write_sf(to_file => $temp, from => $idem, rs => "\cj") );
ok( $other = read_sf(from_file => $temp) and idem );
ok( $other = read_sf(from_file => $temp, rs => "\cj") and idem );
#print `od -xc $temp`;

ok( write_sf(to_file => $temp, from => $idem, rs => "\xF0") );
ok( $other = read_sf(from_file => $temp, rs => "\xF0") and idem );
#print `od -xc $temp`;

ok are_hw_keys_uniform($other);
ok are_hw_values_unique($other);
push @$other, @$other; # cheap hack
ok !are_hw_values_unique($other);

print "Finit.\n";
unlink $temp or warn "Can't unlink $temp";
