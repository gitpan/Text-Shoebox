
require 5;
package Text::Shoebox::Entry;
use strict;
use vars qw(@ISA $Debug $VERSION);
use integer;
use Text::Shoebox ();
use Carp ();

$Debug = 1 unless defined $Debug;
$VERSION = "1.01";
my $Array_Class;
###########################################################################

=head1 NAME

Text::Shoebox::Entry -- class for Shoebox SF lexicon entries

=head1 SYNOPSIS

  use Text::Shoebox::Lexicon;
  my $lex = Text::Shoebox::Lexicon->read_file( "haida.sf" );

  foreach my $entry ($lex->entries) {
    #
    # Each $entry is a Text::Shoebox::Entry object
    #
    my %e = $entry->as_list;
    print "Looky, stuff! [", %e, "]!\n";
  }

=head1 DESCRIPTION

An object of this class represents an entry in a SF lexicon
(L<Text::Shoebox::Lexicon>).

An entry consists of a number of fields.  Each field has two scalars,
a key, and a value.  The first field in an entry is considered the
headword field, and its key must occur there and only there in that
entry.  There is no requirement on uniqueness of keys in the rest of
the entry.

=head1 METHODS

XX TODO XX

=over



=cut

sub new {  # Text::Shoebox is free to not use this, for speed's sake
  my $class = shift;
  $class = ref($class) || $class; # be an object or a class method
  print "New object in class $class\n" if $Debug;
  if(@_ == 1 and ref($_[0]) eq 'ARRAY') {
    # listref form -- call as: Text::Shoebox::Entry->new([foo => 'bar']);
    return bless $_[0], $class;
  } else {
    # list form -- call as: Text::Shoebox::Entry->new(foo => 'bar');
    return bless [@_], $class;
  }
}

sub copy {
  my $original = $_[0];
  Carp::croak("Text::Shoebox::entry is strictly an object method.")
    unless ref $original;
  return bless( [@$original], ref($original) );
  # bless into the same class as the original
  # presumably a deep copy isn't necessary!
}

#--------------------------------------------------------------------------

sub keys {
  my @out;
  for(my $i = 0; $i < @{$_[0]}; $i += 2) { push @out, $_[0][$i] }
  return @out;
}

sub values {
  my @out;
  for(my $i = 1; $i < @{$_[0]}; $i += 2) { push @out, $_[0][$i + 1] }
  return @out;
}

#--------------------------------------------------------------------------

sub headword { @{$_[0]} ? $_[0][1] : undef } # simply the first value

sub headword_field { @{$_[0]} ? $_[0][0] : undef } # simply the first key

sub pair {  # alias, for sanity's sake
  (shift)->pairs(@_);
}

sub pairs { # also good for accessing one pair, or none!
  # get pair #3 (assuming counting from 0) :  ($k,$v) = $e->pairs(3);
  my $o = shift;
  map { @{$o}[$_ * 2, $_ * 2 + 1] } @_;
   # map to slices. Better be legal offsets!
   # e.g., 3 maps to @{$o}[6,7]
}

#--------------------------------------------------------------------------

sub are_keys_unique {
  # returns true iff the keys are unique in this entry.
  # i.e., if no headword occurs twice (or more)
  return 1 if @{$_[0]} < 2; # can't have collisions with just one key!

  my %seen;
  for(my $i = 0; $i < @{$_[0]}; $i += 2) {
    return 0 if $seen{$_[0][$i]}++;
  }
  return 1;
}

sub assert_keys_unique {
  return $_[0] if $_[0]->are_keys_unique;
  my $e = shift;
  my @k = $e->keys;
  my %seen;
  for my $k (@k) { ++$seen{$k} }
  for my $k (@k) { $k = uc $k if $seen{$k} > 1 }
  Carp::croak "Entry $e \"$$e[1]\" has duplicate keys: [@k]\nAborting";
}

sub is_null { return( @{$_[0]} == 0 ) }

sub is_sane {
  my $e = $_[0];
  return 0 unless @$e; # empty entries are not sane
  for(my $i = 0; $i < @{$_[0]}; $i += 2) { # scan keys
    return 0 unless defined $e->[$i] and length $e->[$i];
    # all keys have to be defined and be non-null

    return 0 if ref $e->[$i] or ref $e->[$i+1];
    # no references anywhere!
  }

  return 1;
}

#--------------------------------------------------------------------------

sub tidy_up {
  my $e = $_[0];
  for(my $i = 1; $i < @$e; $i += 2) { # scan keys
    unless( defined $e->[$i] and $e->[$i] =~ m/\S/ ) {
      splice @$e, $i-1, 2;  # nix K=>V where V is null or all-whitespace
      $i-=2;
    }
    $e->[$i] =~ s/^\s+//s;
    $e->[$i] =~ s/\s+$//s;
    $e->[$i] =~ s/[ \t]*[\n\r]+[ \t]*/ /g;
     # remove newlines and any whitespace around them
  }
  return $e;
}

#--------------------------------------------------------------------------

sub dump {
  my $e = $_[0];

  print "Entry $e contains:\n";

  my $safe;
  my $toggle = 0;
  foreach my $v (@$e) {
    ($safe = $v) =~ 
            s<([^\x20\x21\x23\x27-\x3F\x41-\x5B\x5D-\x7E\xA1-\xFE])>
             <$Text::Shoebox::p{$1}>eg;
    print(
      ($toggle ^= 1) ? qq{  $safe = } : qq{"$safe"\n} 
    );
  }
  print "\n";
  return $e;
}

#--------------------------------------------------------------------------

sub as_list { return @{$_[0]} } # it /is/ a list!

sub as_hashref { return {@{$_[0]}} }

sub as_HoL {
  my %h;
  for(my $i = 0; $i < @{$_[0]}; $i += 2) {
    push @{ $h{ $_[0] } ||= []}, $_[0][$i+1];
  }
  foreach my $v (CORE::values %h) { bless $v, $Array_Class }
  \%h;
}

sub as_HoLS { # ref to a hash of list of refs to each of the value slots
  my %h;
  for(my $i = 0; $i < @{$_[0]}; $i += 2) {
    push @{
           $h{ $_[0] } ||= []
          }, \$_[0][$i + 1];
  }
  \%h;
}

sub as_pairs {
  # returns this entry...
  #     (hw => 'shash', english => 'bear')
  # as this...
  #     ([hw => 'shash'], [english => 'bear'])
  my @out;
  for(my $i = 0; $i < @{$_[0]}; $i += 2) {
    push @out, [ @{ $_[0] }[$i, $i+1] ];
  }
  return @out;
}

sub as_xml {
  # Yes, VERY simpleminded.  And note that the result is NOT wrapped
  #  in an <entry>...</entry> or anything.

  # returns this entry...
  #     (hw => 'shash', english => 'bear')
  # as this...
  #     " <hw>shash</hw>\n<english>bear</english>\n"

  # Consider this entry more as a suggestion, and as a debugging tool, than
  #  anything else.

  # Optional first parameter: a reference to a hash mapping key names
  #  to tags.  E.g., $e->as_xml({hw => 'headword', english => 'gloss'})
  # will give you this:
  #     " <headword>shash</headword>\n<gloss>bear</gloss>\n"

  my $map = ref($_[1]) ? $_[1] : {};

  my(@out, $k, $v);
  for(my $i = 0; $i < @{$_[0]}; $i += 2) {
    ($k,$v) = @{$_[0]}[$i, 1 + $i];

    if(exists $map->{$k}){
      $k = $map->{$k};
    } else {
      # spiff up the key name so it's an okay GI (tag name)
      $k =~ tr<-._:a-zA-Z0-9><_>cd; # Yes, this is conservative
      if(length $k) {
        $k = '_' . $k unless $k =~ m<^[_:a-zA-Z]>s;
        # prefix unsafe things.
      } else { # to avoid a null GI
        $k = 'NULL';
      }
    }

    $v =~ s/&/&amp;/g;
    $v =~ s/</&lt;/g ;
    $v =~ s/>/&gt;/g ;
    push @out, " <$k>$v</$k>\n";
  }
  return join '', @out;
}

sub as_xml_pairs {
  # A bit less pointless.  And note that the result is still not wrapped
  #  in an <entry>...</entry> or anything.

  # Returns this entry...
  #     (hw => 'shash', english => 'bear')
  # as this...
  #     " <pair key="hw" value="shash" /><pair key="english" value="bear"/>\n"

  # Consider this entry more as a suggestion, and as a debugging tool, than
  #  anything else.

  # Calling format: $e->as_xml_pairs(TAGNAME, KEYNAME, VALUENAME)
  #  TAGNAME defaults to 'pair'.
  #  KEYNAME defaults to 'key'.
  #  VALUENAME defaults to 'value'.

  my($o, $gi, $key_name, $value_name) = @_;
  $gi ||= 'pair';
  $key_name ||= 'key';
  $value_name ||= 'value';

  my(@out, $k, $v);
  for(my $i = 0; $i < @$o; $i += 2) {
    ($k,$v) = @{$o}[$i, 1 + $i];
    foreach my $x ($k, $v) {
      # NB: Doesn't entitify apostrophes.  No point, really.
      $x =~ s/&/&amp;/g;
      $x =~ s/"/&quot;/g;
      $x =~ s/</&lt;/g;
      $x =~ s/>/&gt;/g;
      $x =~ s<([\n\t\cm\cj])>
             <'&#'.(ord($1)).';'>seg;
       # turn newlines into character references
    }
    push @out, " <$gi $key_name=\"$k\" $value_name=\"$v\" />\n"
  }

  return join '', @out;
}

###########################################################################
{
  # Basically just the guts of Array::Autojoin:

  package Text::Shoebox::Entry::_Autojoin;
  $Array_Class = __PACKAGE__;

  use overload(

    '""' => sub {    join '; ', @{$_[0]}},

    '0+' => sub {0 + ( $_[0][0] || 0  ) },
     # stringifies and then just numerifies, but feh.

    'fallback' => 1,  # turn on cleverness

    'bool', => sub {  # true iff there's any true items in it
      for (@{$_[0]}) { return 1 if $_ };
      return '';
    },

    '.=' => sub {  # sure, why not.
      if(@{$_[0]}) { $_[0][-1] .= $_[1] } else { push @{$_[0]}, $_[1] }
      $_[0];
    },  # but can't overload ||= or the like

  );
}
###########################################################################
1;

__END__


=back



=head1 COPYRIGHT

Copyright 2004, Sean M. Burke C<sburke@cpan.org>, all rights
reserved.  This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 AUTHOR

Sean M. Burke, C<sburke@cpan.org>

I hasten to point out, incidentally, that I am not in any way
affiliated with the Summer Institute of Linguistics.

=cut

