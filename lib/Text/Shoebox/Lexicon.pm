
require 5;
package Text::Shoebox::Lexicon;
use strict;
use vars qw(@ISA $Debug $VERSION $Entry_Class);
use Carp ();

$Debug = 1 unless defined $Debug;
$VERSION = "1.01";
$Entry_Class ||= 'Text::Shoebox::Entry';

use Text::Shoebox::Entry ();
use Text::Shoebox ();
###########################################################################

=head1 NAME

Text::Shoebox::Lexicon -- an object-oriented interface to Shoebox lexicons

=head1 SYNOPSIS

  use Text::Shoebox::Lexicon;
  my $lex = Text::Shoebox::Lexicon->read_file( "haida.sf" );
  my @entries = $lex->entries;
  print "See, it has ", scalar( @entries ), " entries!\n";
  $lex->dump;

=head1 DESCRIPTION

XX TODO XX

=head1 METHODS

XX TODO XX

=cut

#
# TODO: accessors for the various I/O options
#
###########################################################################

sub new {
  my $new = bless {},  ref($_[0]) || $_[0];;
  $new->init;
  return $new;
}

sub init {
  my $self = shift;
  $self->{'e'} = [];
}

#--------------------------------------------------------------------------

sub read_file {
  my($self, $in) = @_;
  $self = $self->new unless ref $self;  # tolerate being a class method
  Text::Shoebox::read_sf( 'from_file' => $in, 'into' => $self->{'e'} );
  $self->tidy_up;
  return $self;
}

sub read_handle {
  my($self, $in) = @_;
  $self = $self->new unless ref $self;  # tolerate being a class method
  Text::Shoebox::read_sf( 'from_handle' => $in, 'into' => $self->{'e'} );
  $self->tidy_up;
  return $self;
}

sub write_file {
  my($self, $out) = @_;
  Carp::confess "write_file is an object method, not a class method"
   unless ref $self;
  Text::Shoebox::read_sf( 'to_file' => $out, 'from' => $self->{'e'} );
  return $self;
}

sub write_handle {
  my($self, $out) = @_;
  Carp::confess "write_handle is an object method, not a class method"
   unless ref $self;
  Text::Shoebox::read_sf( 'to_handle' => $out, 'from' => $self->{'e'} );
  return $self;
}

sub dump {
  my($self, $out) = @_;
  Carp::confess "dump is an object method, not a class method"
   unless ref $self;
  print "Lexicon $self contains ", scalar @{ $self->{'e'} }, " entries:\n\n";
  foreach my $e ( @{ $self->{'e'} } ) {
    $e->dump;
  }
  return $self;
}

#--------------------------------------------------------------------------

sub entries {
  my $self = shift;
  return @{ $self->{'e'} } unless @_;
  @{ $self->{'e'} } = @_ ;  # otherwise, be a set method
}

sub tidy_up {
  my $self = $_[0];
  my $entry_class = $self->{'entry_class'} || $Entry_Class;
  foreach my $e (@{ $self->{'e'} }) {
    if( ref($e) eq 'ARRAY' ) {
      bless $e, $entry_class;
      $e->tidy_up unless $self->{'no_tidy'};
    }
  }
  
  return $self;
}

sub entries_as_lol { return $_[0]{'e'} }

#--------------------------------------------------------------------------

1;
__END__


=head1 COPYRIGHT

Copyright 2004, Sean M. Burke C<sburke@cpan.org>, all rights
reserved.  This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=head1 AUTHOR

Sean M. Burke, C<sburke@cpan.org>

I hasten to point out, incidentally, that I am not in any way
affiliated with the Summer Institute of Linguistics.

=cut

