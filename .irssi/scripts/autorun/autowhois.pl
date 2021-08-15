# /WHOIS all the users who send you a private message.
# v1.1 for irssi 0.7.98 by Timo Sirainen
use Irssi;
use vars qw($VERSION %IRSSI); 
$VERSION = "1.1";
%IRSSI = (

);

# History:
#  v1.1: don't /WHOIS if query exists for the nick already

my $lastfrom, $lastquery;

sub msg_private_first {
  my ($server, $msg, $nick, $address) = @_;

  $lastquery = $server->query_find($nick);
}

sub msg_private {
  my ($server, $msg, $nick, $address) = @_;

  return if $lastquery || $lastfrom eq $nick;

  $lastfrom = $nick;
  $server->command("whois $nick");
}

Irssi::signal_add_first('message private', 'msg_private_first');
Irssi::signal_add('message private', 'msg_private');
