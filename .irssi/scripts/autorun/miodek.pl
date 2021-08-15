# mxl_word v0.1 
use Irssi;
use strict;
use vars qw($VERSION %IRSSI);
$VERSION = "0.2";
%IRSSI = (
        author => "Oskar Baszczyj",
	contact => "mxl on IRCnet #atw",
	name => "mxl-word",
	description => "Simple wordkick system, with extended polish dictionary for channels enforcing correct polish.",
	license => "GPLv2",
	changed => "20.10.2016 21:10"
);


my $mxl_word = '

przejżeć przejrzeć
przejzec przejrzec
wogóle w ogóle

';

my %slowa;
my $ilosc_slow = 0;

foreach my $linia (split(/\n/, $mxl_word)) {
	chomp $linia;
	next if ($linia =~ /^#/ || $linia eq "");

	my ($org, $popraw) = split(/\s+/, $linia, 2);
	$slowa{$org} = $popraw;
	$ilosc_slow++
}

sub server_event {
	my ($server, $data, $nick, $address) = @_;
	my ($type, $data) = split(/ /, $data, 2);
	return unless ($type =~ /privmsg/i);
	my ($target, $tekst) = split(/ :/, $data, 2);
	my $powod;

	$tekst =~ s/[ ]//g;

	foreach my $wyraz (split(/[\s,.;!?\/"`':()_-]/,$tekst)) {
		my $popraw = $slowa{$wyraz};
		if ($popraw) {
			if ($powod) {
				$powod = $powod . ", ";
			}
			$powod = $powod . $popraw;
		}
	}

	if ($powod && $target =~ /^[#!+&]/ ) {
		$server->command("/kick $target $nick $powod");
		Irssi::print "%Rkop%n ($target): %c$nick%n, powod: $powod";
	}
}

Irssi::signal_add_last("server event", "server_event");
