##############################################################################
#
# mh_opercount.pl v0.01 (20161029)
#
# Copyright (c) 2016  Michael Hansen
#
# Permission to use, copy, modify, and distribute this software
# for any purpose with or without fee is hereby granted, provided
# that the above copyright notice and this permission notice
# appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL
# THE AUTHOR BE LIABLE FOR  ANY SPECIAL, DIRECT, INDIRECT, OR
# CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
# NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
# CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
##############################################################################
#
# -
#
# quick instructions:
#
#	load script and put $mh_opercount in your format/statusbaritem
#	where you want it. perhaps like:
#	sb_uc_ops = "%R[%KIrcOps %_%W$mh_opercount%c :.: %KOpers %_%W$*%c :.:"
#	it will print 0 or number of opers if on a synced channel and
#	(unless configured otherwise) ? if not synced yet, nothing if
#	 not a channel (see /set mh_opercount).
#
#	it doesnt update very often/fast for me, but if you are using
#	rotator.pl i dont think it matters to you (it basically only
#	updates when the statusbar is redrawn) - this can be changed
#	by forcing updates more often if needed.
#
#	it is very simple and re-calculates every time, this might need
#	to be cached if it turns out to be slow on bigger channels. but
#	thats a problem for later :-)
#
# history:
#
#	v0.01 (20161029)
#		initial pre-release
#

use v5.14.2;

use strict;

##############################################################################
#
# irssi head
#
##############################################################################

use Irssi 20100403;

our $VERSION = '0.01';
our %IRSSI   =
(
	'name'        => 'mh_opercount',
	'description' => '-',
	'license'     => 'BSD',
	'authors'     => 'Michael Hansen',
	'contact'     => 'mh on IRCnet #help',
	'url'         => 'https://github.com/mh-source/irssi-scripts',
	'changed'     => 'Sat Oct 29 04:15:26 CEST 2016',
);

##############################################################################
#
# script functions
#
##############################################################################

sub opercount
{
	my ($channel) = @_;

	my $count = 0;

	for my $nick ($channel->nicks())
	{
		if ($nick->{'serverop'})
		{
			$count++;
		}
	}

	return($count);
}

##############################################################################
#
# irssi expando functions
#
##############################################################################

sub expando_mh_opercount
{
	my ($server, $windowitem) = @_;

	if (ref($windowitem) eq 'Irssi::Irc::Channel')
	{
		if ($windowitem->{'_irssi'} eq Irssi::active_win()->{'active'}->{'_irssi'})
		{
			if ($windowitem->{'synced'})
			{
				return(opercount($windowitem));

			} else {

				return(Irssi::settings_get_str('mh_opercount_count_syncing'));
			}
		}
	}

	return(Irssi::settings_get_str('mh_opercount_count_none'));
}

##############################################################################
#
# script on load
#
##############################################################################

Irssi::settings_add_str('mh_opercount', 'mh_opercount_count_syncing', '?');
Irssi::settings_add_str('mh_opercount', 'mh_opercount_count_none',    '');

Irssi::expando_create('mh_opercount', 'expando_mh_opercount', {});

1;

##############################################################################
#
# eof mh_opercount.pl
#
##############################################################################
