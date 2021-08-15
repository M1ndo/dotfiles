##############################################################################
#
# mh_cyk.pl v0.06 (20161022)
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
# Keep a list of anyone saying a specific word on channel and show the top 10 on request (word, request command and more is configurable)
#
# history:
#
#	v0.06 (20161022)
#		*** please backup the datafile in irssi dir (mh_cyk.data) ***
#		changed data file name/format, it should automatically update and rename
#		made some preparations to run multiple instances of the script with different settings (not complete)
#		fixed allowing just anything as a nick
#		setting _nick_maxlen added with default 15 characters
#		added !drunks-version
#		setting _match_self to enable/disable triggering for client
#		setting_match_action to enable/disable triggering on /me
#		setting _name used in messages as [Top _name ...]'
#		setting _data_filename name of data file (relative to irssi dir)
#		added a script description
#
#	v0.05 (20161020)
#		much code rearranged neatly and new ugly hacks added
#		setting command_list changed to exlude ! prefix
#		setting command_char added (with "!")
#		setting command_pos (on) to enable/disable !drunks <nick> feature
#
#	v0.04 (20161020)
#		accept match/command on actions too (/me)
#		setting no_hilight to enable/disable not hilighting nicknames (longer than 1 char)
#		setting show_channelname to enable/disable showing the channelname in the top-list
#		setting show_count to enable/disable the wordcount next to each nick in top-list
#
#	v0.03 (20161018)
#		fix 'keys on reference is experimental' warnings
#
#	v0.02 (20161018)
#		accept own messages too
#		automatic save/load data to/from a plain text file (mh_cyk.data) in the irssi dir
#
#	v0.01 (20161017)
#		initial pre-release
#

use v5.14.2;

use strict;
use File::Path qw(make_path remove_tree);

##############################################################################
#
# irssi head
#
##############################################################################

use Irssi 20100403;

{ package Irssi::Nick }

our $VERSION = '0.06';
our %IRSSI   =
(
	'name'        => 'mh_cyk',
	'description' => 'Keep a list of anyone saying a specific word on channel and show the top 10 on request (word, request command and more is configurable)',
	'license'     => 'BSD',
	'authors'     => 'Michael Hansen',
	'contact'     => 'mh on IRCnet #help',
	'url'         => 'https://github.com/mh-source/irssi-scripts',
	'changed'     => 'Sat Oct 22 18:04:15 CEST 2016',
);

##############################################################################
#
# global variables
#
##############################################################################

our $list;
our $data_save_timeout = 0;

##############################################################################
#
# common support functions
#
##############################################################################

sub trim_space
{
	my ($string) = @_;

	if (defined($string))
	{
		$string =~ s/^\s+//g;
		$string =~ s/\s+$//g;

	} else
	{
		$string = '';
	}

	return($string);
}

sub time_string
{
	my ($seconds, $longformat) = @_;

	my $string    = '';
	my $seconds_w = int($seconds / 604800);
	$seconds      = $seconds - ($seconds_w * 604800);
	my $seconds_d = int($seconds / 86400);
	$seconds      = $seconds - ($seconds_d * 86400);
	my $seconds_h = int($seconds / 3600);
 	$seconds      = $seconds - ($seconds_h * 3600);
	my $seconds_m = int($seconds / 60);
	$seconds      = $seconds - ($seconds_m * 60);
	my $always    = 0;
	my $string_w  = 'w';
	my $string_d  = 'd';
	my $string_h  = 'h';
	my $string_m  = 'm';
	my $string_s  = 's';

	if ($longformat)
	{
		$string_w  = ' weeks ';
		$string_d  = ' days ';
		$string_h  = ' hours ';
		$string_m  = ' mins ';
		$string_s  = ' secs';
	}

	if ($seconds_w or $always)
	{
		$string = $string . $seconds_w . $string_w;
		$always = 1;
	}

	if ($seconds_d or $always)
	{
		$string = $string . $seconds_d . $string_d;
		$always = 1;
	}

	if ($seconds_h or $always)
	{
		$string = $string . $seconds_h . $string_h;
		$always = 1;
	}

	if ($seconds_m or $always)
	{
		$string = $string . $seconds_m . $string_m;
		$always = 1;
	}

	if ($seconds or $always)
	{
		$string = $string . $seconds . $string_s;

	} else
	{
		#
		# we have zero seconds
		#
		$string = '0' . $string_s;
	}

	return($string);
}

##############################################################################
#
# script functions
#
##############################################################################

sub list_inc
{
	my ($servertag, $channelname, $nick) = @_;

	my $nick_lc = lc($nick);

	if (exists($list->{$servertag}->{$channelname}->{$nick_lc}))
	{
		$list->{$servertag}->{$channelname}->{$nick_lc}->{'count'}++;

	} else {

		$list->{$servertag}->{$channelname}->{$nick_lc}->{'nick'}  = $nick;
		$list->{$servertag}->{$channelname}->{$nick_lc}->{'count'} = 1;
	}

	if (not $data_save_timeout)
	{
		$data_save_timeout = Irssi::timeout_add_once(5*60000, 'data_save', undef);
	}
}

sub list_command_version
{
	my ($servertag, $channelname) = @_;

	my $channel = Irssi::server_find_tag($servertag)->channel_find($channelname);

	$channel->command('SAY [mh_cyk.pl v' . $VERSION . ' (' . time_string(time() - $^T) . ') ' . $IRSSI{'url'} . ']');

	return(1);
}

sub list_command_toplist
{
	my ($servertag, $channelname, $nick) = @_;

	my $count_max = 10;
	my $reply     = '[Top ' . $count_max . ' ' . Irssi::settings_get_str('mh_cyk_name');
	my $channel   = Irssi::server_find_tag($servertag)->channel_find($channelname);

	if (Irssi::settings_get_bool('mh_cyk_show_channelname'))
	{
		$reply .= ' on ' . $channel->{'visible_name'};
	}

	$reply .= ': ';

	if (not exists($list->{$servertag}->{$channelname}))
	{
		$reply .= 'No data';

	} else {

		my $count     = 0;

		for my $user (sort { $list->{$servertag}->{$channelname}->{$b}->{'count'} <=>
		                     $list->{$servertag}->{$channelname}->{$a}->{'count'} }
                             keys(%{$list->{$servertag}->{$channelname}}))
		{
			$count++;

			if ($count > 1)
			{
				$reply .= ', '
			}


			my $nickname = $list->{$servertag}->{$channelname}->{$user}->{'nick'};

			if (Irssi::settings_get_bool('mh_cyk_no_hilight'))
			{
				$nickname =~ s/(.)(.*)?/$1\x02\x02$2/;
			}

			$reply .= chr(0x02) . $count . '. ' . chr(0x02) . $nickname;

			if (Irssi::settings_get_bool('mh_cyk_show_count'))
			{
				$reply .= ' (' . $list->{$servertag}->{$channelname}->{$user}->{'count'} . ')';
			}

			if ($count >= $count_max)
			{
				last;
			}
		}
	}

	$reply .= ']';

	$channel->command('SAY ' . $reply);

	return(1);
}

sub list_command_poslist
{
	my ($servertag, $channelname, $nick) = @_;

	($nick, undef) = split(/ /, $nick, 2);
	$nick = trim_space($nick);

	my $nick_maxlen = Irssi::settings_get_int('mh_cyk_nick_maxlen');

	if (length($nick) >= $nick_maxlen)
	{
		$nick = substr($nick, 0, $nick_maxlen);
	}

	my $reply   = '[Top ' . Irssi::settings_get_str('mh_cyk_name') . ' position';
	my $channel = Irssi::server_find_tag($servertag)->channel_find($channelname);

	if ($nick ne '')
	{
		$reply .= ' for ' . $nick;

		if (Irssi::settings_get_bool('mh_cyk_show_channelname'))
		{
			$reply .= ' on ' . $channel->{'visible_name'};
		}

		$reply .= ': ';

		if (not exists($list->{$servertag}->{$channelname}))
		{
			$reply .= 'No data';

		} elsif (not exists($list->{$servertag}->{$channelname}->{lc($nick)}))
		{
			$reply .= 'Not found';

		} else {

			my $found     = 0;
			my $count     = 0;
			my $user_prev = '';

			for my $user (sort { $list->{$servertag}->{$channelname}->{$b}->{'count'} <=>
			                     $list->{$servertag}->{$channelname}->{$a}->{'count'} }
            	                 keys(%{$list->{$servertag}->{$channelname}}))
			{
				$count++;

				if ($found)
				{
					my $nickname = $list->{$servertag}->{$channelname}->{$user}->{'nick'};

					if (Irssi::settings_get_bool('mh_cyk_no_hilight'))
					{
						$nickname =~ s/(.)(.*)?/$1\x02\x02$2/;
					}

					$reply .= ', ' . chr(0x02) . $count . '. ' . chr(0x02) . $nickname;

					if (Irssi::settings_get_bool('mh_cyk_show_count'))
					{
						$reply .= ' (' . $list->{$servertag}->{$channelname}->{$user}->{'count'} . ')';
					}

					last;
				}

				if (lc($nick) eq $user)
				{
					$found = 1;

					if ($user_prev ne '')
					{
						my $nickname = $list->{$servertag}->{$channelname}->{$user_prev}->{'nick'};

						if (Irssi::settings_get_bool('mh_cyk_no_hilight'))
						{
							$nickname =~ s/(.)(.*)?/$1\x02\x02$2/;
						}

						$reply .= chr(0x02) . ($count - 1) . '. ' . chr(0x02) . $nickname;

						if (Irssi::settings_get_bool('mh_cyk_show_count'))
						{
							$reply .= ' (' . $list->{$servertag}->{$channelname}->{$user_prev}->{'count'} . ')';
						}

						$reply .= ', ';
					}

					my $nickname = $list->{$servertag}->{$channelname}->{$user}->{'nick'};

					if (Irssi::settings_get_bool('mh_cyk_no_hilight'))
					{
						$nickname =~ s/(.)(.*)?/$1\x02\x02$2/;
					}

					$reply .= chr(0x02) . $count . '. ' . $nickname . chr(0x02);

					if (Irssi::settings_get_bool('mh_cyk_show_count'))
					{
						$reply .= ' (' . $list->{$servertag}->{$channelname}->{$user}->{'count'} . ')';
					}

				} else {

					$user_prev = $user;
				}
			}
		}

	} else {

		$reply .= ' not found';
	}

	$reply .= ']';

	$channel->command('SAY ' . $reply);

	return(1);
}

sub data_load
{
	my ($oldfilename) = @_; # remove!

	my $filepath = Irssi::get_irssi_dir();
	my $filename = Irssi::settings_get_str('mh_cyk_data_filename');

	$filename = trim_space($filename);

	if ($filename eq '')
	{
		return(-1);
	}

	if ($oldfilename) { $filename = $oldfilename } # remove!

	my $file = $filepath . '/' . $filename;

	if ($data_save_timeout)
	{
		Irssi::timeout_remove($data_save_timeout);
		$data_save_timeout = 0;
	}

    if (open(my $filehandle, '<:encoding(UTF-8)' , $file))
    {
		$list         = undef;
		my $validfile = 0;

        while (my $data = <$filehandle>)
        {
            chomp($data);

			if ($data =~ m/^\s*#/)
			{
				if (not $validfile)
				{
					trim_space($data);
					$validfile = 1; 
				}

			} elsif ($data =~ m/(..*);(..*);(..*);(..*);.*/)
			{
				if (not $validfile)
				{
					if ($oldfilename ne $filename) # remove!
					{
						$list = undef;
						return(-1);
					}
				}

				my $nick_lc = lc($3);

				$list->{$1}->{$2}->{$nick_lc}->{'nick'}  = $3;
				$list->{$1}->{$2}->{$nick_lc}->{'count'} = int($4);

			} else {

				if ($data !~ m/^\s*$/)
				{
					$list = undef;
					return(-1);
				}
			}
		}

		close($filehandle);

		if ($oldfilename) # remove!
		{
			data_save();
			print('mh_cyk: Renamed ' . $filepath . '/' . $oldfilename . ' to ' . Irssi::settings_get_str('mh_cyk_data_filename'));
			print('mh_cyk: That is a good thing and supposed to happen this once.');
			unlink($file);
		}

		return(1);

	} else {

		if (not $oldfilename) # remove!
		{
			data_load('mh_cyk.data');
		}
	}

	return(1);
}

sub data_save
{
	my $filepath = Irssi::get_irssi_dir();
	my $filename = Irssi::settings_get_str('mh_cyk_data_filename');

	$filename = trim_space($filename);

	if ($filename eq '')
	{
		return(-1);
	}

	my $file = $filepath . '/' . $filename;

	make_path($filepath);

	if ($data_save_timeout)
	{
		Irssi::timeout_remove($data_save_timeout);
		$data_save_timeout = 0;
	}

	if (open(my $filehandle, '>:encoding(UTF-8)' , $file))
	{
		print($filehandle "#\n# " . $filename  . ' - mh_cyk.pl v' . $VERSION . "\n#\n#   Autogenerated, do not edit!\n#\n");

		for my $server (keys(%{$list}))
		{
			for my $channel (keys(%{$list->{$server}}))
			{

				for my $nick_lc (keys(%{$list->{$server}->{$channel}}))
				{
					my $nick  = $list->{$server}->{$channel}->{$nick_lc}->{'nick'};
					my $count = $list->{$server}->{$channel}->{$nick_lc}->{'count'};

					print($filehandle $server . ';' . $channel . ';' . $nick . ';' . $count  . ";\n");
				}
			}
		}

		close($filehandle);
    }

	return(1);
}

##############################################################################
#
# irssi signal handlers
#
##############################################################################

sub signal_message_public_priority_low
{
	my ($server, $data, $nick, $address, $target) = @_;

	my $servertag    = lc($server->{'tag'});
	my $channelname  = lc($target);
	my $match        = Irssi::settings_get_str('mh_cyk_match');

	for my $serverchannel (split(',', Irssi::settings_get_str('mh_cyk_channels')))
	{
		$serverchannel = lc(trim_space($serverchannel));

		if ($serverchannel eq $servertag . '/' . $channelname)
		{
			if ($data =~ m/.*\b\Q$match\E\b.*/i)
			{
				list_inc($servertag, $channelname, $nick);
			}

			my $command_char = Irssi::settings_get_str('mh_cyk_command_char');

			if ($data =~ m/^\Q$command_char\E(..*)/)
			{
				my $command = $1;
				$match      = lc(Irssi::settings_get_str('mh_cyk_command_list'));

				if ($command =~ m/\Q$match\E-VERSION\b(.*)?$/i)
				{
					return(list_command_version($servertag, $channelname));
				}

				if ($command =~ m/\Q$match\E(\s.*)?$/i)
				{
					$data = trim_space($1);

					if (($data eq '') or (not Irssi::settings_get_bool('mh_cyk_command_pos')))
					{
						return(list_command_toplist($servertag, $channelname));
					}

					return(list_command_poslist($servertag, $channelname, $data));
				}
			}

			return(1);
		}
	}
}

sub signal_message_irc_action_priority_low
{
	my ($server, $data, $nick, $address, $target) = @_;

	if (Irssi::settings_get_bool('mh_cyk_match_action'))
	{
		return(signal_message_public_priority_low($server, $data, $nick, $address, $target));
	}

	return(1);
}

sub signal_message_own_public_priority_low
{
	my ($server, $data, $target) = @_;

	if (Irssi::settings_get_bool('mh_cyk_match_self'))
	{
		return(signal_message_public_priority_low($server, $data, $server->{'nick'}, undef, $target));
	}

	return(1);
}

sub signal_message_irc_own_action_priority_low
{
	my ($server, $data, $target) = @_;

	if (Irssi::settings_get_bool('mh_cyk_match_self') and Irssi::settings_get_bool('mh_cyk_match_action'))
	{
		return(signal_message_public_priority_low($server, $data, $server->{'nick'}, undef, $target));
	}

	return(1);
}

sub signal_gui_exit_last
{
	if ($list)
	{
		if ($data_save_timeout)
		{
			data_save();
		}
	}
}

##############################################################################
#
# script on load
#
##############################################################################

Irssi::settings_add_str('mh_cyk',  'mh_cyk_channels',         '');
Irssi::settings_add_str('mh_cyk',  'mh_cyk_match',            'cyk');
Irssi::settings_add_str('mh_cyk',  'mh_cyk_command_char',     '!');
Irssi::settings_add_str('mh_cyk',  'mh_cyk_command_list',     'drunks');
Irssi::settings_add_bool('mh_cyk', 'mh_cyk_command_pos',      1);
Irssi::settings_add_bool('mh_cyk', 'mh_cyk_no_hilight',       0);
Irssi::settings_add_bool('mh_cyk', 'mh_cyk_show_channelname', 1);
Irssi::settings_add_bool('mh_cyk', 'mh_cyk_show_count',       1);
Irssi::settings_add_int('mh_cyk',  'mh_cyk_nick_maxlen',      15);
Irssi::settings_add_bool('mh_cyk', 'mh_cyk_match_action',     1);
Irssi::settings_add_bool('mh_cyk', 'mh_cyk_match_self',       1);
Irssi::settings_add_str('mh_cyk',  'mh_cyk_name',             Irssi::settings_get_str('mh_cyk_command_list'));
Irssi::settings_add_str('mh_cyk',  'mh_cyk_data_filename',    'mh_cyk.' . Irssi::settings_get_str('mh_cyk_name') . '.data');

Irssi::signal_add_priority('message public',         'signal_message_public_priority_low',         Irssi::SIGNAL_PRIORITY_LOW + 1);
Irssi::signal_add_priority('message irc action',     'signal_message_irc_action_priority_low',     Irssi::SIGNAL_PRIORITY_LOW + 1);
Irssi::signal_add_priority('message own_public',     'signal_message_own_public_priority_low',     Irssi::SIGNAL_PRIORITY_LOW + 1);
Irssi::signal_add_priority('message irc own_action', 'signal_message_irc_own_action_priority_low', Irssi::SIGNAL_PRIORITY_LOW + 1);
Irssi::signal_add_last('gui exit',                   'signal_gui_exit_last');

data_load();

1;

##############################################################################
#
# eof mh_cyk.pl
#
##############################################################################
