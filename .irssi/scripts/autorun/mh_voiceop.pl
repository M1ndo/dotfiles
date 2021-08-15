##############################################################################
#
# mh_voiceop.pl v1.00 (20151231)
#
# Copyright (c) 2015  Michael Hansen
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
# allow voiced users to perform channel commands like kick, ban, mode, etc
#
# When you have configured this script for one or more channels, it
# will look for public commands and execute them
#
# commands (can be individually enabled/disabled with settings):
#
# !BAN <nick> [<reason...>]
# !DEVOICE [<nick>]
# !HELP
# !INVITE <nick>
# !KICK <nick> [<reason...>]
# !MODE [+|-]<mode (iklmnpst)>
# !TOPIC [<topic...>]
# !UNBAN <mask>|<number>
# !VOICE <nick>
#
# settings:
#
# mh_voiceop_channels (default ''): a commas-separated list of 'network/channel'
# where voiceop is enabled (for example: IRCnet/#channel1,Othernet/#channel2)
#
# mh_voiceop_command_char (default '!'): the command prefix for public
# commands. this can also be a string
#
# mh_voiceop_opt_kick (default ON): allow/disallow voiced user to kick
# regular users (!KICK)
#
# mh_voiceop_opt_voice (default OFF): allow/disallow voiced users to voice
# regular users (!VOICE)
#
# mh_voiceop_opt_devoice (default ON): allow/disallow voiced users to
# devoice themself and optionally (see mh_voiceop_devoice_all) other voiced
# users (!DEVOICE)
#
# mh_voiceop_opt_ban (default ON): allow/disallow voiced users to ban
# regular users (!BAN) (see also mh_voiceop_ban_time)
#
# mh_voiceop_opt_unban (default OFF): allow/disallow voiced users to unban
# masks in the banlist (!UNBAN)
#
# mh_voiceop_opt_mode_m (default OFF): allow/disallow voiced users to
# (un)set channelmode 'm' (!MODE)
#
# mh_voiceop_opt_mode_t (default OFF): allow/disallow voiced users to
# (un)set channelmode 't' (!MODE)
#
# mh_voiceop_opt_mode_i (default OFF): allow/disallow voiced users to
# (un)set channelmode 'i' (!MODE)
#
# mh_voiceop_opt_mode_s (default OFF): allow/disallow voiced users to
# (un)set channelmode 's' (!MODE)
#
# mh_voiceop_opt_mode_p (default OFF): allow/disallow voiced users to
# (un)set channelmode 'p' (!MODE)
#
# mh_voiceop_opt_mode_l (default OFF): allow/disallow voiced users to
# (un)set channelmode 'l' (!MODE)
#
# mh_voiceop_opt_mode_n (default OFF): allow/disallow voiced users to
# (un)set channelmode 'n' (!MODE)
#
# mh_voiceop_opt_mode_k (default OFF): allow/disallow voiced users to
# (un)set channelmode 'k' (!MODE)
#
# mh_voiceop_opt_topic (default ON): allow/disallow voiced users to
# change the topic when channel is +t (!TOPIC)
#
# mh_voiceop_opt_invite (default OFF): allow/disallow voiced users to
# invite others to the channel (see also mh_voiceop_invite_always)
#
# mh_voiceop_opt_help (default ON): allow/disallow voiced users to
# request help
#
# mh_voiceop_devoice_all (default OFF): enable/disable voiced users
# to !devoice <nick>. if off, the command is simply !devoice and will
# only devoice the user themself
#
# mh_voiceop_lag_limit (default 5): maximum lag (in seconds) allowed
# before we  start ignoring public commands
#
# mh_voiceop_invite_always (default OFF): normally we only allow
# !INVITE when the channel is +i, but if your voiced users might need
# to override bans, you can enable this and !INVITE will work even if
# channel is -i
#
# mh_voiceop_ban_time (default 60): time (in minutes) for bans set
# with !BAN, after the time is up we unban again
#
# mh_voiceop_kick_no_userreason (default OFF): enable/disable allowing
# users to add their own !kick reason
#
# mh_voiceop_ban_no_userreason (default OFF): enable/disable allowing
# users to add their own !ban reason
#
# mh_voiceop_kick_show_who (default ON): enable/disable showing
# "Kick requested by..."/"Requested by..." in !kick
#
# mh_voiceop_ban_show_who (default ON): enable/disable showing
# "Ban requested by..."/"Requested by..." in !ban
#
# mh_voiceop_kick_show_type (default ON): enable/disable showing
# "Requested by..." instead of "Requested by..." in !kick
#
# mh_voiceop_ban_show_type (default ON): enable/disable showing
# "Ban requested by..." instead of "Requested by..." in !ban
#
# mh_voiceop_kick_default_reason (default ''): set/unset a default
# kick-reason when none is given in !kick
#
# mh_voiceop_ban_default_reason (default ''): set/unset a default
# ban-reason when none is given in !ban
#
# mh_voiceop_autolimit (default 10): when +l is allowed and a user
# requests "+l" (wihout argument) or "+l 0", set the limit to current
# users plus this value. set to 0 to turn off
#
# mh_voiceop_help_notice (default ON): when requesting !HELP reply
# with a notice (if disabled, use a private message)
#
# mh_voiceop_help_on_voice (default ON): send !help when someone
# is voiced (this requires mh_voiceop_opt_help)
#
# mh_voiceop_help_show_prefix (default ON): add a notice to !help
# showing the current command_char
#
# mh_voiceop_help_header (default 'Hello %NICK%, welcome as voice on %CHANNEL%'):
# set/unset a header text to !help reply (supports 2 variables: %NICK%
# (the requesters nickname) and %CHANNEL% (the channel name).
# variables are case-sensitive)
#
# mh_voiceop_help_footer (default 'Please use responsibly'): set/unset
# a footer text to !help reply (supports the same variables as
# mh_voiceop_help_header)
#
# history:
#	v1.00 (20151231)
#		initial release
#

use v5.14.2;

use strict;

##############################################################################
#
# irssi head
#
##############################################################################

use Irssi 20100403;

{ package Irssi::Nick }

our $VERSION = '1.00';
our %IRSSI   =
(
	'name'        => 'mh_voiceop',
	'description' => 'allow voiced users to perform channel commands like kick, ban, mode, etc',
	'license'     => 'BSD',
	'authors'     => 'Michael Hansen',
	'contact'     => 'mh on IRCnet #help',
	'url'         => 'https://github.com/mh-source/irssi-scripts',
	'changed'     => 'Thu Dec 31 14:13:09 CET 2015',
);

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

##############################################################################
#
# script functions
#
##############################################################################

sub get_mode_chars
{
	my $modes = '';

	if (Irssi::settings_get_bool('mh_voiceop_opt_mode_i'))
	{
		$modes = $modes . 'i';
	}

	if (Irssi::settings_get_bool('mh_voiceop_opt_mode_k'))
	{
		$modes = $modes . 'k';
	}

	if (Irssi::settings_get_bool('mh_voiceop_opt_mode_l'))
	{
		$modes = $modes . 'l';
	}

	if (Irssi::settings_get_bool('mh_voiceop_opt_mode_m'))
	{
		$modes = $modes . 'm';
	}

	if (Irssi::settings_get_bool('mh_voiceop_opt_mode_n'))
	{
		$modes = $modes . 'n';
	}

	if (Irssi::settings_get_bool('mh_voiceop_opt_mode_p'))
	{
		$modes = $modes . 'p';
	}

	if (Irssi::settings_get_bool('mh_voiceop_opt_mode_s'))
	{
		$modes = $modes . 's';
	}

	if (Irssi::settings_get_bool('mh_voiceop_opt_mode_t'))
	{
		$modes = $modes . 't';
	}

	return($modes);
}

sub parse_variables
{
	my ($data, $nick, $channel) = @_;

	if ($data ne '')
	{
		$data =~ s/%NICK%/$nick/g;
		$data =~ s/%CHANNEL%/$channel/g;
	}

	return($data);
}

##############################################################################
#
# irssi signal handlers
#
##############################################################################

sub signal_message_public_last
{
	my ($server, $data, $nickname, $address, $target) = @_;

	my $lag_limit = Irssi::settings_get_int('mh_voiceop_lag_limit');

	if ($lag_limit)
	{
		$lag_limit = $lag_limit * 1000; # seconds to milliseconds

		if ($server->{'lag'} >= $lag_limit)
		{
			return(0);
		}
	}

	my $servertag   = lc($server->{'tag'});
	my $channelname = lc($target);

	for my $serverchannel (split(',', Irssi::settings_get_str('mh_voiceop_channels')))
	{
		$serverchannel = lc(trim_space($serverchannel));

		if ($serverchannel eq $servertag . '/' . $channelname)
		{
			my $channel = $server->channel_find($channelname);

			if ($channel)
			{
				if ((not $channel->{'synced'}) or (not $channel->{'chanop'}))
				{
					return(0);
				}

				my $nick = $channel->nick_find($nickname);

				if ($nick)
				{
					if ($nick->{'voice'} and (not $nick->{'op'}))
					{
						my $command_char = Irssi::settings_get_str('mh_voiceop_command_char');

						if ($data =~ s/^\Q$command_char\E//)
						{
							(my $command, $data) = split(' ', $data, 2);
							$data = trim_space($data);

							if ($command =~ m/^k(i(c(k)?)?)?$/i)
							{
								if (Irssi::settings_get_bool('mh_voiceop_opt_kick'))
								{
									(my $kicknickname, $data) = split(' ', $data, 2);

									if (Irssi::settings_get_bool('mh_voiceop_kick_no_userreason'))
									{
										$data = '';

									} else
									{
										$data = trim_space($data);
									}

									my $kicknick = $channel->nick_find($kicknickname);

									if ($kicknick)
									{
										if ((not $kicknick->{'op'}) and (not $kicknick->{'halfop'}) and (not $kicknick->{'voice'}))
										{
											if ($data eq '')
											{
												my $reason = Irssi::settings_get_str('mh_voiceop_kick_default_reason');

												if ($reason ne '')
												{
													$data = $reason;
												}
											}

											my $requested_by = '';

											if (Irssi::settings_get_bool('mh_voiceop_kick_show_who'))
											{
												if (Irssi::settings_get_bool('mh_voiceop_kick_show_type'))
												{
													$requested_by = $requested_by . 'Kick requested by';

												} else
												{
													$requested_by = $requested_by . 'Requested by';
												}

												$requested_by = $requested_by . ' ' . $nick->{'nick'};

												if ($data ne '')
												{
													$requested_by = $requested_by . ': ';
												}
											}

											$channel->command('^QUOTE KICK ' . $channel->{'name'} . ' ' . $kicknick->{'nick'} . ' :' . $requested_by . $data)
										}
									}
								}

								last;

							} elsif ($command =~ m/^b(a(n)?)?$/i)
							{
								if (Irssi::settings_get_bool('mh_voiceop_opt_ban'))
								{
									(my $bannickname, $data) = split(' ', $data, 2);

									if (Irssi::settings_get_bool('mh_voiceop_ban_no_userreason'))
									{
										$data = '';

									} else
									{
										$data = trim_space($data);
									}

									my $bannick = $channel->nick_find($bannickname);

									if ($bannick)
									{
										if ((not $bannick->{'op'}) and (not $bannick->{'halfop'}) and (not $bannick->{'voice'}))
										{
											if ($data eq '')
											{
												my $reason = Irssi::settings_get_str('mh_voiceop_ban_default_reason');

												if ($reason ne '')
												{
													$data = $reason;
												}
											}

											my $bantime = Irssi::settings_get_int('mh_voiceop_ban_time');

											if (not $bantime)
											{
												$bantime = 1;
											}

											$bantime = $bantime * 60; # minutes to seconds

											my $requested_by = '';

											if (Irssi::settings_get_bool('mh_voiceop_ban_show_who'))
											{
												if (Irssi::settings_get_bool('mh_voiceop_ban_show_type'))
												{
													$requested_by = $requested_by . 'Ban requested by';

												} else
												{
													$requested_by = $requested_by . 'Requested by';
												}

												$requested_by = $requested_by . ' ' . $nick->{'nick'};

												if ($data ne '')
												{
													$requested_by = $requested_by . ': ';
												}
											}

											$channel->command('^KNOCKOUT ' . $bantime . ' ' . $bannick->{'nick'} . ' ' . $requested_by . $data)
										}
									}
								}

								last;

							} elsif ($command =~ m/^u(n(b(a(n)?)?)?)?$/i)
							{
								if (Irssi::settings_get_bool('mh_voiceop_opt_unban'))
								{
									(my $banmask, $data) = split(' ', $data, 2);

									if ($banmask =~ m/^[0-9]*$/)
									{
										$banmask = int($banmask);

										if ($banmask)
										{
											my $maxban_count = 0;

											for my $ban ($channel->bans())
											{
												$maxban_count++;
											}

											if ($maxban_count and ($maxban_count >= $banmask))
											{
												$channel->command('^UNBAN ' . $banmask);
											}
										}

									} elsif ($banmask)
									{
										for my $ban ($channel->bans())
										{
											if (lc($banmask) eq lc($ban->{'ban'}))
											{
												$channel->command('^UNBAN ' . $ban->{'ban'});
											}
										}
									}
								}

								last;

							} elsif ($command =~ m/^m(o(d(e)?)?)?$/i)
							{
								my $modes_allowed = get_mode_chars();

								if ($modes_allowed ne '')
								{
									if ($data =~ m/^([+-][$modes_allowed])(\s*.*)?$/)
									{
										$data   = $1;
										my $arg = trim_space($2);

										if ($data =~ m/^\+l$/)
										{
											($arg, undef) = split(' ', $arg, 2);

											if ($arg)
											{
												if (not $arg =~ m/^[0-9]*$/)
												{
													return(0);
												}

												$arg = int($arg);

												if (not $arg)
												{
													return(0);
												}

												if ($channel->{'limit'} == $arg)
												{
													return(0);
												}

												$arg = ' ' . $arg;

											} else
											{
												if (not Irssi::settings_get_int('mh_voiceop_autolimit'))
												{
													return(0);
												}

												my $users = 0;

												for my $nick ($channel->nicks())
												{
													$users++;
												}

												$users = $users + Irssi::settings_get_int('mh_voiceop_autolimit');

												if ($users == $channel->{'limit'})
												{
													return(0);
												}

												$arg = ' ' . $users;
											}

										} elsif($data =~ m/^\+k$/)
										{
											($arg, undef) = split(' ', $arg, 2);

											$arg = trim_space($arg);

											if ($arg eq '')
											{
												return(0);
											}

											if ($channel->{'key'} eq $arg)
											{
												return(0);
											}

											$arg = ' ' . $arg;

										} else
										{
											$arg = '';
										}

										$channel->command('^MODE ' . $data . $arg);
									}
								}

								last;

							} elsif ($command =~ m/^t(o(p(i(c)?)?)?)?$/i)
							{
								if (Irssi::settings_get_bool('mh_voiceop_opt_topic'))
								{
									if ($channel->{'mode'} =~ m/t/)
									{
										if ($channel->{'topic'} ne $data)
										{
											if ($data ne '')
											{
												$channel->command('^QUOTE TOPIC ' . $channel->{'name'} . ' :' . $data);

											} else
											{
												$channel->command('^TOPIC -delete');
											}
										}
									}
								}

								last;

							} elsif ($command =~ m/^i(n(v(i(t(e)?)?)?)?)?$/i)
							{
								if (Irssi::settings_get_bool('mh_voiceop_opt_invite'))
								{
									if (($channel->{'mode'} =~ m/i/) or Irssi::settings_get_bool('mh_voiceop_invite_always'))
									{
										$data = trim_space($data);

										if ($data ne '')
										{
											(my $invitenickname, $data) = split(' ', $data,           2);
											($invitenickname, undef)    = split(',', $invitenickname, 2);

											if ($server->ischannel($invitenickname))
											{
												return(0);
											}

											my $invitenick = $channel->nick_find($invitenickname);

											if (($invitenickname ne '') and (not $invitenick))
											{
												$channel->command('^QUOTE INVITE ' . $invitenickname . ' ' . $channel->{'name'});
											}
										}
									}
								}

								last;

							} elsif ($command =~ m/^v(o(i(c(e)?)?)?)?$/i)
							{
								if (Irssi::settings_get_bool('mh_voiceop_opt_voice'))
								{
									(my $voicenickname, $data) = split(' ', $data, 2);

									my $voicenick = $channel->nick_find($voicenickname);

									if ($voicenick)
									{
										if ((not $voicenick->{'op'}) and (not $voicenick->{'halfop'}) and (not $voicenick->{'voice'}))
										{
											$channel->command('^VOICE ' . $voicenick->{'nick'});
										}
									}
								}

								last;

							} elsif ($command =~ m/^d(e(v(o(i(c(e)?)?)?)?)?)?$/i)
							{
								if (Irssi::settings_get_bool('mh_voiceop_opt_devoice'))
								{
									(my $devoicenickname, $data) = split(' ', $data, 2);

									$devoicenickname = trim_space($devoicenickname);

									if (not Irssi::settings_get_bool('mh_voiceop_devoice_all'))
									{
										if ($devoicenickname ne '')
										{
											return(0);
										}
									}

									if ($devoicenickname eq '')
									{
										$devoicenickname = $nick->{'nick'};
									}

									my $devoicenick = $channel->nick_find($devoicenickname);

									if ($devoicenick)
									{
										if ((not $devoicenick->{'op'}) and (not $devoicenick->{'halfop'}) and ($devoicenick->{'voice'}))
										{
											$channel->command('^DEVOICE ' . $devoicenick->{'nick'});
										}
									}
								}

								last;

							} elsif ($command =~ m/^h(e(l(p)?)?)?$/i)
							{
								if (Irssi::settings_get_bool('mh_voiceop_opt_help'))
								{
									push(my @commands, 'HELP');

									if (Irssi::settings_get_bool('mh_voiceop_opt_kick'))
									{
										if (Irssi::settings_get_bool('mh_voiceop_kick_no_userreason'))
										{
											push(@commands, 'KICK <nick>');

										} else
										{
											push(@commands, 'KICK <nick> [<reason...>]');
										}
									}

									if (Irssi::settings_get_bool('mh_voiceop_opt_ban'))
									{
										if (Irssi::settings_get_bool('mh_voiceop_ban_no_userreason'))
										{
											push(@commands, 'BAN <nick>');

										} else
										{
											push(@commands, 'BAN <nick> [<reason...>]');
										}
									}

									my $modes_allowed = get_mode_chars();

									if ($modes_allowed ne '')
									{
										$modes_allowed = join('', sort(split('', $modes_allowed)));

										push(@commands, 'MODE [+|-]<mode (' . $modes_allowed . ')>');
									}

									if (Irssi::settings_get_bool('mh_voiceop_opt_topic'))
									{
										push(@commands, 'TOPIC [<topic...>]');
									}

									if (Irssi::settings_get_bool('mh_voiceop_opt_invite'))
									{
										push(@commands, 'INVITE <nick>');
									}

									if (Irssi::settings_get_bool('mh_voiceop_opt_voice'))
									{
										push(@commands, 'VOICE <nick>');
									}

									if (Irssi::settings_get_bool('mh_voiceop_opt_devoice'))
									{
										if (Irssi::settings_get_bool('mh_voiceop_devoice_all'))
										{
											push(@commands, 'DEVOICE [<nick>]');

										} else
										{
											push(@commands, 'DEVOICE');
										}

									}

									if (Irssi::settings_get_bool('mh_voiceop_opt_unban'))
									{
										push(@commands, 'UNBAN <mask>|<number>');
									}

									my $arraysize = @commands;

									if (not $arraysize)
									{
										return(0);
									}

									$data = $nick->{'nick'} . ' Available public commands on ' . $target;

									if (Irssi::settings_get_bool('mh_voiceop_help_show_prefix'))
									{
										$data = $data . ' (prefix with \'' . $command_char . '\')';
									}

									my $header = '';

									if (Irssi::settings_get_str('mh_voiceop_help_header') ne '')
									{
										$header = $nick->{'nick'} . ' ' . parse_variables(Irssi::settings_get_str('mh_voiceop_help_header'), $nick->{'nick'}, $channel->{'visible_name'});
									}

									my $footer = '';

									if (Irssi::settings_get_str('mh_voiceop_help_footer') ne '')
									{
										$footer = $nick->{'nick'} . ' ' . parse_variables(Irssi::settings_get_str('mh_voiceop_help_footer'), $nick->{'nick'}, $channel->{'visible_name'});
									}

									$data = $data . ': ' . join(', ', sort(@commands)) . '.';

									if (Irssi::settings_get_bool('mh_voiceop_help_notice'))
									{
										if ($header ne '')
										{
											$channel->command('^NOTICE ' . $header);
										}

										$channel->command('^NOTICE ' . $data);

										if ($footer ne '')
										{
											$channel->command('^NOTICE ' . $footer);
										}

									} else
									{
										if ($header ne '')
										{
											$channel->command('^MSG ' . $header);
										}

										$channel->command('^MSG ' . $data);

										if ($footer ne '')
										{
											$channel->command('^MSG ' . $footer);
										}
									}
								}

								last;
							}
						}
					}
				}
			}

			last;
		}
	}
}

sub signal_nick_mode_changed_last
{
	my ($channel, $nick, $setby, $mode, $type) = @_;

	if (Irssi::settings_get_bool('mh_voiceop_help_on_voice'))
	{
		my $servertag   = lc($channel->{'server'}->{'tag'});
		my $channelname = lc($channel->{'visible_name'});

		for my $serverchannel (split(',', Irssi::settings_get_str('mh_voiceop_channels')))
		{
			$serverchannel = lc(trim_space($serverchannel));

			if ($serverchannel eq $servertag . '/' . $channelname)
			{
				if ($channel->{'chanop'})
				{
					if ((not $nick->{'op'}) and (not $nick->{'halfop'}))
					{
						if (($mode eq '+') and ($type eq '+'))
						{
							signal_message_public_last($channel->{'server'}, Irssi::settings_get_str('mh_voiceop_command_char') . 'HELP', $nick->{'nick'}, $nick->{'host'}, $channel->{'name'});
						}
					}
				}

				last;
			}
		}
	}
}

##############################################################################
#
# script on load
#
##############################################################################

Irssi::settings_add_str('mh_voiceop',  'mh_voiceop_channels',            '');
Irssi::settings_add_str('mh_voiceop',  'mh_voiceop_command_char',        '!');
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_devoice_all',         0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_kick',            1);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_voice',           0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_devoice',         1);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_ban',             1);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_unban',           0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_mode_m',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_mode_t',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_mode_i',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_mode_s',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_mode_p',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_mode_n',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_mode_l',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_mode_k',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_topic',           1);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_invite',          0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_opt_help',            1);
Irssi::settings_add_int('mh_voiceop',  'mh_voiceop_lag_limit',           5);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_invite_always',       0);
Irssi::settings_add_int('mh_voiceop',  'mh_voiceop_ban_time',            60);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_kick_no_userreason',  0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_ban_no_userreason',   0);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_kick_show_who',       1);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_ban_show_who',        1);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_kick_show_type',      1);
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_ban_show_type',       1);
Irssi::settings_add_str('mh_voiceop',  'mh_voiceop_kick_default_reason', '');
Irssi::settings_add_str('mh_voiceop',  'mh_voiceop_ban_default_reason',  '');
Irssi::settings_add_bool('mh_voiceop', 'mh_voiceop_help_notice',         1);
Irssi::settings_add_int('mh_voiceop',  'mh_voiceop_autolimit',           10);
Irssi::settings_add_bool('mh_voiceop',  'mh_voiceop_help_on_voice',      1);
Irssi::settings_add_bool('mh_voiceop',  'mh_voiceop_help_show_prefix',   1);
Irssi::settings_add_str('mh_voiceop',  'mh_voiceop_help_header',         'Hello %NICK%, welcome as voice on %CHANNEL%');
Irssi::settings_add_str('mh_voiceop',  'mh_voiceop_help_footer',         'Please use responsibly');
Irssi::settings_add_str('mh_voiceop',  'mh_voiceop_ban_default_reason',  '');

Irssi::signal_add_last('message public',    'signal_message_public_last');
Irssi::signal_add_last('nick mode changed', 'signal_nick_mode_changed_last');

1;

##############################################################################
#
# eof mh_voiceop.pl
#
##############################################################################
