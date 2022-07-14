# Copyright (c) 2022 Younes Ben El (ybenel)
# Requires GetMusic Shell Script (https://github.com/m1ndo/dotfiles (eww scripts))

import os,time

from libqtile.log_utils import logger
from libqtile.widget import base

class GetMusic(base.InLoopPollText):
    """A simple Music text-based Updater"""

    defaults = [
        ("update_interval", 1.0, "Update interval for the clock"),
    ]

    def __init__(self, **config):
        base.InLoopPollText.__init__(self, **config)
        self.add_defaults(GetMusic.defaults)


    def tick(self):
        self.update(self.poll())
        return self.update_interval - time.time() % self.update_interval

    def poll(self):
        artist = os.popen("getmusic --artist").read().strip('\n').strip('\n')
        title = os.popen("getmusic --title").read().strip('\n')
        asme = f"{artist} - {title}"
        return asme
