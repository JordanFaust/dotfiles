#!/bin/bash
# Noodle Cleanup Script
#   Some of my widgets (mpd, volume) rely on scripts that have to be
#   run persistently in the background.
#   They sleep until mpd/volume state changes, in an infinite loop.
#   As a result when awesome restarts, they keep running in background, along with the new ones that are created after the restart.
#   This script cleans up the old processes.

ps aux | grep "spotify-info" | grep -v grep | awk '{print $2}' | xargs kill
ps aux | grep "pactl subscribe" | grep -v grep | awk '{print $2}' | xargs kill
ps aux | grep "desktopevents-data" | grep -v grep | awk '{print $2}' | xargs kill
