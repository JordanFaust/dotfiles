#!/usr/bin/env bash
# Use $INFO from front_app_switched if available; otherwise poll via lsappinfo.
APP="${INFO}"
if [ -z "$APP" ]; then
  APP=$(lsappinfo info -only name "$(lsappinfo front)" 2>/dev/null | cut -d'"' -f4)
fi
APP="${APP:-Desktop}"

case "$APP" in
  kitty|Kitty)                            ICON="" ;;
  Firefox)                                ICON="󰈹" ;;
  Neovim|nvim)                            ICON="" ;;
  Slack)                                  ICON="󰒱" ;;
  Spotify)                                ICON="󰓇" ;;
  "Visual Studio Code"|Code)              ICON="󰨞" ;;
  Cursor)                                 ICON="󰅭" ;;
  zoom.us)                                ICON="󰒙" ;;
  "Google Chrome")                        ICON="" ;;
  Safari)                                 ICON="" ;;
  Finder)                                 ICON="󰀶" ;;
  "System Settings"|"System Preferences") ICON="󰒓" ;;
  Terminal|iTerm2)                        ICON="" ;;
  Raycast)                                ICON="󱗖" ;;
  "Desktop"|"")                          ICON="󰣇" ; APP="Desktop" ;;
  *)                                      ICON="󰣆" ;;
esac

sketchybar --set "$NAME" icon="$ICON" label="$APP"
