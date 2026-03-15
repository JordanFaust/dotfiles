#!/usr/bin/env bash
# Called on front_app_switched event.
# $INFO contains the frontmost application name.

APP_NAME="${INFO:-Desktop}"

case "$APP_NAME" in
  "kitty"|"Kitty")                          ICON="" ;;
  "Firefox")                                ICON="󰈹" ;;
  "Neovim"|"nvim")                          ICON="" ;;
  "Slack")                                  ICON="󰒱" ;;
  "Spotify")                                ICON="󰓇" ;;
  "Code"|"Visual Studio Code")              ICON="󰨞" ;;
  "Cursor")                                 ICON="󰅭" ;;
  "zoom.us")                                ICON="󰒙" ;;
  "Google Chrome")                          ICON="" ;;
  "Safari")                                 ICON="" ;;
  "Finder")                                 ICON="󰀶" ;;
  "System Settings"|"System Preferences")   ICON="󰒓" ;;
  "Terminal"|"iTerm2")                      ICON="" ;;
  "Raycast")                                ICON="󱗖" ;;
  "Desktop"|"")                             ICON="󰣇" ; APP_NAME="Desktop" ;;
  *)                                        ICON="󰣆" ;;
esac

sketchybar --set "$NAME" icon="$ICON" label="$APP_NAME"
