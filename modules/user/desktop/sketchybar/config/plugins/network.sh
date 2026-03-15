#!/usr/bin/env bash
# Try Wi-Fi first (en0), then ethernet (en1).
SSID=$(networksetup -getairportnetwork en0 2>/dev/null | awk -F': ' '{print $2}')

if [ -n "$SSID" ] && [ "$SSID" != "You are not associated with an AirPort network." ]; then
  sketchybar --set "$NAME" \
    icon="󰤨" \
    icon.color=0xff8aadf4 \
    label="$SSID"
elif ifconfig en1 2>/dev/null | grep -q "status: active"; then
  sketchybar --set "$NAME" \
    icon="󰈀" \
    icon.color=0xffa6da95 \
    label="Wired"
else
  sketchybar --set "$NAME" \
    icon="󰤭" \
    icon.color=0xffed8796 \
    label="Offline"
fi
