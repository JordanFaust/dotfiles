#!/usr/bin/env bash
# Detect connection status without SSID.
# macOS 26 Tahoe redacts SSID from all CLI tools unless the calling process
# has Location Services entitlements, which unsigned shell scripts never get.
if ifconfig en0 2>/dev/null | grep -q 'inet '; then
  sketchybar --set "$NAME" \
    icon="󰤨" \
    icon.color=0xff8aadf4 \
    label="Wi-Fi"
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
