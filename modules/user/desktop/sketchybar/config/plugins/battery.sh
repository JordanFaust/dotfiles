#!/usr/bin/env bash
PERCENTAGE=$(pmset -g batt | grep -Eo "[0-9]+%" | head -1 | tr -d '%')
CHARGING=$(pmset -g batt | grep -c "AC Power" || true)

if [ "${CHARGING:-0}" -gt "0" ]; then
  ICON="󰂄"
  COLOR=0xffa6da95
elif [ "${PERCENTAGE:-0}" -ge "80" ]; then
  ICON="󰁹"
  COLOR=0xffa6da95
elif [ "${PERCENTAGE:-0}" -ge "50" ]; then
  ICON="󰁾"
  COLOR=0xffeed49f
elif [ "${PERCENTAGE:-0}" -ge "20" ]; then
  ICON="󰁼"
  COLOR=0xfffab387
else
  ICON="󰁺"
  COLOR=0xffed8796
fi

sketchybar --set "$NAME" icon="$ICON" icon.color="$COLOR" label="${PERCENTAGE}%"
