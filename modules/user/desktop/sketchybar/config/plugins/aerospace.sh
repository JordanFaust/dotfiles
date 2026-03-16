#!/usr/bin/env bash
# Updates workspace pill appearance on aerospace_workspace_change.
# $1 = workspace number this item represents (1–5).
#
# $FOCUSED_WORKSPACE is set by sketchybar from the event trigger args —
# use it directly instead of shelling out to aerospace (which may not be in PATH).

export PATH="/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:$HOME/.nix-profile/bin:$PATH"

declare -a WS_ICONS=("" "󰅩" "󰒱" "󰃽" "")
declare -a WS_COLORS=(0xffcad3f5 0xff7dc4e4 0xffa6da95 0xffb7bdf8 0xffeed49f)
declare -a WS_BG=(0x30cad3f5 0x307dc4e4 0x30a6da95 0x30b7bdf8 0x30eed49f)

IDX=$(( $1 - 1 ))
ICON="${WS_ICONS[$IDX]}"
COLOR="${WS_COLORS[$IDX]}"
BG_ACTIVE="${WS_BG[$IDX]}"

# Prefer the env var passed by the event trigger; fall back to querying AeroSpace
FOCUSED="${FOCUSED_WORKSPACE:-$(aerospace list-workspaces --focused 2>/dev/null || echo "1")}"

if [ "$1" = "$FOCUSED" ]; then
  sketchybar --set "$NAME" \
    icon="$ICON" \
    icon.color="$COLOR" \
    background.drawing=on \
    background.color="$BG_ACTIVE" \
    background.border_color="$COLOR" \
    background.border_width=2
else
  WINDOWS=$(aerospace list-windows --workspace "$1" 2>/dev/null | wc -l | tr -d ' ')
  if [ "${WINDOWS:-0}" -gt "0" ]; then
    sketchybar --set "$NAME" \
      icon="$ICON" \
      icon.color="$COLOR" \
      background.color=0xff494d64 \
      background.border_width=0
  else
    sketchybar --set "$NAME" \
      icon="$ICON" \
      icon.color=0xff6e738d \
      background.color=0xff363a4f \
      background.border_width=0
  fi
fi
