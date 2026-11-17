#!/usr/bin/env bash
# Workspace pill for a native macOS desktop (sketchybar `space` component).
# $1 = desktop number this item represents (1–5).
#
# Events (sent by sketchybar itself — no window manager needed):
#   space_change          → $SELECTED is "true" for the active desktop
#   space_windows_change  → $INFO = {"space": n, "apps": {"App": count, ...}}
#   mouse.clicked         → switch to this desktop

export PATH="/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:$HOME/.nix-profile/bin:$PATH"

declare -a WS_COLORS=(0xffcad3f5 0xff7dc4e4 0xffa6da95 0xffb7bdf8 0xffeed49f)
declare -a WS_BG=(0x30cad3f5 0x307dc4e4 0x30a6da95 0x30b7bdf8 0x30eed49f)
# ANSI key codes for 1–5; desktops are bound to cmd-N (modules/user/desktop/spaces.nix)
declare -a KEY_CODES=(18 19 20 21 23)

IDX=$(($1 - 1))
COLOR="${WS_COLORS[$IDX]}"
BG_ACTIVE="${WS_BG[$IDX]}"
STATE="${TMPDIR:-/tmp}/sketchybar-space-$1"

case "$SENDER" in
  mouse.clicked)
    osascript -e "tell application \"System Events\" to key code ${KEY_CODES[$IDX]} using command down"
    exit 0
    ;;
  space_windows_change)
    # Fired per desktop; only record the window count for ours.
    [ "$(jq -r '.space' <<<"$INFO")" = "$1" ] || exit 0
    jq -r '[.apps[]] | add // 0' <<<"$INFO" >"$STATE"
    ;;
esac

WINDOWS=$(cat "$STATE" 2>/dev/null || echo 0)

if [ "$SELECTED" = "true" ]; then
  sketchybar --set "$NAME" \
    icon.color="$COLOR" \
    background.drawing=on \
    background.color="$BG_ACTIVE" \
    background.border_color="$COLOR" \
    background.border_width=2
elif [ "${WINDOWS:-0}" -gt 0 ]; then
  sketchybar --set "$NAME" \
    icon.color="$COLOR" \
    background.color=0xff494d64 \
    background.border_width=0
else
  sketchybar --set "$NAME" \
    icon.color=0xff6e738d \
    background.color=0xff363a4f \
    background.border_width=0
fi
