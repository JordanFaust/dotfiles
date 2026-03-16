#!/usr/bin/env bash
# clients.sh <slot>
# Shows the <slot>-th window from the focused AeroSpace workspace.
# Items stay drawing=on always (drawing=off silences update_freq AND events).
# "Hidden" state = empty icon/label + zero padding + no background.

LOG=/tmp/sketchybar-clients.log
log() { echo "[$(date '+%H:%M:%S')] slot=${SLOT:-?} $*" >> "$LOG"; }

hide() {
  sketchybar --set "$NAME" \
    icon="" label="" \
    icon.padding_left=0 icon.padding_right=0 \
    label.padding_left=0 label.padding_right=0 \
    background.drawing=off \
    background.padding_left=0 background.padding_right=0
}

show() {
  sketchybar --set "$NAME" \
    icon="$1" label="$2" \
    icon.padding_left=8 icon.padding_right=4 \
    label.padding_left=2 label.padding_right=8 \
    background.drawing=on \
    background.padding_left=4 background.padding_right=4
}

# Probe common nix/darwin install locations for the aerospace binary.
for _CANDIDATE in \
  /run/current-system/sw/bin/aerospace \
  /nix/var/nix/profiles/default/bin/aerospace \
  "${HOME:-/Users/jordan.faust}/.nix-profile/bin/aerospace" \
  /opt/homebrew/bin/aerospace \
  /usr/local/bin/aerospace; do
  if [ -x "$_CANDIDATE" ]; then
    AEROSPACE="$_CANDIDATE"
    break
  fi
done

SLOT="${1:-1}"

if [ -z "${AEROSPACE:-}" ]; then
  log "aerospace not found in any candidate path"
  hide; exit 0
fi

STATEFILE=/tmp/sketchybar-aerospace-ws
log "aerospace=$AEROSPACE NAME=$NAME FOCUSED_WORKSPACE=${FOCUSED_WORKSPACE:-<unset>} SENDER=${SENDER:-<unset>}"

if [ -n "$FOCUSED_WORKSPACE" ]; then
  # Event path: save workspace for future non-event calls
  FOCUSED="$FOCUSED_WORKSPACE"
  echo "$FOCUSED" > "$STATEFILE"
else
  # Non-event path (shouldn't happen without update_freq, but handle gracefully)
  FOCUSED=$(cat "$STATEFILE" 2>/dev/null)
  log "using cached workspace -> '${FOCUSED:-none}'"
fi

if [ -z "$FOCUSED" ]; then
  log "no workspace available — hiding"
  hide; exit 0
fi

RAW=$("$AEROSPACE" list-windows --workspace "$FOCUSED" 2>&1)
log "list-windows output: $(echo "$RAW" | head -8 | tr '\n' '|')"

APP=$(echo "$RAW" \
  | sed -n "${SLOT}p" \
  | cut -d'|' -f2 \
  | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

log "slot $SLOT -> app='$APP'"

if [ -z "$APP" ]; then
  hide
else
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
    *)                                      ICON="󰣆" ;;
  esac
  log "showing icon=$ICON label=$APP"
  show "$ICON" "$APP"
fi
