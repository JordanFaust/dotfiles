#!/usr/bin/env bash
# Orchestrator: called once on aerospace_workspace_change or front_app_switched.
# Queries all windows in the focused workspace and updates client.1–8 slots.

# Ensure nix-installed binaries (aerospace) are reachable.
export PATH="/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:${HOME:-/Users/jordan.faust}/.nix-profile/bin:$PATH"

MAX_CLIENTS=8

# $FOCUSED_WORKSPACE is injected by sketchybar from aerospace_workspace_change
# trigger args.  Fall back to querying AeroSpace directly for other events.
FOCUSED="${FOCUSED_WORKSPACE}"
if [ -z "$FOCUSED" ]; then
  FOCUSED=$(aerospace list-workspaces --focused 2>/dev/null | tr -d '[:space:]')
fi
[ -z "$FOCUSED" ] && FOCUSED="1"

# Collect app names — one per line, no extra columns.
APPS=()
while IFS= read -r APP; do
  [ -n "$APP" ] && APPS+=("$APP")
done < <(aerospace list-windows --workspace "$FOCUSED" --format '%{app-name}' 2>/dev/null)

app_to_icon() {
  case "$1" in
    kitty|Kitty)                          echo "" ;;
    Firefox)                              echo "󰈹" ;;
    Neovim|nvim)                          echo "" ;;
    Slack)                                echo "󰒱" ;;
    Spotify)                              echo "󰓇" ;;
    "Visual Studio Code"|Code)            echo "󰨞" ;;
    Cursor)                               echo "󰅭" ;;
    zoom.us)                              echo "󰒙" ;;
    "Google Chrome")                      echo "" ;;
    Safari)                               echo "" ;;
    Finder)                               echo "󰀶" ;;
    "System Settings"|"System Preferences") echo "󰒓" ;;
    Terminal|iTerm2)                      echo "" ;;
    Raycast)                              echo "󱗖" ;;
    *)                                    echo "󰣆" ;;
  esac
}

for i in $(seq 1 $MAX_CLIENTS); do
  IDX=$((i - 1))
  APP="${APPS[$IDX]}"
  if [ -z "$APP" ]; then
    sketchybar --set "client.$i" drawing=off
  else
    ICON=$(app_to_icon "$APP")
    sketchybar --set "client.$i" \
      drawing=on \
      icon="$ICON" \
      label="$APP"
  fi
done
