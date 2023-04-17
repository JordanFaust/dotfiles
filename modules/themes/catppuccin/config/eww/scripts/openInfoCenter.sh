#!/usr/bin/env bash

LOCK_FILE="$HOME/.cache/eww-info-center.lock"

fix_stacking_bug() {
	for entry in $(xdotool search --pid $(pidof eww)); do
		xdo below -N eww-control-panel $entry
	done
}

run() {
	eww open info-center
	sleep 0.2
	fix_stacking_bug; eww update icenter=true; xdo raise -N eww-bar
}

# Run eww daemon if not running
# if [[ ! `pidof eww` ]]; then
# 	eww daemon
# 	sleep 1
# else
if [[ ! -f "$LOCK_FILE" ]]; then
  touch "$LOCK_FILE"
  run
else
  rm "$LOCK_FILE"
  eww update icenter=false
  sleep 0.6
  eww close info-center
  xdo lower -N eww-bar
fi
# fi
