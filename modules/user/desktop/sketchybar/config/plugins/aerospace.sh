#!/usr/bin/env bash
# Called on aerospace_workspace_change event and on initial load.
# $1 = the workspace ID this item represents.
# $FOCUSED_WORKSPACE = the currently focused workspace (from trigger env).

FOCUSED=$(aerospace list-workspaces --focused 2>/dev/null || echo "")

if [ "$1" = "$FOCUSED" ]; then
  # Active workspace: yellow background, dark icon
  sketchybar --set "$NAME" \
    icon.color=0xff24273a \
    background.color=0xffeed49f
else
  # Check if workspace has any windows
  WINDOWS=$(aerospace list-windows --workspace "$1" 2>/dev/null | wc -l | tr -d ' ')
  if [ "${WINDOWS:-0}" -gt "0" ]; then
    # Occupied but not focused: medium surface, light icon
    sketchybar --set "$NAME" \
      icon.color=0xffcad3f5 \
      background.color=0xff494d64
  else
    # Empty: dim surface, dim icon
    sketchybar --set "$NAME" \
      icon.color=0xff6e738d \
      background.color=0xff363a4f
  fi
fi
