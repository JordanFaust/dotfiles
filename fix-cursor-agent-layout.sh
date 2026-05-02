#!/usr/bin/env bash
#
# Resets all Cursor workspace layout state to the "desired" configuration:
#   - Explorer (unified sidebar) on the LEFT
#   - Agent chat panel on the RIGHT (auxiliary bar)
#   - No per-workspace layout overrides (uses global defaults)
#
# Must be run with Cursor CLOSED. Safe to run repeatedly.
#
# Usage:
#   fix-cursor-agent-layout.sh           # fix all workspaces
#   fix-cursor-agent-layout.sh --check   # dry-run: show what would change
#
set -euo pipefail

SQLITE="${CURSOR_SQLITE:-$(command -v sqlite3 2>/dev/null || echo "")}"
if [ -z "$SQLITE" ]; then
  # Fall back to nix store sqlite
  for candidate in /nix/store/*-sqlite-*-bin/bin/sqlite3; do
    [ -x "$candidate" ] && SQLITE="$candidate" && break
  done
fi
if [ -z "$SQLITE" ] || [ ! -x "$SQLITE" ]; then
  echo "ERROR: sqlite3 not found. Install it or set CURSOR_SQLITE=/path/to/sqlite3"
  exit 1
fi

WS_ROOT="${HOME}/.config/Cursor/User/workspaceStorage"
GLOBAL_DB="${HOME}/.config/Cursor/User/globalStorage/state.vscdb"
DRY_RUN=false
[ "${1:-}" = "--check" ] || [ "${1:-}" = "--dry-run" ] && DRY_RUN=true

if ! $DRY_RUN; then
  if pgrep -x "cursor" >/dev/null 2>&1 || pgrep -f "[C]ursor Helper" >/dev/null 2>&1; then
    echo "ERROR: Cursor appears to be running. Close it first or changes will be overwritten."
    echo "       (Use --check for a dry-run that works while Cursor is open.)"
    exit 1
  fi
fi

fixed=0
skipped=0
total=0

for dir in "$WS_ROOT"/*/; do
  db="${dir}state.vscdb"
  ws="${dir}workspace.json"
  [ -f "$db" ] && [ -f "$ws" ] || continue

  folder=$(grep -o '"folder":"[^"]*"' "$ws" 2>/dev/null | sed 's/"folder":"file:\/\///' | sed 's/"//' || echo "unknown")
  total=$((total + 1))

  sidebar_hidden=$($SQLITE "$db" "SELECT value FROM ItemTable WHERE key = 'workbench.sideBar.hidden'" 2>/dev/null || echo "")
  unified_hidden=$($SQLITE "$db" "SELECT value FROM ItemTable WHERE key = 'workbench.unifiedSidebar.hidden'" 2>/dev/null || echo "")
  agent_count=$($SQLITE "$db" "SELECT count(*) FROM ItemTable WHERE key LIKE 'cursor/agentLayout%'" 2>/dev/null || echo "0")
  editor_count=$($SQLITE "$db" "SELECT count(*) FROM ItemTable WHERE key LIKE 'cursor/editorLayout%'" 2>/dev/null || echo "0")

  needs_fix=false
  reasons=""
  if [ "$sidebar_hidden" != "true" ]; then
    needs_fix=true
    reasons="${reasons} sideBar.hidden=${sidebar_hidden:-unset}->true"
  fi
  if [ "$unified_hidden" != "false" ]; then
    needs_fix=true
    reasons="${reasons} unifiedSidebar.hidden=${unified_hidden:-unset}->false"
  fi
  if [ "$agent_count" -gt 0 ] 2>/dev/null; then
    needs_fix=true
    reasons="${reasons} remove ${agent_count} agentLayout keys"
  fi
  if [ "$editor_count" -gt 0 ] 2>/dev/null; then
    needs_fix=true
    reasons="${reasons} remove ${editor_count} editorLayout keys"
  fi

  if [ "$needs_fix" = "false" ]; then
    echo "  OK  $folder"
    skipped=$((skipped + 1))
    continue
  fi

  if $DRY_RUN; then
    echo " NEED $folder"
    echo "      ${reasons}"
  else
    echo "  FIX $folder"
    $SQLITE "$db" "INSERT OR REPLACE INTO ItemTable (key, value) VALUES ('workbench.sideBar.hidden', 'true')"
    $SQLITE "$db" "INSERT OR REPLACE INTO ItemTable (key, value) VALUES ('workbench.unifiedSidebar.hidden', 'false')"
    $SQLITE "$db" "DELETE FROM ItemTable WHERE key LIKE 'cursor/agentLayout%'"
    $SQLITE "$db" "DELETE FROM ItemTable WHERE key LIKE 'cursor/editorLayout%'"
    $SQLITE "$db" "DELETE FROM ItemTable WHERE key = 'cursor/layout/editor.auxiliaryBar.width'"
    echo "      ${reasons}"
  fi
  fixed=$((fixed + 1))
done

# Also ensure the global sidebar location is correct
if [ -f "$GLOBAL_DB" ]; then
  sidebar_loc=$($SQLITE "$GLOBAL_DB" "SELECT value FROM ItemTable WHERE key = 'cursor/agentLayout.sidebarLocation'" 2>/dev/null || echo "")
  user_loc=$($SQLITE "$GLOBAL_DB" "SELECT value FROM ItemTable WHERE key = 'cursor/agentLayout.sidebarLocationUser'" 2>/dev/null || echo "")
  if [ "$sidebar_loc" != "right" ] || [ "$user_loc" != "right" ]; then
    if $DRY_RUN; then
      echo ""
      echo " NEED [global] agentLayout.sidebarLocation=${sidebar_loc:-unset}->right, sidebarLocationUser=${user_loc:-unset}->right"
    else
      $SQLITE "$GLOBAL_DB" "INSERT OR REPLACE INTO ItemTable (key, value) VALUES ('cursor/agentLayout.sidebarLocation', 'right')"
      $SQLITE "$GLOBAL_DB" "INSERT OR REPLACE INTO ItemTable (key, value) VALUES ('cursor/agentLayout.sidebarLocationUser', 'right')"
      echo ""
      echo "  FIX [global] agentLayout.sidebarLocation->right, sidebarLocationUser->right"
    fi
  else
    echo ""
    echo "  OK  [global] sidebar location already 'right'"
  fi
fi

echo ""
if $DRY_RUN; then
  echo "Dry run complete. ${fixed} of ${total} workspace(s) need fixing, ${skipped} already correct."
  echo "Run without --check to apply fixes (with Cursor closed)."
else
  echo "Done. Fixed ${fixed} of ${total} workspace(s), ${skipped} already correct."
fi
