#!/usr/bin/env bash
# Claude Code status line, after OpenCode's footer, in Catppuccin Macchiato:
#   ~/code/project:main                              168.3K (17%) · $4.14   Opus 5.5
# Reads the session JSON on stdin; needs jq.

IFS=$'\t' read -r model dir used pct cost < <(
  jq -r '[
    (.model.display_name // "Claude"),
    (.workspace.current_dir // env.PWD),
    (.context_window.current_usage // {} | (.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0)),
    (.context_window.used_percentage // 0 | floor),
    (.cost.total_cost_usd // 0)
  ] | @tsv'
)

rgb() { printf '\033[38;2;%d;%d;%dm' 0x"${1:0:2}" 0x"${1:2:2}" 0x"${1:4:2}"; }
RED=$(rgb ed8796) SUB=$(rgb a5adcb)
BOLD=$'\033[1m' RESET=$'\033[0m'

# left: where you are
left="${dir/#$HOME/\~}"
if branch=$(git -C "$dir" branch --show-current 2>/dev/null) && [[ -n $branch ]]; then
  left+=":$branch"
fi

# right: context, cost, model
if ((used >= 1000)); then tokens=$(awk -v n="$used" 'BEGIN { printf "%.1fK", n / 1000 }'); else tokens=$used; fi
stats="$tokens ($pct%)"
if awk -v c="$cost" 'BEGIN { exit !(c > 0) }'; then stats+=" · \$$(printf '%.2f' "$cost")"; fi

# Claude Code insets the line 2 columns; keep 2 more clear on the right to match the boxes
width=$((${COLUMNS:-80} - 4))
gap=$((width - ${#left} - ${#stats} - 3 - ${#model}))
((gap < 2)) && gap=2

printf '%s%s%*s%s%s%s   %s%s%s\n' \
  "$SUB" "$left" "$gap" '' "$SUB" "$stats" "$RESET" "$BOLD$RED" "$model" "$RESET"
