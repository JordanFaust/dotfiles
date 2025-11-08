#!/usr/bin/env bash

# TMUX Session Manager Wrapper
# Handles TMUX popup environment properly

# Change to script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR" || return

# Execute the CLI with proper terminal handling
node session-manager/cli.js fzf "$@"
