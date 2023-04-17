#!/usr/bin/env bash

[[ "$(dunstctl history | jq -r '.data[0]')" = "[]" ]] && echo true || echo false
