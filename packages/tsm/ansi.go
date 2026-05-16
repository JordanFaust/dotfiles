package main

import (
	"fmt"
	"strconv"
)

// Hex color constants (Catppuccin Macchiato palette).
// These are the source of truth — ANSI escape sequences below are derived
// from them so both the FZF formatter and the JSON palette formatter share
// the same color values.
const (
	HexGreen  = "#a6da95"
	HexPeach  = "#f5a97f"
	HexDim    = "#6e738d"
	HexText   = "#cad3f5"
	HexMauve  = "#c6a0f6"
	HexBlue   = "#8aadf4"
	HexYellow = "#eed49f"
	HexLavend = "#b7bdf8"
	HexRed    = "#ed8796"
)

// ansi24bit converts a "#rrggbb" hex string to an ANSI 24-bit foreground
// escape sequence. Panics on malformed input (only called with literals above).
func ansi24bit(hex string) string {
	r, g, b := parseHex(hex)
	return fmt.Sprintf("\x1b[38;2;%d;%d;%dm", r, g, b)
}

// ansi24bitBold is like ansi24bit but adds the bold attribute.
func ansi24bitBold(hex string) string {
	r, g, b := parseHex(hex)
	return fmt.Sprintf("\x1b[1;38;2;%d;%d;%dm", r, g, b)
}

// parseHex parses "#rrggbb" into r, g, b uint8 values.
func parseHex(hex string) (r, g, b uint8) {
	if len(hex) != 7 || hex[0] != '#' {
		panic("ansi.go: invalid hex color: " + hex)
	}
	rv, _ := strconv.ParseUint(hex[1:3], 16, 8)
	gv, _ := strconv.ParseUint(hex[3:5], 16, 8)
	bv, _ := strconv.ParseUint(hex[5:7], 16, 8)
	return uint8(rv), uint8(gv), uint8(bv)
}

// ANSI escape sequences derived from the hex constants above.
// These are vars (not consts) because they are computed via function calls.
var (
	AnsiReset  = "\x1b[0m"
	AnsiGreen  = ansi24bit(HexGreen)
	AnsiBGreen = ansi24bitBold(HexGreen)
	AnsiPeach  = ansi24bit(HexPeach)
	AnsiDim    = ansi24bit(HexDim)
	AnsiText   = ansi24bit(HexText)
	AnsiMauve  = ansi24bit(HexMauve)
	AnsiBlue   = ansi24bit(HexBlue)
	AnsiYellow = ansi24bit(HexYellow)
	AnsiLavend = ansi24bit(HexLavend)
	AnsiRed    = ansi24bit(HexRed)
)
