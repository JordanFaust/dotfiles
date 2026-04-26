package main

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

const maxPreviewItems = 20

func formatPreviewItem(fullPath, name string) string {
	info, err := os.Lstat(fullPath)
	if err != nil {
		return AnsiDim + "  " + name + AnsiReset
	}

	if info.IsDir() {
		switch {
		case name == ".git":
			return AnsiRed + " " + name + AnsiReset
		case name == "node_modules":
			return AnsiGreen + " " + name + AnsiReset
		case strings.HasPrefix(name, "."):
			return AnsiDim + " " + name + AnsiReset
		default:
			return AnsiBlue + " " + name + AnsiReset
		}
	}

	ext := strings.ToLower(filepath.Ext(name))
	switch ext {
	case ".js", ".mjs":
		return AnsiYellow + " " + name + AnsiReset
	case ".ts":
		return AnsiYellow + " " + name + AnsiReset
	case ".json":
		return AnsiYellow + " " + name + AnsiReset
	case ".md", ".markdown":
		return AnsiBlue + " " + name + AnsiReset
	case ".lua":
		return AnsiBlue + " " + name + AnsiReset
	case ".go":
		return AnsiBlue + " " + name + AnsiReset
	case ".nix":
		return AnsiLavend + " " + name + AnsiReset
	case ".lock":
		return AnsiDim + " " + name + AnsiReset
	case ".gitignore", ".gitattributes":
		return AnsiRed + " " + name + AnsiReset
	default:
		return AnsiText + " " + name + AnsiReset
	}
}

// PreviewProject prints a Catppuccin-coloured directory listing to stdout.
func PreviewProject(projectPath string) {
	if _, err := os.Stat(projectPath); err != nil {
		fmt.Println(AnsiRed + "Error: Directory not found" + AnsiReset)
		fmt.Println(AnsiDim + projectPath + AnsiReset)
		return
	}

	entries, err := os.ReadDir(projectPath)
	if err != nil {
		fmt.Println(AnsiRed + "Error reading directory:" + AnsiReset)
		fmt.Println(AnsiDim + err.Error() + AnsiReset)
		return
	}

	sort.Slice(entries, func(i, j int) bool {
		di, dj := entries[i].IsDir(), entries[j].IsDir()
		if di != dj {
			return di
		}
		return entries[i].Name() < entries[j].Name()
	})

	fmt.Println(AnsiMauve + projectPath + AnsiReset)
	fmt.Println()

	limit := len(entries)
	if limit > maxPreviewItems {
		limit = maxPreviewItems
	}
	for _, e := range entries[:limit] {
		fmt.Println(formatPreviewItem(filepath.Join(projectPath, e.Name()), e.Name()))
	}

	if len(entries) > maxPreviewItems {
		fmt.Printf("\n%s... and %d more items%s\n", AnsiDim, len(entries)-maxPreviewItems, AnsiReset)
	}
}
