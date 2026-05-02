package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/BurntSushi/toml"
)

type SearchPath struct {
	Path  string `toml:"path"`
	Name  string `toml:"name,omitempty"`
	Depth int    `toml:"depth"`
}

type Config struct {
	SearchPaths []SearchPath `toml:"search_paths"`
}

func DefaultConfig() Config {
	home, _ := os.UserHomeDir()
	return Config{
		SearchPaths: []SearchPath{
			{Path: "/etc/dotfiles", Name: "dotfiles", Depth: 0},
			{Path: filepath.Join(home, ".config/nvim"), Name: "nvim", Depth: 0},
			{Path: filepath.Join(home, "github.com"), Depth: 1},
		},
	}
}

func LoadConfig() Config {
	home, _ := os.UserHomeDir()
	cfgFile := filepath.Join(home, ".config", "tmux", "tsm.toml")

	var cfg Config
	if _, err := toml.DecodeFile(cfgFile, &cfg); err != nil {
		if os.IsNotExist(err) {
			return DefaultConfig()
		}
		fmt.Fprintf(os.Stderr, "tsm: warning: could not parse %s: %v\n", cfgFile, err)
		return DefaultConfig()
	}
	return cfg
}
