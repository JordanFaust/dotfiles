package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
)

// ── tmux-palette JSON types ──────────────────────────────────────────────────

type paletteAction struct {
	Shell string `json:"shell"`
}

type paletteItem struct {
	Icon        string        `json:"icon"`
	IconColor   string        `json:"iconColor"`
	Title       string        `json:"title"`
	Description string        `json:"description,omitempty"`
	Category    string        `json:"category,omitempty"`
	Action      paletteAction `json:"action"`
}

// shellQuote wraps s in single quotes with proper escaping so it is safe to
// embed in a POSIX shell command string. Single quotes inside s are handled
// via the '\'  idiom.
func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// FormatForPalette returns a slice of paletteItem values for use as a
// tmux-palette JSON-mode plugin response. Each item carries status-based
// icon/colour, the repo path as a searchable description, a category for
// the grouped palette header, and a shell action that invokes tsm.
func FormatForPalette(repos []Repo, current string, active, recent []string) []paletteItem {
	items := make([]paletteItem, 0, len(repos))
	for _, r := range repos {
		var icon, color, category string
		switch {
		case r.Name == current:
			icon, color, category = "●", HexGreen, "Current"
		case contains(active, r.Name):
			icon, color, category = "○", HexPeach, "Active"
		case contains(recent, r.Name):
			icon, color, category = "◦", HexDim, "Recent"
		default:
			icon, color, category = "◦", HexDim, "All"
		}
		items = append(items, paletteItem{
			Icon:        icon,
			IconColor:   color,
			Title:       r.Name,
			Description: r.Path,
			Category:    category,
			Action:      paletteAction{Shell: fmt.Sprintf("tsm session %s", shellQuote(r.Name))},
		})
	}
	return items
}

// Repo represents a discovered git repository or worktree.
type Repo struct {
	Name string
	Path string
}

// isGitRepo returns true if path contains a .git entry.
func isGitRepo(path string) bool {
	_, err := os.Stat(filepath.Join(path, ".git"))
	return err == nil
}

// discoverWorktrees finds directories under <repoPath>/.worktrees/.
func discoverWorktrees(repoPath, repoName string) []Repo {
	wtDir := filepath.Join(repoPath, ".worktrees")
	entries, err := os.ReadDir(wtDir)
	if err != nil {
		return nil
	}
	var wt []Repo
	for _, e := range entries {
		if e.IsDir() {
			wt = append(wt, Repo{
				Name: repoName + "/" + e.Name(),
				Path: filepath.Join(wtDir, e.Name()),
			})
		}
	}
	return wt
}

// DiscoverRepos walks the configured search paths and returns all git repos
// and their worktrees.
func DiscoverRepos(cfg Config) []Repo {
	var repos []Repo

	for _, sp := range cfg.SearchPaths {
		path := expandHome(sp.Path)

		if !isGitRepo(path) && sp.Depth == 0 {
			continue
		}

		if isGitRepo(path) && sp.Depth == 0 {
			name := sp.Name
			if name == "" {
				name = filepath.Base(path)
			}
			repos = append(repos, Repo{Name: name, Path: path})
			repos = append(repos, discoverWorktrees(path, name)...)
			continue
		}

		// depth >= 1: scan one or two levels
		top, err := os.ReadDir(path)
		if err != nil {
			continue
		}
		for _, e := range top {
			if !e.IsDir() {
				continue
			}
			full := filepath.Join(path, e.Name())
			if isGitRepo(full) {
				name := e.Name()
				repos = append(repos, Repo{Name: name, Path: full})
				repos = append(repos, discoverWorktrees(full, name)...)
			} else if sp.Depth >= 1 {
				// one level deeper: user/repo layout
				sub, err := os.ReadDir(full)
				if err != nil {
					continue
				}
				for _, se := range sub {
					if !se.IsDir() {
						continue
					}
					subFull := filepath.Join(full, se.Name())
					if isGitRepo(subFull) {
						name := e.Name() + "/" + se.Name()
						repos = append(repos, Repo{Name: name, Path: subFull})
						repos = append(repos, discoverWorktrees(subFull, name)...)
					}
				}
			}
		}
	}

	return repos
}

// expandHome replaces a leading ~ with the user's home directory.
func expandHome(path string) string {
	if strings.HasPrefix(path, "~/") {
		home, _ := os.UserHomeDir()
		return filepath.Join(home, path[2:])
	}
	return path
}

// CurrentSession returns the name of the active tmux session, or "".
func CurrentSession() string {
	out, err := exec.Command("tmux", "display-message", "-p", "#S").Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}

// ExistingSessions returns all current tmux session names.
func ExistingSessions() []string {
	out, err := exec.Command("tmux", "list-sessions", "-F", "#{session_name}").Output()
	if err != nil {
		return nil
	}
	var names []string
	for _, line := range strings.Split(strings.TrimSpace(string(out)), "\n") {
		if line != "" {
			names = append(names, line)
		}
	}
	return names
}

// contains is a small helper used for set membership.
func contains(slice []string, s string) bool {
	for _, v := range slice {
		if v == s {
			return true
		}
	}
	return false
}

// SortedRepos orders repos as: current session → active by recency → recently
// accessed (not active) → alphabetical.
func SortedRepos(repos []Repo, current string, active []string, recent []string) []Repo {
	type ranked struct {
		repo  Repo
		group int // 0=current, 1=active, 2=recent, 3=alpha
		rank  int
	}

	indexOf := func(list []string, s string) int {
		for i, v := range list {
			if v == s {
				return i
			}
		}
		return -1
	}

	var ranked_ []ranked
	for _, r := range repos {
		var g, rk int
		switch {
		case r.Name == current:
			g, rk = 0, 0
		case contains(active, r.Name):
			g = 1
			idx := indexOf(recent, r.Name)
			if idx == -1 {
				rk = len(recent) + 1
			} else {
				rk = idx
			}
		case contains(recent, r.Name):
			g = 2
			rk = indexOf(recent, r.Name)
		default:
			g, rk = 3, 0
		}
		ranked_ = append(ranked_, ranked{r, g, rk})
	}

	sort.SliceStable(ranked_, func(i, j int) bool {
		a, b := ranked_[i], ranked_[j]
		if a.group != b.group {
			return a.group < b.group
		}
		if a.rank != b.rank {
			return a.rank < b.rank
		}
		return a.repo.Name < b.repo.Name
	})

	out := make([]Repo, len(ranked_))
	for i, r := range ranked_ {
		out[i] = r.repo
	}
	return out
}

// FormatForFZF returns lines in the format: "<status> <colored-name> <raw-path>"
// FZF is called with --with-nth=1,2 (display) and --nth=2 (search).
func FormatForFZF(repos []Repo, current string, active []string) []string {
	lines := make([]string, len(repos))
	for i, r := range repos {
		var statusIcon, nameColor string
		switch {
		case r.Name == current:
			statusIcon = AnsiGreen + "●" + AnsiReset
			nameColor = AnsiBGreen
		case contains(active, r.Name):
			statusIcon = AnsiPeach + "○" + AnsiReset
			nameColor = AnsiPeach
		default:
			statusIcon = AnsiDim + "◦" + AnsiReset
			nameColor = AnsiText
		}
		coloredName := nameColor + r.Name + AnsiReset
		lines[i] = statusIcon + " " + coloredName + " " + r.Path
	}
	return lines
}

// ResolveSessionName maps an absolute workspace path to the canonical session
// name used by DiscoverRepos. Falls back to filepath.Base(workspacePath).
func ResolveSessionName(workspacePath string, cfg Config) string {
	repos := DiscoverRepos(cfg)
	real, err := filepath.EvalSymlinks(workspacePath)
	if err != nil {
		real = workspacePath
	}
	for _, r := range repos {
		rReal, err := filepath.EvalSymlinks(r.Path)
		if err != nil {
			rReal = r.Path
		}
		if rReal == real {
			return r.Name
		}
	}
	return filepath.Base(workspacePath)
}
