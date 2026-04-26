package main

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

func main() {
	args := os.Args[1:]
	if len(args) == 0 {
		runFZF(nil)
		return
	}

	cmd, rest := args[0], args[1:]
	switch cmd {
	case "fzf":
		runFZF(rest)
	case "connect":
		if len(rest) == 0 {
			fmt.Fprintln(os.Stderr, "tsm connect: workspace path required")
			os.Exit(1)
		}
		connectSession(rest[0])
	case "finder":
		mode := "detailed"
		if len(rest) > 0 {
			mode = rest[0]
		}
		runFinder(mode)
	case "preview":
		if len(rest) == 0 {
			fmt.Fprintln(os.Stderr, "tsm preview: path required")
			os.Exit(1)
		}
		PreviewProject(rest[0])
	case "tracker":
		runTracker(rest)
	case "session":
		if len(rest) == 0 {
			runFZF(nil)
		} else {
			launchSession(rest[0])
		}
	case "help", "--help", "-h":
		showHelp()
	default:
		runFZF(args)
	}
}

// runFZF launches the interactive FZF session picker.
// If args[0] is a project name, go straight to launchSession.
func runFZF(args []string) {
	if len(args) > 0 {
		launchSession(args[0])
		return
	}

	cfg := LoadConfig()
	repos := DiscoverRepos(cfg)
	current := CurrentSession()
	active := ExistingSessions()
	recent := RecentSessions()

	sorted := SortedRepos(repos, current, active, recent)
	lines := FormatForFZF(sorted, current, active)

	input := strings.Join(lines, "\n")

	selfPath, _ := os.Executable()

	fzfArgs := []string{
		`--prompt=  `,
		`--layout=reverse-list`,
		`--info=inline-right`,
		`--ansi`,
		`--border=none`,
		`--margin=0`,
		`--padding=0,0,0,0`,
		`--no-scrollbar`,
		`--no-hscroll`,
		`--no-mouse`,
		`--separator= `,
		`--color=fg+:#cad3f5,bg+:#2c3047,hl+:#8aadf4`,
		`--color=info:#8aadf4,prompt:#8aadf4:bold,pointer:#ed8796`,
		`--color=marker:#a6da95,spinner:#ed8796`,
		`--color=gutter:#24273a`,
		`--color=prompt:#eed49f,input-bg:#2c3047`,
		`--pointer=▶`,
		`--marker=✓`,
		`--highlight-line`,
		`--with-nth=1,2`,
		`--nth=2`,
		fmt.Sprintf(`--preview=%s preview {3}`, selfPath),
		`--preview-window=right:50%:wrap`,
		`--preview-border=none`,
	}

	// Write the repo list to a temp file so fzf can read it via stdin
	// redirection from the shell. Running fzf through a shell with
	// cmd.Stdin = os.Stdin (the real PTY) gives fzf proper TTY access for
	// both rendering and keyboard — the same way the old Node version worked
	// with execSync(..., { stdio: ["inherit", "pipe", "inherit"] }).
	listFile, err := os.CreateTemp("", "tsm-list-*")
	if err != nil {
		fmt.Fprintf(os.Stderr, "tsm: %v\n", err)
		return
	}
	defer os.Remove(listFile.Name())
	listFile.WriteString(input)
	listFile.Close()

	// Single-quote each fzf argument for the shell command.
	escaped := make([]string, len(fzfArgs))
	for i, a := range fzfArgs {
		escaped[i] = "'" + strings.ReplaceAll(a, "'", `'\''`) + "'"
	}
	shellCmd := "fzf " + strings.Join(escaped, " ") + " < '" + listFile.Name() + "'"

	sh := exec.Command("/bin/sh", "-c", shellCmd)
	sh.Stdin = os.Stdin   // real terminal — critical for fzf's TTY access
	sh.Stderr = os.Stderr

	out, err := sh.Output()
	if err != nil {
		return
	}

	parts := strings.SplitN(strings.TrimSpace(string(out)), " ", 3)
	if len(parts) < 2 {
		return
	}
	projectName := stripANSI(parts[1])
	launchSession(projectName)
}

// launchSession creates or attaches to a named tmux session.
func launchSession(name string) {
	cfg := LoadConfig()
	RecordSessionAccess(name)

	if SessionExists(name) {
		_ = AttachToSession(name)
		return
	}

	repos := DiscoverRepos(cfg)
	var projectPath string
	for _, r := range repos {
		if r.Name == name {
			projectPath = r.Path
			break
		}
	}

	if projectPath == "" {
		fmt.Fprintf(os.Stderr, "tsm: project %q not found\n", name)
		os.Exit(1)
	}

	if err := CreateSession(name, projectPath); err != nil {
		fmt.Fprintf(os.Stderr, "tsm: create session: %v\n", err)
		os.Exit(1)
	}

	_ = AttachToSession(name)
}

// connectSession resolves a workspace path to a canonical session name,
// then creates or attaches to that session.
func connectSession(workspacePath string) {
	resolved, err := filepath.Abs(workspacePath)
	if err != nil {
		resolved = workspacePath
	}

	cfg := LoadConfig()
	name := ResolveSessionName(resolved, cfg)
	RecordSessionAccess(name)

	if SessionExists(name) {
		_ = AttachToSession(name)
		return
	}

	if err := CreateSession(name, resolved); err != nil {
		fmt.Fprintf(os.Stderr, "tsm: create session: %v\n", err)
		os.Exit(1)
	}

	_ = AttachToSession(name)
}

// runFinder prints the repo list for use in scripts.
func runFinder(mode string) {
	cfg := LoadConfig()
	repos := DiscoverRepos(cfg)
	current := CurrentSession()
	active := ExistingSessions()
	recent := RecentSessions()
	sorted := SortedRepos(repos, current, active, recent)

	if mode == "simple" {
		for _, r := range sorted {
			fmt.Println(r.Name)
		}
		return
	}

	for _, line := range FormatForFZF(sorted, current, active) {
		fmt.Println(line)
	}
}

// runTracker dispatches tracker sub-commands.
func runTracker(args []string) {
	if len(args) == 0 {
		fmt.Println("Usage: tsm tracker <record|list|cleanup|show>")
		return
	}
	switch args[0] {
	case "record":
		if len(args) < 2 {
			fmt.Fprintln(os.Stderr, "tsm tracker record: session name required")
			os.Exit(1)
		}
		RecordSessionAccess(args[1])
		fmt.Printf("Recorded access to session: %s\n", args[1])
	case "list":
		for i, name := range RecentSessions() {
			fmt.Printf("  %d. %s\n", i+1, name)
		}
	case "cleanup":
		CleanupOldSessions()
		fmt.Println("Cleaned up old session data")
	case "show":
		h := AllHistory()
		data, _ := json.MarshalIndent(h, "", "  ")
		fmt.Println(string(data))
	default:
		fmt.Fprintf(os.Stderr, "tsm tracker: unknown command %q\n", args[0])
		os.Exit(1)
	}
}

var ansiEscRE = regexp.MustCompile(`\x1b\[[0-9;]*m`)

// stripANSI removes ANSI CSI SGR escape sequences from s.
func stripANSI(s string) string {
	return ansiEscRE.ReplaceAllString(s, "")
}

func showHelp() {
	fmt.Print(`tsm — tmux session manager

Usage: tsm <command> [args]

Commands:
  fzf [name]         Interactive FZF session picker (default)
  connect <path>     Create or attach; derives session name via repo discovery
  finder [mode]      Print repo list (modes: simple, detailed)
  preview <path>     Render directory preview for FZF
  tracker <cmd>      Session history (record|list|cleanup|show)
  session <name>     Create or attach to a named session
  help               Show this help

Examples:
  tsm fzf
  tsm connect ~/github.com/procore/api-gateway
  tsm connect /etc/dotfiles
  tsm finder detailed
  tsm tracker list
`)
}
