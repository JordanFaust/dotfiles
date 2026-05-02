package main

import (
	"fmt"
	"os"
	"os/exec"
)

// SessionExists returns true if a tmux session with the given name is running.
func SessionExists(name string) bool {
	return contains(ExistingSessions(), name)
}

// CreateSession creates a tmux session with three windows: nvim, terminal, server.
// The nvim window launches nvim immediately. Returns an error on failure.
func CreateSession(name, projectPath string) error {
	cmds := [][]string{
		{"tmux", "new-session", "-d", "-s", name, "-c", projectPath, "-n", "nvim"},
		{"tmux", "send-keys", "-t", name + ":nvim", "nvim", "Enter"},
		{"tmux", "new-window", "-t", name + ":", "-c", projectPath, "-n", "terminal"},
		{"tmux", "new-window", "-t", name + ":", "-c", projectPath, "-n", "server"},
		{"tmux", "select-window", "-t", name + ":nvim"},
	}
	for _, args := range cmds {
		if err := exec.Command(args[0], args[1:]...).Run(); err != nil {
			return fmt.Errorf("tmux command %v: %w", args, err)
		}
	}
	return nil
}

// AttachToSession switches to the session (if inside tmux) or attaches to it.
// When stdout is not a TTY (e.g. Cursor subprocess), attach failure is non-fatal.
func AttachToSession(name string) error {
	isTTY := isTerminal()

	var cmd *exec.Cmd
	if os.Getenv("TMUX") != "" {
		cmd = exec.Command("tmux", "switch-client", "-t", name)
	} else {
		cmd = exec.Command("tmux", "attach-session", "-t", name)
	}

	if isTTY {
		cmd.Stdin = os.Stdin
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	}

	_ = cmd.Run()
	return nil
}

// isTerminal returns true when stdout is connected to a real TTY.
func isTerminal() bool {
	fi, err := os.Stdout.Stat()
	if err != nil {
		return false
	}
	return (fi.Mode() & os.ModeCharDevice) != 0
}
