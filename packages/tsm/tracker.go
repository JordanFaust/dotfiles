package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"time"
)

const maxRecentSessions = 20
const staleSessionDays = 30

type sessionEntry struct {
	AccessCount   int   `json:"accessCount"`
	FirstAccessed int64 `json:"firstAccessed"`
	LastAccessed  int64 `json:"lastAccessed"`
}

type sessionHistory struct {
	Sessions     map[string]sessionEntry `json:"sessions"`
	LastAccessed []string                `json:"lastAccessed"`
}

func historyFile() string {
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".local", "share", "tmux", "session-history.json")
}

func loadHistory() sessionHistory {
	h := sessionHistory{Sessions: make(map[string]sessionEntry)}
	data, err := os.ReadFile(historyFile())
	if err != nil {
		return h
	}
	_ = json.Unmarshal(data, &h)
	if h.Sessions == nil {
		h.Sessions = make(map[string]sessionEntry)
	}
	return h
}

func saveHistory(h sessionHistory) {
	f := historyFile()
	_ = os.MkdirAll(filepath.Dir(f), 0o755)
	data, _ := json.MarshalIndent(h, "", "  ")
	_ = os.WriteFile(f, data, 0o644)
}

// RecordSessionAccess bumps the access count and prepends the session
// to the recently-accessed list (capped at maxRecentSessions).
func RecordSessionAccess(name string) {
	h := loadHistory()
	now := time.Now().UnixMilli()

	entry := h.Sessions[name]
	if entry.FirstAccessed == 0 {
		entry.FirstAccessed = now
	}
	entry.AccessCount++
	entry.LastAccessed = now
	h.Sessions[name] = entry

	filtered := make([]string, 0, len(h.LastAccessed))
	for _, n := range h.LastAccessed {
		if n != name {
			filtered = append(filtered, n)
		}
	}
	h.LastAccessed = append([]string{name}, filtered...)
	if len(h.LastAccessed) > maxRecentSessions {
		h.LastAccessed = h.LastAccessed[:maxRecentSessions]
	}

	saveHistory(h)
}

// RecentSessions returns the recently-accessed session names in order.
func RecentSessions() []string {
	return loadHistory().LastAccessed
}

// CleanupOldSessions removes sessions not accessed in staleSessionDays days.
func CleanupOldSessions() {
	h := loadHistory()
	cutoff := time.Now().AddDate(0, 0, -staleSessionDays).UnixMilli()

	changed := false
	for name, entry := range h.Sessions {
		if entry.LastAccessed < cutoff {
			delete(h.Sessions, name)
			changed = true
		}
	}

	filtered := make([]string, 0, len(h.LastAccessed))
	for _, name := range h.LastAccessed {
		if _, ok := h.Sessions[name]; ok {
			filtered = append(filtered, name)
		}
	}
	if len(filtered) != len(h.LastAccessed) {
		h.LastAccessed = filtered
		changed = true
	}

	if changed {
		saveHistory(h)
	}
}

// AllHistory returns the raw history struct (for the `tracker show` command).
func AllHistory() sessionHistory {
	return loadHistory()
}
