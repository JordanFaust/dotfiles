#!/usr/bin/env node

/**
 * TMUX Session Finder
 * Discovers git repositories and provides FZF integration for session management
 */

import { execSync } from "child_process";
import fs from "fs";
import os from "os";
import path from "path";

/**
 * Configuration for directory search paths
 * @type {string[]}
 */
const SEARCH_PATHS = ["/etc/dotfiles", "/media/procore", path.join(os.homedir(), "github.com")];

/**
 * Checks if a directory contains a .git folder
 * @param {string} dirPath - Path to check for git repository
 * @returns {boolean} True if directory is a git repository
 */
function isGitRepository(dirPath) {
  try {
    const gitPath = path.join(dirPath, ".git");
    return fs.existsSync(gitPath);
  } catch (error) {
    return false;
  }
}

/**
 * Discovers all git repositories in the specified search paths
 * @returns {Array<{name: string, path: string}>} Array of git repositories
 */
function discoverGitRepositories() {
  const repositories = [];

  // Always include /etc/dotfiles if it exists and is a git repository
  const dotfilesPath = '/etc/dotfiles';
  if (fs.existsSync(dotfilesPath) && isGitRepository(dotfilesPath)) {
    repositories.push({
      name: 'dotfiles',
      path: dotfilesPath,
    });
  }

  // Always include ~/.config/nvim if it exists and is a git repository
  const nvimPath = path.join(os.homedir(), '.config/nvim');
  if (fs.existsSync(nvimPath) && isGitRepository(nvimPath)) {
    repositories.push({
      name: 'nvim',
      path: nvimPath,
    });
  }

  for (const searchPath of SEARCH_PATHS) {
    try {
      if (!fs.existsSync(searchPath)) {
        continue;
      }

      const entries = fs.readdirSync(searchPath, { withFileTypes: true });

      for (const entry of entries) {
        if (entry.isDirectory()) {
          const fullPath = path.join(searchPath, entry.name);

          if (isGitRepository(fullPath)) {
            repositories.push({
              name: entry.name,
              path: fullPath,
            });
          }
        }
      }
    } catch (error) {
      // Skip directories we can't read
      continue;
    }
  }

  return repositories;
}

/**
 * Gets the current TMUX session name if inside a TMUX session
 * @returns {string|null} Current session name or null if not in TMUX
 */
function getCurrentSession() {
  try {
    const sessionName = execSync('tmux display-message -p "#S"', {
      encoding: "utf8",
      stdio: ["pipe", "pipe", "ignore"],
    }).trim();
    return sessionName;
  } catch (error) {
    return null;
  }
}

/**
 * Gets all existing TMUX session names
 * @returns {string[]} Array of existing session names
 */
function getExistingSessions() {
  try {
    const output = execSync('tmux list-sessions -F "#{session_name}"', {
      encoding: "utf8",
      stdio: ["pipe", "pipe", "ignore"],
    });
    return output
      .trim()
      .split("\n")
      .filter((name) => name);
  } catch (error) {
    return [];
  }
}

/**
 * Sorts repositories putting the current session first
 * @param {Array<{name: string, path: string}>} repositories - Array of repositories
 * @param {string|null} currentSession - Current session name
 * @returns {Array<{name: string, path: string}>} Sorted repositories
 */
function sortRepositoriesByCurrentSession(repositories, currentSession) {
  if (!currentSession) {
    return repositories;
  }

  const currentRepo = repositories.find((repo) => repo.name === currentSession);
  const otherRepos = repositories.filter((repo) => repo.name !== currentSession);

  return currentRepo ? [currentRepo, ...otherRepos] : repositories;
}

/**
 * Formats repository information for FZF display
 * @param {Array<{name: string, path: string}>} repositories - Array of repositories
 * @param {string|null} currentSession - Current session name
 * @param {string[]} existingSessions - Array of existing session names
 * @returns {string[]} Formatted lines for FZF
 */
function formatRepositoriesForFZF(repositories, currentSession, existingSessions) {
  // Find the longest project name for consistent padding
  const maxNameLength = Math.max(...repositories.map(repo => repo.name.length));
  const nameWidth = Math.max(maxNameLength + 2, 25); // At least 25 chars

  return repositories.map((repo) => {
    const isCurrentSession = repo.name === currentSession;
    const hasSession = existingSessions.includes(repo.name);

    // Create status indicator with Catppuccin Macchiato colors
    let status = "";
    let nameColor = "";
    if (isCurrentSession) {
      status = "\x1b[38;2;166;218;149m●\x1b[0m"; // green (#a6da95) = 166,218,149
      nameColor = "\x1b[1;38;2;166;218;149m"; // Bold green for current session name
    } else if (hasSession) {
      status = "\x1b[38;2;245;169;127m○\x1b[0m"; // peach (#f5a97f) = 245,169,127
      nameColor = "\x1b[38;2;245;169;127m"; // Peach for existing session name
    } else {
      status = "\x1b[38;2;110;115;141m◦\x1b[0m"; // overlay0 (#6e738d) = 110,115,141
      nameColor = "\x1b[38;2;202;211;245m"; // text (#cad3f5) = 202,211,245
    }

    // Format with space delimiter
    const coloredName = `${nameColor}${repo.name}\x1b[0m`;

    // Use space delimiter with status, name, and raw path
    return `${status} ${coloredName} ${repo.path}`;
  });
}

/**
 * Main function to discover repositories and output for FZF
 * @param {string} [mode='simple'] - Output mode: 'simple' for names only, 'detailed' for formatted output
 */
function main(mode = "simple") {
  const repositories = discoverGitRepositories();
  const currentSession = getCurrentSession();
  const existingSessions = getExistingSessions();
  const sortedRepositories = sortRepositoriesByCurrentSession(repositories, currentSession);

  if (mode === "detailed") {
    // Output formatted information for enhanced FZF display
    const formattedLines = formatRepositoriesForFZF(
      sortedRepositories,
      currentSession,
      existingSessions
    );
    for (const line of formattedLines) {
      console.log(line);
    }
  } else {
    // Output repository names only for simple mode
    for (const repo of sortedRepositories) {
      console.log(repo.name);
    }
  }
}

// Export functions for potential reuse
export { discoverGitRepositories, getCurrentSession, getExistingSessions, isGitRepository };

// Run main function if this file is executed directly
if (process.argv[1] === new URL(import.meta.url).pathname) {
  const mode = process.argv[2] || "simple";
  main(mode);
}

