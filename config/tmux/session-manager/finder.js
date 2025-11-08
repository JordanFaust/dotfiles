#!/usr/bin/env node

/**
 * TMUX Session Finder
 * Discovers git repositories and provides FZF integration for session management
 */

import { execSync } from "child_process";
import fs from "fs";
import os from "os";
import path from "path";
import { getRecentlyAccessedSessions } from "./tracker.js";

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
  const dotfilesPath = "/etc/dotfiles";
  if (fs.existsSync(dotfilesPath) && isGitRepository(dotfilesPath)) {
    repositories.push({
      name: "dotfiles",
      path: dotfilesPath,
    });
  }

  // Always include ~/.config/nvim if it exists and is a git repository
  const nvimPath = path.join(os.homedir(), ".config/nvim");
  if (fs.existsSync(nvimPath) && isGitRepository(nvimPath)) {
    repositories.push({
      name: "nvim",
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
 * Ordered session sorting: current session -> active sessions -> recently accessed -> alphabetical
 * @param {Array<{name: string, path: string}>} repositories - Array of repositories
 * @param {string|null} currentSession - Current session name
 * @param {string[]} existingSessions - Array of existing session names
 * @returns {Array<{name: string, path: string}>} Sorted repositories
 */
function sortRepositories(repositories, currentSession, existingSessions) {
  const recentlyAccessed = getRecentlyAccessedSessions();

  // Create priority groups
  const current = [];
  const activeOther = [];
  const recentlyAccessedSessions = [];
  const alphabetical = [];

  for (const repo of repositories) {
    if (repo.name === currentSession) {
      current.push(repo);
    } else if (existingSessions.includes(repo.name)) {
      activeOther.push(repo);
    } else if (recentlyAccessed.includes(repo.name)) {
      recentlyAccessedSessions.push(repo);
    } else {
      alphabetical.push(repo);
    }
  }

  // Sort active sessions by recent access (most recent first)
  activeOther.sort((a, b) => {
    const aIndex = recentlyAccessed.indexOf(a.name);
    const bIndex = recentlyAccessed.indexOf(b.name);

    // If both are in recent list, sort by recency
    if (aIndex !== -1 && bIndex !== -1) {
      return aIndex - bIndex; // Lower index = more recent
    }

    // If only one is in recent list, prioritize it
    if (aIndex !== -1) return -1;
    if (bIndex !== -1) return 1;

    // Both not in recent list, sort alphabetically
    return a.name.localeCompare(b.name);
  });

  // Sort recently accessed sessions by their position in recent list
  recentlyAccessedSessions.sort((a, b) => {
    const aIndex = recentlyAccessed.indexOf(a.name);
    const bIndex = recentlyAccessed.indexOf(b.name);
    return aIndex - bIndex;
  });

  // Sort alphabetical group
  alphabetical.sort((a, b) => a.name.localeCompare(b.name));

  // Combine all groups in priority order
  return [...current, ...activeOther, ...recentlyAccessedSessions, ...alphabetical];
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
  const maxNameLength = Math.max(...repositories.map((repo) => repo.name.length));

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
 * Discovers all projects along with existing TMUX sessions and outputs for FZF
 */
function main() {
  const repositories = discoverGitRepositories();
  const currentSession = getCurrentSession();
  const existingSessions = getExistingSessions();
  const sortedRepositories = sortRepositories(repositories, currentSession, existingSessions);

  // Output formatted information for enhanced FZF display
  const formattedLines = formatRepositoriesForFZF(
    sortedRepositories,
    currentSession,
    existingSessions
  );
  for (const line of formattedLines) {
    console.log(line);
  }
}

// Export functions for potential reuse
export { discoverGitRepositories, getCurrentSession, getExistingSessions, isGitRepository };

// Run main function if this file is executed directly
if (process.argv[1] === new URL(import.meta.url).pathname) {
  main();
}
