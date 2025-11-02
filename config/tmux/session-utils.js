/**
 * TMUX Session Management Utilities
 * Contains all the session management functions
 */

import fs from "fs";
import path from "path";
import { execSync } from "child_process";
import { discoverGitRepositories, getExistingSessions } from "./session-finder.js";

/**
 * Creates a new TMUX session with the specified configuration
 * @param {string} sessionName - Name of the session to create
 * @param {string} projectPath - Root directory path for the project
 * @returns {boolean} True if session was created successfully
 */
export function createSession(sessionName, projectPath) {
  try {
    // Create new session with first window (nvim)
    execSync(`tmux new-session -d -s "${sessionName}" -c "${projectPath}" -n nvim`);

    // Launch nvim in the first window
    execSync(`tmux send-keys -t "${sessionName}:nvim" "nvim" Enter`);

    // Create terminal window
    execSync(`tmux new-window -t "${sessionName}:" -c "${projectPath}" -n terminal`);

    // Create server window
    execSync(`tmux new-window -t "${sessionName}:" -c "${projectPath}" -n server`);

    // Select the nvim window by default
    execSync(`tmux select-window -t "${sessionName}:nvim"`);

    return true;
  } catch (error) {
    console.error(`Failed to create session "${sessionName}":`, error.message);
    return false;
  }
}

/**
 * Checks if a TMUX session exists
 * @param {string} sessionName - Name of the session to check
 * @returns {boolean} True if session exists
 */
export function sessionExists(sessionName) {
  const existingSessions = getExistingSessions();
  return existingSessions.includes(sessionName);
}

/**
 * Attaches to an existing TMUX session or switches to it if already in TMUX
 * @param {string} sessionName - Name of the session to attach to
 * @returns {boolean} True if attachment was successful
 */
export function attachToSession(sessionName) {
  try {
    // Check if we're already in a TMUX session
    const currentSession = process.env.TMUX;

    if (currentSession) {
      // We're in TMUX, switch to the session
      execSync(`tmux switch-client -t "${sessionName}"`, {
        stdio: "inherit",
      });
    } else {
      // We're not in TMUX, attach to the session
      execSync(`tmux attach-session -t "${sessionName}"`, {
        stdio: "inherit",
      });
    }

    return true;
  } catch (error) {
    console.error(`Failed to attach to session "${sessionName}":`, error.message);
    return false;
  }
}

/**
 * Finds the project path for a given project name
 * @param {string} projectName - Name of the project to find
 * @returns {string|null} Path to the project or null if not found
 */
export function findProjectPath(projectName) {
  const repositories = discoverGitRepositories();
  const project = repositories.find((repo) => repo.name === projectName);
  return project ? project.path : null;
}

/**
 * Launches FZF to select a project and returns the selection
 * @returns {string|null} Selected project name or null if cancelled
 */
export function selectProjectWithFZF() {
  try {
    const scriptPath = path.join(
      path.dirname(new URL(import.meta.url).pathname),
      "session-finder.js"
    );

    const previewScript = path.join(
      path.dirname(new URL(import.meta.url).pathname),
      "project-preview.js"
    );

    const fzfCommand = [
      "fzf",
      '--prompt="  "', // Search icon with space
      "--layout=reverse-list", // Puts prompt at bottom
      "--info=right", // Show count on right side
      "--ansi",
      "--border=none",
      "--margin=0",
      "--padding=0,0,0,0", // Reduce bottom padding due to FZF limitations
      "--no-scrollbar",
      // "--color=fg:#cad3f5,bg:#24273a,hl:#8aadf4", // text, base, blue
      "--color=fg+:#cad3f5,bg+:#2c3047,hl+:#8aadf4", // text, custom prompt background, blue
      "--color=info:#8aadf4,prompt:#8aadf4:bold,pointer:#ed8796", // blue, blue bold, red
      "--color=marker:#a6da95,spinner:#ed8796", // green, red
      "--color=gutter:#24273a",
      "--color=prompt:#eed49f,input-bg:#24273a", // text, base, blue
      "--pointer=▶",
      "--marker=✓",
      "--with-nth=1,2", // Show only status and name
      "--nth=2", // Search on project name (2nd column)
      `--preview="node ${previewScript} {3}"`, // Preview using the 3rd column (raw path)
      "--preview-window=right:50%:wrap", // Preview on right side, 50% width, wrap text
      "--preview-border=none",
    ].join(" ");

    const result = execSync(`node "${scriptPath}" detailed | ${fzfCommand}`, {
      encoding: "utf8",
      stdio: ["inherit", "pipe", "inherit"],
    });

    // Extract just the project name from the space-delimited line
    const parts = result.trim().split(/\s+/);
    // Strip ANSI codes from the project name (2nd column)
    const projectName = parts[1].replace(/\x1b\[[0-9;]*m/g, "");
    return projectName;
  } catch (error) {
    // User cancelled or FZF not available
    return null;
  }
}

/**
 * Main session management function
 * @param {string} [projectName] - Optional project name, if not provided will prompt with FZF
 */
export function main(projectName) {
  // If no project name provided, use FZF to select one
  if (!projectName) {
    projectName = selectProjectWithFZF();

    if (!projectName) {
      console.log("No project selected.");
      process.exit(0);
    }
  }

  // Check if session already exists
  if (sessionExists(projectName)) {
    console.log(`Attaching to existing session: ${projectName}`);
    attachToSession(projectName);
    return;
  }

  // Find the project path
  const projectPath = findProjectPath(projectName);

  if (!projectPath) {
    console.error(`Project "${projectName}" not found in any search paths.`);
    process.exit(1);
  }

  // Create new session
  console.log(`Creating new session: ${projectName}`);

  if (createSession(projectName, projectPath)) {
    console.log(`Session "${projectName}" created successfully.`);
    attachToSession(projectName);
  } else {
    console.error(`Failed to create session "${projectName}".`);
    process.exit(1);
  }
}
