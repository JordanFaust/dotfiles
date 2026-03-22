#!/usr/bin/env node

/**
 * TMUX Session Manager CLI
 * Modular CLI for session management with chainable commands
 */

import { execSync } from "child_process";
import fs from "fs";
import path from "path";
import { discoverGitRepositories, getExistingSessions } from "./finder.js";
import { recordSessionAccess } from "./tracker.js";

/**
 * Creates a new TMUX session with the specified configuration
 * @param {string} sessionName - Name of the session to create
 * @param {string} projectPath - Root directory path for the project
 * @returns {boolean} True if session was created successfully
 */
function createSession(sessionName, projectPath) {
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
    const scriptPath = path.join(path.dirname(new URL(import.meta.url).pathname), "finder.js");
    const previewScript = path.join(path.dirname(new URL(import.meta.url).pathname), "preview.js");

    const fzfCommand = [
      "fzf",
      '--prompt="  "', // Search icon with space
      "--layout=reverse-list", // Puts prompt at bottom
      "--info=inline-right", // Show count on right side
      "--ansi",
      "--border=none",
      "--margin=0",
      "--padding=0,0,0,0", // Reduce bottom padding due to FZF limitations
      "--no-scrollbar",
      "--no-hscroll",
      "--no-mouse",
      "--separator=' '",
      "--color=fg+:#cad3f5,bg+:#2c3047,hl+:#8aadf4", // text, custom prompt background, blue
      "--color=info:#8aadf4,prompt:#8aadf4:bold,pointer:#ed8796", // blue, blue bold, red
      "--color=marker:#a6da95,spinner:#ed8796", // green, red
      "--color=gutter:#24273a",
      "--color=prompt:#eed49f,input-bg:#2c3047",
      "--pointer=▶",
      "--marker=✓",
      "--highlight-line",
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

  // Record session access for tracking
  recordSessionAccess(projectName);

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

/**
 * Resolves a workspace path to the canonical session name using the same
 * discovery logic as the FZF finder. Falls back to the directory basename
 * when the path doesn't match any known repository.
 * @param {string} workspacePath - Absolute path to the workspace
 * @returns {string} Canonical session name
 */
function resolveSessionName(workspacePath) {
  const repositories = discoverGitRepositories();

  let resolvedWorkspace;
  try {
    resolvedWorkspace = fs.realpathSync(path.resolve(workspacePath));
  } catch {
    resolvedWorkspace = path.resolve(workspacePath);
  }

  const match = repositories.find((repo) => {
    try {
      return fs.realpathSync(path.resolve(repo.path)) === resolvedWorkspace;
    } catch {
      return path.resolve(repo.path) === resolvedWorkspace;
    }
  });

  return match ? match.name : path.basename(workspacePath);
}

/**
 * Direct connect: resolves the workspace path to a canonical session name,
 * then creates or attaches to that session. Intended for editors (VS Code /
 * Cursor) that know the workspace path but should defer naming to the
 * session manager's discovery logic.
 * @param {string} workspacePath - Absolute path to the workspace
 */
function connectSession(workspacePath) {
  if (!workspacePath) {
    console.error("Error: Workspace path required");
    console.error("Usage: cli.js connect <path>");
    process.exit(1);
  }

  const resolvedPath = path.resolve(workspacePath);
  const sessionName = resolveSessionName(resolvedPath);

  recordSessionAccess(sessionName);

  if (sessionExists(sessionName)) {
    attachToSession(sessionName);
    return;
  }

  if (createSession(sessionName, resolvedPath)) {
    attachToSession(sessionName);
  } else {
    process.exit(1);
  }
}

/**
 * CLI Commands
 */

// CLI argument parsing
const [, , command, ...args] = process.argv;

function showHelp() {
  console.log(`
TMUX Session Manager CLI

Usage: cli.js <command> [options]

Commands:
  fzf [project]      Run the full FZF interface (default)
  connect <path>     Create or attach; derives session name via discovery
  finder [mode]      Run session finder (modes: simple, detailed)
  preview <path>     Run project preview for given path
  tracker <action>   Session tracking operations
  session <name>     Manage specific session
  help               Show this help

Examples:
  cli.js fzf                                         # Interactive session selection
  cli.js connect ~/github.com/procore/api-gateway    # → session "procore/api-gateway"
  cli.js connect /etc/dotfiles                       # → session "dotfiles"
  cli.js finder detailed                             # List sessions with formatting
  cli.js preview /path/to/proj                       # Preview project directory
  cli.js tracker list                                # List recent sessions
  cli.js session myproject                           # Create/attach to session
`);
}

function runFinder(mode = "detailed") {
  // Import and run finder
  const scriptPath = path.join(path.dirname(new URL(import.meta.url).pathname), "finder.js");
  try {
    const result = execSync(`node "${scriptPath}" ${mode}`, {
      encoding: "utf8",
      stdio: ["inherit", "pipe", "inherit"],
    });
    process.stdout.write(result);
  } catch (error) {
    process.exit(error.status || 1);
  }
}

function runPreview(projectPath) {
  if (!projectPath) {
    console.error("Error: Project path required for preview command");
    console.error("Usage: cli.js preview <path>");
    process.exit(1);
  }

  const scriptPath = path.join(path.dirname(new URL(import.meta.url).pathname), "preview.js");
  try {
    execSync(`node "${scriptPath}" "${projectPath}"`, {
      stdio: "inherit",
    });
  } catch (error) {
    process.exit(error.status || 1);
  }
}

function runTracker(action, ...trackerArgs) {
  const scriptPath = path.join(path.dirname(new URL(import.meta.url).pathname), "tracker.js");
  try {
    const fullArgs = action ? [action, ...trackerArgs] : [];
    execSync(`node "${scriptPath}" ${fullArgs.join(" ")}`, {
      stdio: "inherit",
    });
  } catch (error) {
    process.exit(error.status || 1);
  }
}

function runFZF(projectName) {
  if (projectName) {
    // Direct session management
    main(projectName);
  } else {
    // Interactive FZF selection
    main();
  }
}

// Command routing
switch (command) {
  case "finder":
    runFinder(args[0]);
    break;

  case "preview":
    runPreview(args[0]);
    break;

  case "tracker":
    runTracker(args[0], ...args.slice(1));
    break;

  case "connect":
    connectSession(args[0], args[1]);
    break;

  case "session":
    runFZF(args[0]);
    break;

  case "fzf":
  case undefined:
    // Default command - run FZF interface
    runFZF(args[0]);
    break;

  case "help":
  case "--help":
  case "-h":
    showHelp();
    break;

  default:
    runFZF(args[0]);
}
