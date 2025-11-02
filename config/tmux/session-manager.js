#!/usr/bin/env node

/**
 * TMUX Session Manager Executable
 * Simple entry point that imports and runs the session management logic
 */

import { main } from "./session-utils.js";

// Get project name from command line arguments (if provided)
const projectName = process.argv[2];

// Always run the main function
main(projectName);

