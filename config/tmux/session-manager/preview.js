#!/usr/bin/env node

/**
 * Project Preview Script
 * Shows top-level files and directories for a selected project
 */

import fs from "fs";
import path from "path";

/**
 * Gets file/directory info with icons and colors
 * @param {string} itemPath - Full path to the item
 * @param {string} itemName - Name of the item
 * @returns {string} Formatted line with icon and colors
 */
function formatItem(itemPath, itemName) {
  try {
    const stats = fs.lstatSync(itemPath);
    let icon = "";
    let color = "";

    if (stats.isDirectory()) {
      // Directory icons and colors
      if (itemName === ".git") {
        icon = ""; // Git icon
        color = "\x1b[38;2;237;135;150m"; // red
      } else if (itemName === "node_modules") {
        icon = ""; // Node.js icon
        color = "\x1b[38;2;166;218;149m"; // green
      } else if (itemName.startsWith(".")) {
        icon = ""; // Regular folder icon
        color = "\x1b[38;2;110;115;141m"; // overlay0 (dim)
      } else {
        icon = ""; // Regular folder icon
        color = "\x1b[38;2;138;173;244m"; // blue
      }
    } else {
      // File icons based on extension
      const ext = path.extname(itemName).toLowerCase();
      switch (ext) {
        case ".js":
        case ".mjs":
          icon = "";
          color = "\x1b[38;2;238;212;159m"; // yellow
          break;
        case ".ts":
          icon = "";
          color = "\x1b[38;2;238;212;159m"; // yellow
          break;
        case ".json":
          icon = "";
          color = "\x1b[38;2;238;212;159m"; // yellow
          break;
        case ".md":
        case ".markdown":
          icon = "";
          color = "\x1b[38;2;138;173;244m"; // blue
          break;
        case ".lua":
          icon = "";
          color = "\x1b[38;2;138;173;244m"; // blue
          break;
        case ".nix":
          icon = "";
          color = "\x1b[38;2;183;189;248m"; // lavender
          break;
        case ".lock":
          icon = "";
          color = "\x1b[38;2;110;115;141m"; // overlay0 (dim)
          break;
        case ".gitignore":
        case ".gitattributes":
          icon = "";
          color = "\x1b[38;2;237;135;150m"; // red
          break;
        default:
          icon = ""; // Regular file
          color = "\x1b[38;2;202;211;245m"; // text
      }
    }

    return `${color}${icon} ${itemName}\x1b[0m`;
  } catch (error) {
    return `  \x1b[38;2;110;115;141m${itemName}\x1b[0m`; // dim for error items
  }
}

/**
 * Main function to preview project directory
 * @param {string} projectPath - Path to the project directory
 */
function previewProject(projectPath) {
  try {
    if (!fs.existsSync(projectPath)) {
      console.log(`\x1b[38;2;237;135;150mError: Directory not found\x1b[0m`);
      console.log(`\x1b[38;2;110;115;141m${projectPath}\x1b[0m`);
      return;
    }

    const items = fs.readdirSync(projectPath, { withFileTypes: true });

    // Sort items: directories first, then files, both alphabetically
    const sortedItems = items.sort((a, b) => {
      if (a.isDirectory() && !b.isDirectory()) return -1;
      if (!a.isDirectory() && b.isDirectory()) return 1;
      return a.name.localeCompare(b.name);
    });

    // Show project path header
    console.log(`\x1b[38;2;198;160;246m${projectPath}\x1b[0m`); // mauve
    console.log("");

    // Show each item
    for (const item of sortedItems.slice(0, 20)) {
      // Limit to first 20 items
      const itemPath = path.join(projectPath, item.name);
      console.log(formatItem(itemPath, item.name));
    }

    // Show count if there are more items
    if (sortedItems.length > 20) {
      const remaining = sortedItems.length - 20;
      console.log("");
      console.log(`\x1b[38;2;110;115;141m... and ${remaining} more items\x1b[0m`);
    }
  } catch (error) {
    console.log(`\x1b[38;2;237;135;150mError reading directory:\x1b[0m`);
    console.log(`\x1b[38;2;110;115;141m${error.message}\x1b[0m`);
  }
}

// Get project path from command line argument
const projectPath = process.argv[2];

if (!projectPath) {
  console.log(`\x1b[38;2;237;135;150mUsage: node project-preview.js <project-path>\x1b[0m`);
  process.exit(1);
}

previewProject(projectPath);
