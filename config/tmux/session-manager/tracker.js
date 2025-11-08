/**
 * Session Access Tracker
 * Manages persistent state for session access history
 */

import fs from "fs";
import path from "path";
import os from "os";

// State file location
const STATE_DIR = path.join(os.homedir(), ".local", "share", "tmux");
const STATE_FILE = path.join(STATE_DIR, "session-history.json");

/**
 * Ensures the state directory exists
 */
function ensureStateDirectory() {
  if (!fs.existsSync(STATE_DIR)) {
    fs.mkdirSync(STATE_DIR, { recursive: true });
  }
}

/**
 * Loads session access history from disk
 * @returns {Object} Session history data
 */
function loadSessionHistory() {
  try {
    if (!fs.existsSync(STATE_FILE)) {
      return { sessions: {}, lastAccessed: [] };
    }

    const data = fs.readFileSync(STATE_FILE, "utf8");
    const parsed = JSON.parse(data);

    // Validate structure
    return {
      sessions: parsed.sessions || {},
      lastAccessed: parsed.lastAccessed || [],
    };
  } catch (error) {
    console.error(`Warning: Could not load session history: ${error.message}`);
    return { sessions: {}, lastAccessed: [] };
  }
}

/**
 * Saves session access history to disk
 * @param {Object} history - Session history data
 */
function saveSessionHistory(history) {
  try {
    ensureStateDirectory();
    fs.writeFileSync(STATE_FILE, JSON.stringify(history, null, 2));
  } catch (error) {
    console.error(`Warning: Could not save session history: ${error.message}`);
  }
}

/**
 * Records access to a session
 * @param {string} sessionName - Name of the session
 */
export function recordSessionAccess(sessionName) {
  const history = loadSessionHistory();
  const now = Date.now();

  // Update session access data
  if (!history.sessions[sessionName]) {
    history.sessions[sessionName] = {
      accessCount: 0,
      firstAccessed: now,
      lastAccessed: now,
    };
  }

  history.sessions[sessionName].accessCount++;
  history.sessions[sessionName].lastAccessed = now;

  // Update recent access list (keep only last 20)
  history.lastAccessed = history.lastAccessed.filter((name) => name !== sessionName);
  history.lastAccessed.unshift(sessionName);
  history.lastAccessed = history.lastAccessed.slice(0, 20);

  saveSessionHistory(history);
}

/**
 * Gets session access data
 * @param {string} sessionName - Name of the session
 * @returns {Object|null} Session access data or null if not found
 */
export function getSessionAccess(sessionName) {
  const history = loadSessionHistory();
  return history.sessions[sessionName] || null;
}

/**
 * Gets recently accessed sessions in order
 * @returns {string[]} Array of session names in recent access order
 */
export function getRecentlyAccessedSessions() {
  const history = loadSessionHistory();
  return history.lastAccessed;
}

/**
 * Gets all session access data
 * @returns {Object} All session access data
 */
export function getAllSessionHistory() {
  return loadSessionHistory();
}

/**
 * Cleans up old session data (removes sessions not accessed in 30 days)
 */
export function cleanupOldSessions() {
  const history = loadSessionHistory();
  const thirtyDaysAgo = Date.now() - 30 * 24 * 60 * 60 * 1000;

  let hasChanges = false;

  // Remove old sessions
  for (const [sessionName, data] of Object.entries(history.sessions)) {
    if (data.lastAccessed < thirtyDaysAgo) {
      delete history.sessions[sessionName];
      hasChanges = true;
    }
  }

  // Clean up recent access list
  const oldLength = history.lastAccessed.length;
  history.lastAccessed = history.lastAccessed.filter(
    (sessionName) => history.sessions[sessionName]
  );

  if (history.lastAccessed.length !== oldLength) {
    hasChanges = true;
  }

  if (hasChanges) {
    saveSessionHistory(history);
  }
}

// CLI functionality
if (process.argv[1] === new URL(import.meta.url).pathname) {
  const command = process.argv[2];

  switch (command) {
    case "record":
      const sessionName = process.argv[3];
      if (sessionName) {
        recordSessionAccess(sessionName);
        console.log(`Recorded access to session: ${sessionName}`);
      } else {
        console.error("Usage: session-tracker.js record <session-name>");
      }
      break;

    case "list":
      const recent = getRecentlyAccessedSessions();
      console.log("Recently accessed sessions:");
      recent.forEach((name, index) => {
        console.log(`  ${index + 1}. ${name}`);
      });
      break;

    case "cleanup":
      cleanupOldSessions();
      console.log("Cleaned up old session data");
      break;

    case "show":
      const allHistory = getAllSessionHistory();
      console.log(JSON.stringify(allHistory, null, 2));
      break;

    default:
      console.log("Usage: session-tracker.js <command>");
      console.log("Commands:");
      console.log("  record <session-name>  - Record access to a session");
      console.log("  list                   - List recently accessed sessions");
      console.log("  cleanup                - Clean up old session data");
      console.log("  show                   - Show all session history");
  }
}
