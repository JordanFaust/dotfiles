// wm.js — minimal hotkey-driven window commands for macOS, no third-party WM.
//
// Run via `osascript -l JavaScript wm.js <command> [arg]` (wrapped as `wm` by
// ../skhd/default.nix and bound in skhdrc). Uses only Apple-provided APIs:
// CoreGraphics to list on-screen windows and System Events (Accessibility) to
// focus/move them. Accessibility is attributed to skhd, which already has it.
//
// Commands:
//   focus <left|down|up|right>   focus nearest window in that direction
//   move  <left|down|up|right>   swap with neighbor; at the edge snap to that half
//   place <left|right|full>      snap focused window to half / full (with gaps)
//   resize <dw> <dh>             grow/shrink focused window by pixels
//   tile                         tile all windows on the current Space
//   send <n>                     carry focused window to desktop n
//   debug                        print what the script sees
ObjC.import('CoreGraphics');
ObjC.import('AppKit');

// Gaps mirror the old AeroSpace config (top 80 leaves room for sketchybar).
const GAP = {inner: 28, left: 28, right: 28, top: 80, bottom: 28};
const IGNORE = ['borders', 'Dock', 'Window Server', 'SketchyBar', 'sketchybar',
  'Control Center', 'Notification Center', 'Raycast', 'Okta Verify'];

// Screens in top-left-origin global coordinates (same space as CG/AX).
function screens() {
  const all = $.NSScreen.screens.js;
  const mainH = all[0].frame.size.height;
  return all.map(s => {
    const f = s.frame;
    return {x: f.origin.x, y: mainH - f.origin.y - f.size.height, w: f.size.width, h: f.size.height};
  });
}

function overlap(a, b) {
  const w = Math.min(a.x + a.w, b.x + b.w) - Math.max(a.x, b.x);
  const h = Math.min(a.y + a.h, b.y + b.h) - Math.max(a.y, b.y);
  return w > 0 && h > 0 ? w * h : 0;
}

function screenOf(win) {
  let best = null, bestArea = 0;
  for (const s of screens()) {
    const a = overlap(win, s);
    if (a > bestArea) { best = s; bestArea = a; }
  }
  return best;
}

// Usable area of a screen after outer gaps.
function area(s) {
  return {x: s.x + GAP.left, y: s.y + GAP.top,
    w: s.w - GAP.left - GAP.right, h: s.h - GAP.top - GAP.bottom};
}

// Normal windows on the current Space(s), front-to-back. Windows parked
// off-screen (e.g. left in a corner by AeroSpace) are included as long as any
// part intersects a screen, so `tile` can recover them.
function windows() {
  const raw = ObjC.deepUnwrap(ObjC.castRefToObject(
    // 17 = kCGWindowListOptionOnScreenOnly (1) | kCGWindowListExcludeDesktopElements (16);
    // 0 = kCGNullWindowID. Literals because JXA doesn't reliably bridge these enums.
    $.CGWindowListCopyWindowInfo(17, 0))) || [];
  return raw
    .filter(w => w.kCGWindowLayer === 0 && IGNORE.indexOf(w.kCGWindowOwnerName) < 0)
    .filter(w => w.kCGWindowAlpha === undefined || w.kCGWindowAlpha > 0)
    .map(w => ({pid: w.kCGWindowOwnerPID, app: w.kCGWindowOwnerName,
      x: w.kCGWindowBounds.X, y: w.kCGWindowBounds.Y, w: w.kCGWindowBounds.Width, h: w.kCGWindowBounds.Height}))
    .filter(w => w.w >= 100 && w.h >= 100 && screenOf(w) !== null);
}

function focused(wins) {
  const front = $.NSWorkspace.sharedWorkspace.frontmostApplication;
  const pid = front.isNil() ? -1 : front.processIdentifier;
  return wins.find(w => w.pid === pid) || null;
}

const center = w => ({x: w.x + w.w / 2, y: w.y + w.h / 2});

// Nearest window in a direction: distance along the axis plus a penalty for
// being off-axis, so the visually adjacent window wins.
function neighbor(cur, wins, dir) {
  const c = center(cur);
  let best = null, bestScore = Infinity;
  for (const w of wins) {
    if (w === cur) continue;
    const o = center(w);
    const dx = o.x - c.x, dy = o.y - c.y;
    let along, across;
    if (dir === 'left') { along = -dx; across = Math.abs(dy); }
    else if (dir === 'right') { along = dx; across = Math.abs(dy); }
    else if (dir === 'up') { along = -dy; across = Math.abs(dx); }
    else if (dir === 'down') { along = dy; across = Math.abs(dx); }
    else throw new Error('bad direction: ' + dir);
    if (along <= 1) continue;
    const score = along + 2 * across;
    if (score < bestScore) { best = w; bestScore = score; }
  }
  return best;
}

// --- Accessibility (System Events) ---------------------------------------
const SE = Application('System Events');

function axWindow(win) {
  const proc = SE.processes.whose({unixId: win.pid})[0];
  const axWins = proc.windows();
  for (const a of axWins) {
    try {
      const [px, py] = a.position();
      const [sw, sh] = a.size();
      if (Math.abs(px - win.x) <= 2 && Math.abs(py - win.y) <= 2 &&
          Math.abs(sw - win.w) <= 2 && Math.abs(sh - win.h) <= 2) return {proc, ax: a};
    } catch (e) { /* window vanished mid-scan */ }
  }
  return {proc, ax: axWins.length ? axWins[0] : null};
}

function focusWin(win) {
  const {proc, ax} = axWindow(win);
  proc.frontmost = true;
  if (ax) {
    try { ax.actions.byName('AXRaise').perform(); } catch (e) {}
    try { ax.attributes.byName('AXMain').value = true; } catch (e) {}
  }
}

function setFrame(win, r) {
  const {ax} = axWindow(win);
  if (!ax) return;
  const f = {x: Math.round(r.x), y: Math.round(r.y), w: Math.round(r.w), h: Math.round(r.h)};
  // Position, size, position: some apps clamp size against the old origin.
  ax.position = [f.x, f.y];
  ax.size = [f.w, f.h];
  ax.position = [f.x, f.y];
  Object.assign(win, f);
}

function half(a, side) {
  const w = (a.w - GAP.inner) / 2;
  return {x: side === 'left' ? a.x : a.x + w + GAP.inner, y: a.y, w, h: a.h};
}

// --- Commands ---------------------------------------------------------------
function place(cur, where) {
  const a = area(screenOf(cur));
  if (where === 'full') setFrame(cur, a);
  else if (where === 'left' || where === 'right') setFrame(cur, half(a, where));
  else throw new Error('bad placement: ' + where);
}

function move(cur, wins, dir) {
  const other = neighbor(cur, wins, dir);
  if (other) {
    const a = {x: cur.x, y: cur.y, w: cur.w, h: cur.h};
    const b = {x: other.x, y: other.y, w: other.w, h: other.h};
    setFrame(cur, b);
    setFrame(other, a);
  } else if (dir === 'left' || dir === 'right') {
    place(cur, dir);
  }
}

// 1 window: full. 2: halves. 3+: leftmost window as master on the left half,
// the rest stacked on the right. Order follows current x position so a re-tile
// keeps windows roughly where they were.
function tile(cur, wins) {
  const s = screenOf(cur);
  const mine = wins.filter(w => screenOf(w) === s || overlap(w, s) > 0)
    .sort((p, q) => p.x - q.x || p.y - q.y);
  const a = area(s);
  if (mine.length === 1) { setFrame(mine[0], a); return; }
  setFrame(mine[0], half(a, 'left'));
  const right = half(a, 'right');
  const rest = mine.slice(1);
  const h = (right.h - GAP.inner * (rest.length - 1)) / rest.length;
  rest.forEach((w, i) => setFrame(w, {x: right.x, y: right.y + i * (h + GAP.inner), w: right.w, h}));
}

// --- Send window to desktop N ------------------------------------------------
// macOS has no public API to move a window between Spaces. Instead, pick the
// window up with a ctrl-cmd drag (enabled by NSWindowShouldDragOnGesture, set in
// spaces.nix — works anywhere in the window, so no title-bar hit-testing), press
// the "Switch to Desktop N" shortcut while holding it, then drop it.
const CMD = 0x100000, CTRL = 0x40000;
const EV = {down: 1, up: 2, moved: 5, dragged: 6};

function desktopHotkey(n) {
  const d = $.NSUserDefaults.alloc.initWithSuiteName('com.apple.symbolichotkeys');
  const all = ObjC.deepUnwrap(d.dictionaryForKey('AppleSymbolicHotKeys')) || {};
  const hk = all[String(117 + n)]; // 118 = Switch to Desktop 1
  if (!hk || !hk.enabled || !hk.value) return null;
  const [, code, mods] = hk.value.parameters;
  return {code, mods};
}

function mouse(type, p, flags) {
  const e = $.CGEventCreateMouseEvent(null, type, p, 0);
  $.CGEventSetFlags(e, flags);
  $.CGEventPost(0, e);
}

function key(code, down, flags) {
  const e = $.CGEventCreateKeyboardEvent(null, code, down);
  $.CGEventSetFlags(e, flags);
  $.CGEventPost(0, e);
}

function send(cur, n) {
  const hk = desktopHotkey(n);
  if (!hk) throw new Error('"Switch to Desktop ' + n + '" shortcut is disabled or missing');
  const enabled = $.NSUserDefaults.standardUserDefaults.boolForKey('NSWindowShouldDragOnGesture');
  if (!enabled) throw new Error('NSWindowShouldDragOnGesture is off; run darwin-rebuild and relaunch apps');

  const saved = $.CGEventGetLocation($.CGEventCreate(null));
  const grab = {x: cur.x + cur.w / 2, y: cur.y + cur.h / 2};
  const drag = CMD | CTRL;

  mouse(EV.moved, grab, 0); delay(0.05);
  mouse(EV.down, grab, drag); delay(0.1);
  mouse(EV.dragged, {x: grab.x + 4, y: grab.y + 4}, drag); delay(0.05);
  mouse(EV.dragged, {x: grab.x + 8, y: grab.y + 8}, drag); delay(0.1);
  key(hk.code, true, hk.mods);
  key(hk.code, false, hk.mods);
  delay(0.7); // let the Space transition finish before dropping
  mouse(EV.up, {x: grab.x + 8, y: grab.y + 8}, 0);
  delay(0.05);
  $.CGWarpMouseCursorPosition(saved);
}

function run(argv) {
  const [cmd, arg, arg2] = argv;
  const wins = windows();
  if (cmd === 'debug') {
    const cur = focused(wins);
    return JSON.stringify({screens: screens(), focused: cur, windows: wins}, null, 2);
  }
  const cur = focused(wins);
  if (!cur) return;
  if (cmd === 'focus') { const n = neighbor(cur, wins, arg); if (n) focusWin(n); }
  else if (cmd === 'move') move(cur, wins, arg);
  else if (cmd === 'place') place(cur, arg);
  else if (cmd === 'resize') setFrame(cur, {x: cur.x, y: cur.y, w: cur.w + Number(arg), h: cur.h + Number(arg2 || 0)});
  else if (cmd === 'tile') tile(cur, wins);
  else if (cmd === 'send') send(cur, Number(arg));
  else throw new Error('usage: wm <focus|move|place|resize|tile|send|debug> ...');
}
