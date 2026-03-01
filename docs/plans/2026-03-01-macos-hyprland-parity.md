# macOS Hyprland Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Bring the macOS `work` host AeroSpace config to like-for-like keybinding and look-and-feel parity with the NixOS Hyprland setup.

**Architecture:** Switch AeroSpace's primary modifier from `alt` to `cmd` (matching Hyprland's `SUPER`), add all missing bindings, complete window auto-assignment rules, activate skhd screenshot bindings, add Raycast via Homebrew, and set macOS dark mode. Raycast self-manages the `cmd-space` Spotlight conflict without Karabiner-Elements.

**Tech Stack:** AeroSpace (TOML config), skhd (hotkey daemon), nix-darwin `homebrew.casks`, nix-darwin `system.defaults`, Home Manager `xdg.configFile`

**Verification commands (run after each task):**
```bash
# Verify Darwin config still evaluates
nix eval .#darwinConfigurations.work.config.networking.hostName

# Verify NixOS builds are unaffected
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```

---

### Task 1: Update aerospace.toml — switch modifier and add missing bindings

**Files:**
- Modify: `modules/user/desktop/aerospace/aerospace.toml`

**Context:**
The current config uses `alt` as the primary modifier. We're switching to `cmd` to match Hyprland's `SUPER`. AeroSpace intercepts `cmd` combinations before apps receive them. The `automatically-unhide-macos-hidden-apps = true` setting prevents `cmd-h` from hiding windows as a side-effect.

**Step 1: Replace the entire aerospace.toml**

Replace `modules/user/desktop/aerospace/aerospace.toml` with the following:

```toml
# AeroSpace configuration
# Docs: https://nikitabobko.github.io/AeroSpace/guide
# Primary modifier: cmd (⌘) — matches Hyprland's SUPER key muscle memory

start-at-login = true

# Unhide any app that AeroSpace's cmd-h binding may have hidden as a side effect
automatically-unhide-macos-hidden-apps = true

# Normalizations
enable-normalization-flatten-containers = true
enable-normalization-opposite-orientation-for-nested-containers = true

# Mouse follows focus
on-focused-monitor-changed = ['move-mouse monitor-lazy-center']

[mode.main.binding]

# Disable macOS "Hide application" so cmd-h is fully available for focus
# See: https://nikitabobko.github.io/AeroSpace/goodies#disable-hide-app
cmd-alt-h = []

# Focus movement (vim-style) — matches SUPER+h/j/k/l
cmd-h = 'focus left'
cmd-j = 'focus down'
cmd-k = 'focus up'
cmd-l = 'focus right'

# Focus back-and-forth — matches ALT+Tab in Hyprland
cmd-grave = 'focus-back-and-forth'

# Move windows — matches SUPER+SHIFT+h/j/k/l
cmd-shift-h = 'move left'
cmd-shift-j = 'move down'
cmd-shift-k = 'move up'
cmd-shift-l = 'move right'

# Resize — matches SUPER+ALT+h/j/k/l
cmd-ctrl-h = 'resize width -50'
cmd-ctrl-l = 'resize width +50'
cmd-ctrl-k = 'resize height -50'
cmd-ctrl-j = 'resize height +50'

# Layouts
cmd-slash = 'layout tiles horizontal vertical'
cmd-comma = 'layout accordion horizontal vertical'

# Toggle split — matches SUPER+P
cmd-p = 'toggle-split'

# Fullscreen — matches SUPER+SHIFT+F (cmd-f reserved for Find in apps)
cmd-shift-f = 'macos-native-fullscreen'

# Close window — matches SUPER+Q
cmd-q = 'close'

# Launch terminal (kitty + tmux) — matches SUPER+Return
# Attaches to an existing non-attached tmux session, or creates a new one
cmd-return = '''exec-and-forget osascript -e '
tell application "kitty"
    activate
    tell application "System Events" to keystroke "d" using {control down, shift down}
end tell' '''

# App launcher — cmd-space is handled by Raycast directly (set cmd-space in Raycast prefs)
# Clipboard history — matches SUPER+V (cmd-v reserved for paste)
cmd-shift-v = 'exec-and-forget open "raycast://extensions/raycast/clipboard-history/clipboard-history"'

# Workspace switching — matches SUPER+1-5
cmd-1 = 'workspace 1'
cmd-2 = 'workspace 2'
cmd-3 = 'workspace 3'
cmd-4 = 'workspace 4'
cmd-5 = 'workspace 5'

# Move window to workspace — matches SUPER+CTRL+1-5
cmd-shift-1 = 'move-node-to-workspace 1'
cmd-shift-2 = 'move-node-to-workspace 2'
cmd-shift-3 = 'move-node-to-workspace 3'
cmd-shift-4 = 'move-node-to-workspace 4'
cmd-shift-5 = 'move-node-to-workspace 5'

# Workspace cycling — matches SUPER+left/right
cmd-left  = 'workspace prev'
cmd-right = 'workspace next'

# Move to adjacent workspace — matches SUPER+CTRL+left/right
cmd-shift-left  = 'move-node-to-workspace prev'
cmd-shift-right = 'move-node-to-workspace next'

# Service mode for less-used commands
cmd-shift-semicolon = 'mode service'

[mode.service.binding]
esc = ['reload-config', 'mode main']
r = ['flatten-workspace-tree', 'mode main']
f = ['layout floating tiling', 'mode main']
backspace = ['close-all-windows-but-current', 'mode main']

# Window auto-assignment — mirrors Hyprland windowrule workspace assignments
# Use app-id for precision. Get IDs via: aerospace list-apps
[[on-window-detected]]
if.app-id = 'net.kovidgoyal.kitty'
run = 'move-node-to-workspace 2'

[[on-window-detected]]
if.app-id = 'org.mozilla.firefox'
run = 'move-node-to-workspace 2'

[[on-window-detected]]
if.app-id = 'com.tinyspeck.slackmacgap'
run = 'move-node-to-workspace 3'

[[on-window-detected]]
if.app-id = 'com.spotify.client'
run = 'move-node-to-workspace 3'

[[on-window-detected]]
if.app-id = 'us.zoom.xos'
run = 'move-node-to-workspace 4'

[[on-window-detected]]
if.app-id = 'com.google.Chrome'
run = 'move-node-to-workspace 4'
```

**Step 2: Verify Darwin config evaluates**

```bash
cd /etc/dotfiles
nix eval .#darwinConfigurations.work.config.networking.hostName
```

Expected output: `"work"`

**Step 3: Verify NixOS is unaffected**

```bash
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```

Expected: exits 0, prints store path.

**Step 4: Commit**

```bash
cd /etc/dotfiles
git add modules/user/desktop/aerospace/aerospace.toml
git commit -m "feat(aerospace): switch to cmd modifier for Hyprland parity

- Switch primary modifier from alt to cmd (matches SUPER muscle memory)
- Add all missing bindings: terminal, workspace cycling, focus-back-and-forth
- Add cmd-shift-f fullscreen, cmd-shift-v clipboard history
- Add window auto-assignment rules for kitty, firefox, spotify, chrome
- Enable automatically-unhide-macos-hidden-apps to neutralise cmd-h side effects
- Disable cmd-alt-h (hide others) to free modifier space"
```

---

### Task 2: Activate skhd screenshot bindings

**Files:**
- Modify: `modules/user/desktop/skhd/skhdrc`

**Context:**
Hyprland binds `Home` to a region screenshot saved to file, and `SUPER+Home` to region screenshot copied to clipboard. skhd handles system-level bindings that AeroSpace doesn't. The `home` key in skhd syntax is `home`. `screencapture -is` opens interactive region selection and saves to file; `-ic` copies to clipboard.

**Step 1: Update skhdrc**

Replace the contents of `modules/user/desktop/skhd/skhdrc`:

```
# skhdrc — skhd hotkey daemon configuration
# Docs: https://github.com/koekeishiya/skhd
#
# cmd-space is configured directly in Raycast preferences (replaces Spotlight).
# Screenshots mirror Hyprland: Home = region to file, shift+Home = region to clipboard.

# Screenshot region to file — matches Hyprland: Home
home : screencapture -is ~/Pictures/screenshot-$(date +%Y%m%d-%H%M%S).png

# Screenshot region to clipboard — matches Hyprland: SUPER+Home
shift - home : screencapture -ic
```

**Step 2: Verify Darwin evaluates**

```bash
nix eval .#darwinConfigurations.work.config.networking.hostName
```

Expected: `"work"`

**Step 3: Commit**

```bash
git add modules/user/desktop/skhd/skhdrc
git commit -m "feat(skhd): activate screenshot bindings to match Hyprland Home key"
```

---

### Task 3: Add Raycast to Homebrew casks

**Files:**
- Modify: `hosts/darwin/work/default.nix`

**Context:**
Raycast replaces rofi for app launching and provides clipboard history. It is installed as a macOS cask (not available in nixpkgs). Once installed, configure `cmd-space` as Raycast's activation hotkey inside the Raycast app itself — it will prompt to disable Spotlight's shortcut automatically.

**Step 1: Add raycast to casks**

In `hosts/darwin/work/default.nix`, update the `homebrew.casks` list:

```nix
homebrew.casks = [
  "raycast"
];
```

**Step 2: Verify Darwin evaluates**

```bash
nix eval .#darwinConfigurations.work.config.networking.hostName
```

Expected: `"work"`

**Step 3: Commit**

```bash
git add hosts/darwin/work/default.nix
git commit -m "feat(darwin/work): add raycast cask for app launcher and clipboard history"
```

---

### Task 4: Add dark mode and appearance defaults

**Files:**
- Modify: `modules/darwin/defaults.nix`

**Context:**
Hyprland uses a dark Catppuccin Macchiato theme. macOS system appearance should match. `AppleInterfaceStyle = "Dark"` enables dark mode. `AppleReduceTransparency = true` gives a cleaner, more opaque look closer to Hyprland's rendering. These are declarative nix-darwin options applied on `darwin-rebuild switch`.

**Step 1: Add appearance settings to defaults.nix**

In `modules/darwin/defaults.nix`, extend the `NSGlobalDomain` block to add:

```nix
NSGlobalDomain = {
  # existing keys stay ...
  KeyRepeat = 1;
  InitialKeyRepeat = 10;
  NSNavPanelExpandedStateForSaveMode = true;
  PMPrintingExpandedStateForPrint = true;

  # Dark mode — matches Catppuccin Macchiato dark theme
  AppleInterfaceStyle = "Dark";
  # Reduce transparency for cleaner, more opaque windows (closer to Hyprland feel)
  AppleReduceTransparency = true;
};
```

**Step 2: Verify Darwin evaluates**

```bash
nix eval .#darwinConfigurations.work.config.system.defaults.NSGlobalDomain.AppleInterfaceStyle
```

Expected output: `"Dark"`

**Step 3: Verify NixOS is unaffected**

```bash
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```

Expected: exits 0.

**Step 4: Commit**

```bash
git add modules/darwin/defaults.nix
git commit -m "feat(darwin/defaults): add dark mode and reduce transparency for Catppuccin parity"
```

---

### Task 5: Verify terminal launch binding

**Context:**
The `cmd-return` binding in aerospace.toml uses AppleScript to activate kitty. This needs verification on the actual machine because kitty's AppleScript support and the exact `tell application "kitty"` syntax may need adjustment. The Hyprland binding runs `kitty -e bash -c "(tmux ls | grep -qEv 'attached|scratch' && tmux at) || tmux"`.

A simpler macOS equivalent that works with kitty's `open -na` invocation:

```toml
cmd-return = 'exec-and-forget open -na kitty'
```

Or with tmux session reuse via a shell script:

```toml
cmd-return = 'exec-and-forget /bin/bash -c "open -na kitty --args -e bash -c \"(tmux ls 2>/dev/null | grep -qEv attached && tmux attach) || tmux\""'
```

**Step 1: Test the binding on the work machine**

After activating with `darwin-rebuild switch --flake /etc/dotfiles#work`:

1. Press `cmd-return` — kitty should open
2. If AppleScript approach fails, update `aerospace.toml` to use the `open -na` form above
3. Confirm tmux attaches to existing session if one is running

**Step 2: Update binding if needed and commit**

```bash
git add modules/user/desktop/aerospace/aerospace.toml
git commit -m "fix(aerospace): adjust kitty launch binding for macOS open invocation"
```

---

### Task 6: Final activation and smoke test

**Step 1: Apply the configuration**

On the work macOS machine:

```bash
darwin-rebuild switch --flake /etc/dotfiles#work
```

**Step 2: Configure Raycast (one-time manual)**

1. Open Raycast from Applications
2. Go to Raycast Settings → General → Raycast Hotkey
3. Set to `⌘ Space`
4. Accept the prompt to disable Spotlight's shortcut

**Step 3: Smoke test all bindings**

| Binding | Expected |
|---|---|
| `cmd-h/j/k/l` | Focus moves left/down/up/right between windows |
| `cmd-shift-h/j/k/l` | Window moves in that direction |
| `cmd-ctrl-h/l` | Window resizes narrower/wider |
| `cmd-1` through `cmd-5` | Switches to workspace 1–5 |
| `cmd-shift-1` | Moves focused window to workspace 1 |
| `cmd-left/right` | Cycles to previous/next workspace |
| `cmd-return` | Opens kitty with tmux |
| `cmd-space` | Opens Raycast |
| `cmd-shift-v` | Opens Raycast clipboard history |
| `cmd-q` | Closes focused window |
| `cmd-shift-f` | Enters macOS native fullscreen |
| `cmd-grave` | Switches back to last focused window |
| `home` | Opens region screenshot selector, saves to ~/Pictures |
| `shift+home` | Opens region screenshot selector, copies to clipboard |
| Launch kitty | Auto-moves to workspace 2 |
| Launch Firefox | Auto-moves to workspace 2 |
| Launch Slack | Auto-moves to workspace 3 |
| Launch Spotify | Auto-moves to workspace 3 |
| Launch Zoom | Auto-moves to workspace 4 |
| Launch Chrome | Auto-moves to workspace 4 |

**Step 4: Verify dark mode applied**

System Settings → Appearance should show Dark mode enabled.

---

## Post-Activation Notes

- **Raycast clipboard**: By default the Raycast clipboard extension may not be installed. Install it from the Raycast Store (search "Clipboard History"). The `cmd-shift-v` deeplink will then work.
- **App IDs**: If a window rule doesn't fire, verify the app-id with `aerospace list-apps` on the machine. Some apps (e.g. a work-specific Slack variant) may have a different bundle ID.
- **`cmd-return` / tmux**: If the tmux session reuse doesn't work via `open -na`, simplify to `open -na kitty` and manage tmux manually inside kitty.
