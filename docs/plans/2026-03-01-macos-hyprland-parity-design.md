# macOS Hyprland Parity — Design Document

**Date:** 2026-03-01
**Branch:** feat/darwin-support

## Goal

Bring the macOS `work` host to like-for-like keybinding, window management, and look-and-feel parity with the NixOS Hyprland setup, prioritising muscle memory and daily workflow continuity.

## Context

The NixOS hosts use Hyprland with `SUPER` (Windows/Meta key) as the primary WM modifier. The macOS `work` host uses AeroSpace with `alt` (Option) as the primary modifier — a significant muscle-memory divergence.

AeroSpace CAN intercept `cmd` key combinations before apps receive them (confirmed by AeroSpace maintainer and community). The `automatically-unhide-macos-hidden-apps` config option handles the `cmd-h` hide side-effect. This means `cmd` can serve as the macOS equivalent of `SUPER` for all WM bindings, with two targeted exceptions.

## Decisions

### Modifier key: `cmd` instead of `alt`

`cmd` (⌘) is the physical and conceptual equivalent of `SUPER`/Windows key on a Mac keyboard. AeroSpace intercepts it before apps, with one known system-level exception (`cmd-space` = Spotlight) handled by Raycast's own hotkey takeover.

### App launcher: Raycast (not rofi, not Karabiner)

Rofi requires XQuartz (X11 server) on macOS — impractical with AeroSpace which is fully native. Karabiner-Elements requires a DriverKit System Extension that corporate MDM policies may block. Raycast handles `cmd-space` takeover via its own Accessibility API without any driver, and provides both app launching and clipboard history.

### Karabiner-Elements: excluded

Only needed for `cmd-space` Spotlight interception. Raycast's own hotkey configuration supersedes this without requiring a system extension. This avoids the broken `services.karabiner-elements` nix-darwin module (open issue since v15.0, Aug 2024) and any MDM policy conflicts.

### `cmd-v` (paste) and `cmd-f` (find): not overridden

These are too fundamental to macOS app usage to reassign. Equivalent Hyprland bindings are remapped:
- `SUPER+V` (clipboard) → `cmd-shift-v`
- `SUPER+SHIFT+F` (fullscreen) → `cmd-shift-f`

## Binding Map: Hyprland → AeroSpace

| Hyprland | AeroSpace (new) | Action |
|---|---|---|
| `SUPER+h/j/k/l` | `cmd-h/j/k/l` | Focus left/down/up/right |
| `SUPER+SHIFT+h/j/k/l` | `cmd-shift-h/j/k/l` | Move window |
| `SUPER+ALT+h/j/k/l` | `cmd-ctrl-h/j/k/l` | Resize |
| `SUPER+1-5` | `cmd-1-5` | Switch workspace |
| `SUPER+CTRL+1-5` | `cmd-shift-1-5` | Move window to workspace |
| `SUPER+left/right` | `cmd-left/right` | Workspace prev/next |
| `SUPER+CTRL+left/right` | `cmd-shift-left/right` | Move to prev/next workspace |
| `SUPER+Return` | `cmd-return` | Launch kitty + tmux |
| `SUPER+Space` | Raycast (self-managed) | App launcher |
| `SUPER+V` | `cmd-shift-v` | Raycast clipboard history |
| `SUPER+Q` | `cmd-q` | Close window |
| `SUPER+SHIFT+F` | `cmd-shift-f` | macOS native fullscreen |
| `SUPER+P` | `cmd-p` | Toggle split |
| `ALT+Tab` | `cmd-grave` | Focus back-and-forth |
| `Home` (screenshot) | `skhd: home` | Region screenshot to file |
| `SUPER+Home` (screenshot clipboard) | `skhd: shift+home` | Region screenshot to clipboard |

## Window Rules: Hyprland → AeroSpace

| App | Workspace | Source |
|---|---|---|
| kitty | 2 | Added (was Linux only) |
| Firefox | 2 | Added (was Linux only) |
| Slack | 3 | Exists (update to use app-id) |
| Spotify | 3 | Added (was Linux only) |
| Zoom | 4 | Exists |
| Google Chrome | 4 | Added (was Linux only) |

## Files Changed

| File | Change |
|---|---|
| `modules/user/desktop/aerospace/aerospace.toml` | Switch to `cmd` modifier; add all missing bindings; add window rules; add hide-app handling |
| `modules/user/desktop/skhd/skhdrc` | Activate screenshot bindings |
| `hosts/darwin/work/default.nix` | Add `raycast` to `homebrew.casks` |
| `modules/darwin/defaults.nix` | Add dark mode, reduce transparency |

## Out of Scope

- Hyprpanel / status bar equivalent (sketchybar would be the macOS equivalent — separate effort)
- Hyprlock / hypridle equivalent (macOS has native screen lock)
- Catppuccin Raycast theme (manual, one-time setup in Raycast app)
- Rofi theming on macOS (not viable without X11)
