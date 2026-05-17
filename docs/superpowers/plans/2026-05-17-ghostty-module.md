# Ghostty Module Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `modules/user/applications/ghostty.nix` as a new Home Manager module that configures Ghostty with Catppuccin Macchiato theme, MonoLisa font, and core settings translated from the existing kitty config, while leaving `kitty.nix` completely untouched.

**Architecture:** Two files change — a new `ghostty.nix` module that uses `programs.ghostty` (Home Manager's built-in Ghostty support) for declarative config, and a small addition to `catppuccin.nix` to enable the catppuccin HM module's Ghostty theme hook. The module follows the exact option pattern used by `kitty.nix`.

**Tech Stack:** Nix, Home Manager `programs.ghostty`, `catppuccin` Home Manager flake module

---

## File Map

| File | Action | Responsibility |
|---|---|---|
| `modules/user/applications/ghostty.nix` | **Create** | Package install, `programs.ghostty.enable`, all settings, session variable, Linux desktop item |
| `modules/user/themes/catppuccin/catppuccin.nix` | **Modify** (lines 113–121) | Add `catppuccin.ghostty.enable` alongside existing `catppuccin.ghostty` block |

---

## Task 1: Create `ghostty.nix` module

**Files:**
- Create: `modules/user/applications/ghostty.nix`

- [ ] **Step 1: Write the module file**

```nix
# modules/user/applications/ghostty.nix
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.ghostty;
  desktop = pkgs.makeDesktopItem {
    name = "Ghostty";
    desktopName = "Ghostty";
    genericName = "Terminal emulator";
    icon = "utilities-terminal";
    exec = "uwsm app -- ${pkgs.ghostty}/bin/ghostty";
    categories = ["Development" "System" "Utility"];
  };
in {
  options.modules.applications.ghostty = mkOption {
    description = ''
      Configuration for the Ghostty terminal.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "ghostty";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  config = mkIf cfg.enable {
    home = {
      packages =
        if pkgs.stdenv.isLinux
        # Linux: custom .desktop wrapper only — avoids duplicate entry in rofi
        then [desktop pkgs.ghostty]
        # macOS: install directly (no rofi, no duplicate concern)
        else [pkgs.ghostty];

      sessionVariables = {
        TERMINAL = "ghostty";
        # TERM is intentionally not set here — Ghostty sets it when it spawns a
        # shell. Ghostty bundles its own terminfo (xterm-ghostty); no
        # TERMINFO_DIRS export needed unlike kitty.
      };
    };

    # Desktop item autostart — Linux/XDG only
    xdg.configFile = lib.mkIf pkgs.stdenv.isLinux {
      "autostart/ghostty.desktop".source = "${desktop}/share/applications/Ghostty.desktop";
    };

    programs.ghostty = {
      enable = true;
      settings =
        {
          font-family = "MonoLisa Variable";
          font-size = 18;
          # kitty adjust_line_height 150% → Ghostty counts the delta above 100%
          adjust-cell-height = "50%";
          cursor-style = "block";
          shell-integration = "detect";
          background-opacity = 1.0;
          copy-on-select = false;
          term = "xterm-ghostty";
        }
        // lib.optionalAttrs pkgs.stdenv.isDarwin {
          # macOS-specific settings
          macos-option-as-alt = true;
          macos-titlebar-style = "hidden";
        };
    };
  };
}
```

- [ ] **Step 2: Verify Darwin config evaluates**

```bash
cd /etc/dotfiles
nix eval .#darwinConfigurations.work.config.home-manager.users."jordan.faust".programs.ghostty.settings
```

Expected: attrset printed with `font-family`, `font-size`, `macos-option-as-alt`, etc. No errors.

- [ ] **Step 3: Verify NixOS dry-run still passes**

```bash
cd /etc/dotfiles
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```

Expected: exits 0, may print "these derivations will be built" or "nothing to be built". No eval errors.

- [ ] **Step 4: Commit**

```bash
cd /etc/dotfiles
git add modules/user/applications/ghostty.nix
git commit -m "feat: add ghostty terminal module"
```

---

## Task 2: Wire Catppuccin theme in `catppuccin.nix`

**Files:**
- Modify: `modules/user/themes/catppuccin/catppuccin.nix` (lines 114–121)

- [ ] **Step 1: Add `catppuccin.ghostty.enable` to the catppuccin block**

The existing block at lines 114–121 is:

```nix
      catppuccin = {
        enable = true;
        accent = "rosewater";
        flavor = "macchiato";

        gtk.icon.enable = pkgs.stdenv.isLinux;
        cursors.enable = pkgs.stdenv.isLinux;
      };
```

Replace it with:

```nix
      catppuccin = {
        enable = true;
        accent = "rosewater";
        flavor = "macchiato";

        gtk.icon.enable = pkgs.stdenv.isLinux;
        cursors.enable = pkgs.stdenv.isLinux;

        # Enable Ghostty theme when the ghostty module is active.
        # The catppuccin HM module drops the theme file into
        # ~/.config/ghostty/themes/catppuccin-macchiato and sets
        # programs.ghostty.settings.theme automatically.
        ghostty.enable =
          config.modules.applications.ghostty != null
          && config.modules.applications.ghostty.enable;
      };
```

- [ ] **Step 2: Verify Darwin config evaluates with theme set**

```bash
cd /etc/dotfiles
nix eval .#darwinConfigurations.work.config.home-manager.users."jordan.faust".programs.ghostty.settings
```

Expected: attrset now also contains `theme = "light:catppuccin-macchiato,dark:catppuccin-macchiato"`.

- [ ] **Step 3: Verify NixOS dry-run still passes**

```bash
cd /etc/dotfiles
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```

Expected: exits 0. No eval errors.

- [ ] **Step 4: Commit**

```bash
cd /etc/dotfiles
git add modules/user/themes/catppuccin/catppuccin.nix
git commit -m "feat: enable catppuccin ghostty theme in catppuccin module"
```

---

## Task 3: Final verification

- [ ] **Step 1: Confirm full flake evaluates**

```bash
cd /etc/dotfiles
nix flake show
```

Expected: tree printed without errors. Both `darwinConfigurations.work` and `nixosConfigurations.faust` listed.

- [ ] **Step 2: Confirm ghostty settings are complete on Darwin**

```bash
cd /etc/dotfiles
nix eval .#darwinConfigurations.work.config.home-manager.users."jordan.faust".programs.ghostty.settings
```

Expected output should contain all of these keys:
- `font-family = "MonoLisa Variable"`
- `font-size = 18`
- `adjust-cell-height = "50%"`
- `cursor-style = "block"`
- `shell-integration = "detect"`
- `background-opacity = 1`
- `copy-on-select = false`
- `term = "xterm-ghostty"`
- `macos-option-as-alt = true`
- `macos-titlebar-style = "hidden"`
- `theme = "light:catppuccin-macchiato,dark:catppuccin-macchiato"`

- [ ] **Step 3: Confirm kitty is unaffected**

```bash
cd /etc/dotfiles
nix eval .#darwinConfigurations.work.config.home-manager.users."jordan.faust".home.packages --apply 'pkgs: builtins.map (p: p.name or p.pname or "?") pkgs' 2>/dev/null | grep -E "kitty|ghostty" || \
nix eval .#darwinConfigurations.work.config.home-manager.users."jordan.faust".home.packages --apply 'builtins.length'
```

Expected: ghostty package present; kitty still present (both coexist).
