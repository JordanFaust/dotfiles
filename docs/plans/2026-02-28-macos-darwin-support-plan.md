# macOS Darwin Support Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extend this NixOS dotfiles flake to support an Apple Silicon work Mac via nix-darwin + Determinate Nix, reusing the existing `modules/user/` Home Manager layer.

**Architecture:** Add `lib/darwin.nix` (mirrors `lib/nixos.nix`), reorganize `hosts/` into `hosts/nixos/` and `hosts/darwin/`, and place nix-darwin system modules at `modules/darwin/` (NOT `modules/system/darwin/` — that path is auto-imported into every NixOS build by `default.nix` line 15). Guard Linux-only user modules with `pkgs.stdenv.isLinux`. Add AeroSpace and skhd as macOS desktop modules.

**Tech Stack:** nix-darwin (`lnl7/nix-darwin`), Home Manager, Determinate Nix, AeroSpace, skhd, nixpkgs `aarch64-darwin`

**Design doc:** `docs/plans/2026-02-28-macos-darwin-support-design.md`

---

## Critical Path Notes

**`default.nix` (repo root) line 15:**
```nix
mapModulesRec' (toString ./modules/system) import
```
This recursively imports everything under `modules/system/` into every NixOS build. Darwin system modules at `modules/system/darwin/` would be evaluated in a NixOS context and fail (nix-darwin options like `homebrew`, `system.defaults.*` don't exist). **Darwin system modules go in `modules/darwin/` instead.**

**`hosts/*/home.nix` path to user modules:**
Currently `../../modules/user`. After moving hosts to `hosts/nixos/*/`, this becomes `../../../modules/user` in every host's `home.nix`.

**`hosts/*/default.nix` path to `local.nix`:**
Currently `../local.nix`. `local.nix` moves alongside to `hosts/nixos/local.nix`, so `../local.nix` stays correct in `hosts/nixos/*/default.nix`.

**Flake `system` variable:**
Currently `system = "x86_64-linux"` is used throughout `flake.nix`. The `overlay` attribute closes over it. This must be replaced with `linuxSystem`/`darwinSystem` and the overlay updated.

---

## Task 1: Reorganize hosts/ into hosts/nixos/

**Files:**
- Create: `hosts/nixos/` directory
- Move: `hosts/faust/` → `hosts/nixos/faust/`
- Move: `hosts/thelio/` → `hosts/nixos/thelio/`
- Move: `hosts/system76/` → `hosts/nixos/system76/`
- Move: `hosts/local.nix` → `hosts/nixos/local.nix`
- Move: `hosts/niximg.nix` → `hosts/nixos/niximg.nix`
- Modify: `flake.nix` — update `mapHosts ./hosts {}` to `mapHosts ./hosts/nixos {}`
- Modify: `hosts/nixos/faust/home.nix` — `../../modules/user` → `../../../modules/user`
- Modify: `hosts/nixos/thelio/home.nix` — `../../modules/user` → `../../../modules/user`
- Modify: `hosts/nixos/system76/home.nix` — `../../modules/user` → `../../../modules/user`

**Step 1: Move hosts into nixos/ subdirectory**

```bash
cd /etc/dotfiles
mkdir -p hosts/nixos
mv hosts/faust hosts/thelio hosts/system76 hosts/nixos/
mv hosts/local.nix hosts/niximg.nix hosts/nixos/
```

**Step 2: Update mapHosts path in flake.nix**

In `flake.nix`, find:
```nix
nixosConfigurations =
  mapHosts ./hosts {};
```
Replace with:
```nix
nixosConfigurations =
  mapHosts ./hosts/nixos {};
```

**Step 3: Update user module path in each home.nix**

In `hosts/nixos/faust/home.nix`, `hosts/nixos/thelio/home.nix`, and `hosts/nixos/system76/home.nix`, find:
```nix
(toString ../../modules/user)
```
Replace with:
```nix
(toString ../../../modules/user)
```

**Step 4: Verify NixOS builds still evaluate**

```bash
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```
Expected: resolves without errors (full build not required)

**Step 5: Commit**

```bash
git add -A
git commit -m "refactor: reorganize hosts/ into hosts/nixos/ for darwin separation"
```

---

## Task 2: Add nix-darwin input and generalize mkPkgs in flake.nix

**Files:**
- Modify: `flake.nix`

**Step 1: Add nix-darwin to inputs**

In `flake.nix`, in the `inputs` block after `nixpkgs-stable`, add:
```nix
nix-darwin = {
  url = "github:lnl7/nix-darwin";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Also add `nix-darwin` to the `outputs` destructure:
```nix
outputs = inputs @ {
  self,
  nixpkgs,
  nixpkgs-stable,
  nixpkgs-unstable,
  home-manager,
  nix-darwin,
  alejandra,
  nix-vscode-extensions,
  ...
}:
```

**Step 2: Replace hardcoded system with named constants and generalize mkPkgs**

Find the current `let` block:
```nix
    system = "x86_64-linux";

    mkPkgs = pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };

    pkgs = mkPkgs nixpkgs [
      self.overlay
      inputs.nix-vscode-extensions.overlays.default
    ];
    pkgs' = mkPkgs nixpkgs-unstable [];
    pkgs-stable' = mkPkgs nixpkgs-stable [];
```

Replace with:
```nix
    linuxSystem  = "x86_64-linux";
    darwinSystem = "aarch64-darwin";

    mkPkgs = system: pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };

    pkgs        = mkPkgs linuxSystem  nixpkgs [ self.overlay inputs.nix-vscode-extensions.overlays.default ];
    pkgs'       = mkPkgs linuxSystem  nixpkgs-unstable [];
    pkgs-stable' = mkPkgs linuxSystem nixpkgs-stable [];
    pkgs-darwin  = mkPkgs darwinSystem nixpkgs [ self.overlay inputs.nix-vscode-extensions.overlays.default ];
```

**Step 3: Fix overlay — it referenced the old `system` variable**

Find:
```nix
    overlay = final: prev: {
      unstable = pkgs';
      stable = pkgs-stable';
      my = self.packages."${system}";
    };
```
Replace with:
```nix
    overlay = final: prev: {
      unstable = pkgs';
      stable = pkgs-stable';
      my = self.packages."${linuxSystem}";
    };
```

**Step 4: Fix remaining `system` references in outputs**

Find:
```nix
    packages."${system}" =
      mapModules ./packages (p: pkgs.callPackage p {inherit inputs;});

    nixosModules =
      {dotfiles = import ./.;} // mapModulesRec ./modules/system import;

    nixosConfigurations =
      mapHosts ./hosts/nixos {};

    devShell."${system}" =
      import ./shell.nix {inherit pkgs;};

    defaultApp."${system}" = {
      type = "app";
      program = ./bin/hey;
    };
```
Replace with:
```nix
    packages."${linuxSystem}" =
      mapModules ./packages (p: pkgs.callPackage p {inherit inputs;});

    nixosModules =
      {dotfiles = import ./.;} // mapModulesRec ./modules/system import;

    nixosConfigurations =
      mapHosts ./hosts/nixos {};

    darwinConfigurations = {};

    devShell."${linuxSystem}" =
      import ./shell.nix {inherit pkgs;};

    defaultApp."${linuxSystem}" = {
      type = "app";
      program = ./bin/hey;
    };
```

**Step 5: Verify flake loads without errors**

```bash
nix flake show 2>&1 | head -30
```
Expected: outputs listed, no evaluation errors. `darwinConfigurations` shows as an empty attrset.

**Step 6: Verify NixOS still builds**

```bash
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```

**Step 7: Commit**

```bash
git add flake.nix flake.lock
git commit -m "feat: add nix-darwin input and generalize mkPkgs for multi-arch"
```

---

## Task 3: Create lib/darwin.nix

`lib/default.nix` uses `mapModules ./.` to auto-load every `.nix` file in `lib/` — this new file is picked up automatically with no changes to `lib/default.nix`.

**Files:**
- Create: `lib/darwin.nix`

**Step 1: Create lib/darwin.nix**

```nix
{
  inputs,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  sys = "aarch64-darwin";
in {
  mkDarwinHost = path: attrs @ {
    system ? sys,
    pkgs ? (throw "pkgs-darwin must be passed to mapDarwinHosts"),
    ...
  }: let
    specialArgs = {inherit lib inputs system home-manager;};
  in
    inputs.nix-darwin.lib.darwinSystem {
      inherit system;
      specialArgs = specialArgs;
      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        (filterAttrs (n: v: !elem n ["system" "pkgs"]) attrs)
        ../modules/darwin
        inputs.home-manager.darwinModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.jordan = import "${path}/home.nix";
            extraSpecialArgs = {inherit (specialArgs) inputs system;};
          };
        }
        (import path)
      ];
    };

  mapDarwinHosts = dir: attrs @ {system ? sys, ...}:
    mapModules dir (hostPath: mkDarwinHost hostPath attrs);
}
```

**Step 2: Update flake.nix to use mapDarwinHosts**

Find the placeholder:
```nix
    darwinConfigurations = {};
```
Replace with:
```nix
    darwinConfigurations =
      mapDarwinHosts ./hosts/darwin {pkgs = pkgs-darwin;};
```

Also update the `inherit` in the `lib.my` destructure at the top of `outputs`:
```nix
    inherit (lib.my) mapModules mapModulesRec mapHosts mapDarwinHosts;
```

**Step 3: Create the hosts/darwin/ directory so mapDarwinHosts doesn't fail on an empty/missing dir**

```bash
mkdir -p /etc/dotfiles/hosts/darwin
```

**Step 4: Verify flake loads**

```bash
nix flake show 2>&1 | head -30
```
Expected: No errors. `darwinConfigurations` is empty (no hosts yet).

**Step 5: Commit**

```bash
git add lib/darwin.nix flake.nix hosts/darwin/
git commit -m "feat: add lib/darwin.nix with mkDarwinHost and mapDarwinHosts"
```

---

## Task 4: Fix shared user modules for cross-platform

These modules are evaluated on EVERY build (NixOS and Darwin) since all user modules are loaded via `mapModulesRec'`. Platform-specific code must be gated.

**Files:**
- Modify: `modules/user/common.nix`
- Modify: `modules/user/workstation.nix`
- Modify: `modules/user/desktop/gtk.nix`
- Modify: `modules/user/desktop/hyprland/default.nix`

**Step 1: Fix modules/user/common.nix**

Find:
```nix
{ username, ... }:
let
  homeDirectory = "/home/${username}";
in {
  news.display = "show";

  targets.genericLinux.enable = true;
```
Replace with:
```nix
{ username, pkgs, ... }:
let
  homeDirectory =
    if pkgs.stdenv.isDarwin
    then "/Users/${username}"
    else "/home/${username}";
in {
  news.display = "show";

  targets.genericLinux.enable = pkgs.stdenv.isLinux;
```

**Step 2: Fix modules/user/workstation.nix**

The `dconf` and `gtk` desktop modules are Linux-only. Find:
```nix
  config = lib.mkIf cfg.enable {
    # Enable DConf for configuration of systems
    modules.desktop.dconf.enable = true;

    # Enable GTK
    modules.desktop.gtk.enable = config.modules.themes.gtk.enable;
```
Replace with:
```nix
  config = lib.mkIf cfg.enable {
    # Enable DConf for configuration of systems (Linux only)
    modules.desktop.dconf.enable = lib.mkIf pkgs.stdenv.isLinux true;

    # Enable GTK (Linux only)
    modules.desktop.gtk.enable = lib.mkIf pkgs.stdenv.isLinux config.modules.themes.gtk.enable;
```

Also add `pkgs` to the function arguments at the top of `workstation.nix`:
```nix
{ config, lib, pkgs, ... }:
```

**Step 3: Fix modules/user/desktop/gtk.nix**

The entire config block uses Linux-only packages (dconf-editor, Kvantum, Qt). Find:
```nix
  config = lib.mkIf cfg.enable {
```
Replace with:
```nix
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
```

**Step 4: Fix modules/user/desktop/hyprland/default.nix**

The config block references Wayland-specific packages (`swww`, `hyprpanel`, `playerctl`, `brightnessctl`, `pulseaudio`). Find:
```nix
  config = lib.mkIf cfg.enable {
```
Replace with:
```nix
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
```

**Step 5: Verify NixOS builds are unbroken**

```bash
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```
Expected: resolves without errors.

**Step 6: Commit**

```bash
git add modules/user/common.nix modules/user/workstation.nix \
        modules/user/desktop/gtk.nix modules/user/desktop/hyprland/default.nix
git commit -m "fix: gate Linux-only user modules behind pkgs.stdenv.isLinux"
```

---

## Task 5: Create nix-darwin system modules at modules/darwin/

These are imported explicitly by `lib/darwin.nix` (`../modules/darwin`). They are NOT under `modules/system/` and will NOT be auto-imported into NixOS builds.

**Files:**
- Create: `modules/darwin/default.nix`
- Create: `modules/darwin/homebrew.nix`
- Create: `modules/darwin/defaults.nix`

**Step 1: Create modules/darwin/default.nix**

```nix
# modules/darwin/default.nix
#
# Imported explicitly by lib/darwin.nix — NOT auto-loaded by the NixOS default.nix.
# This is the nix-darwin equivalent of the repo root default.nix.
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./homebrew.nix
    ./defaults.nix
  ];

  # Leave the Nix daemon entirely to Determinate Nix — do not conflict.
  # Do not set nix.package or nix.settings here.

  # Touch ID for sudo
  security.pam.enableSudoTouchIdAuth = true;

  environment.systemPackages = with pkgs; [
    git
    curl
    wget
    gnumake
    unzip
    vim
  ];

  programs.zsh.enable = true;

  # Required by nix-darwin — increment when nix-darwin breaks compatibility.
  system.stateVersion = 5;
}
```

**Step 2: Create modules/darwin/homebrew.nix**

```nix
# modules/darwin/homebrew.nix
#
# Declarative Homebrew management via nix-darwin.
# Homebrew itself must be pre-installed: https://brew.sh
# All casks and brews listed here are installed on `darwin-rebuild switch`.
# Apps NOT listed are removed when cleanup = "zap".
{...}: {
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      # "zap" removes casks not listed here on activation — set to "none" temporarily
      # when testing to avoid accidentally removing apps.
      cleanup = "zap";
    };
    casks = [
      # Add work-required GUI apps here, e.g.:
      # "zoom"
      # "slack"
      # "1password"
    ];
    brews = [
      # CLI tools not available in nixpkgs, e.g.:
      # "awscli"
    ];
  };
}
```

**Step 3: Create modules/darwin/defaults.nix**

```nix
# modules/darwin/defaults.nix
#
# macOS system preferences set declaratively.
# Run `darwin-rebuild switch` to apply. Some changes require logout/reboot.
# Full option list: https://daiderd.com/nix-darwin/manual/index.html#opt-system.defaults
{...}: {
  system.defaults = {
    NSGlobalDomain = {
      # Fastest key repeat
      KeyRepeat = 1;
      InitialKeyRepeatDelay = 10;
      # Expand save/print panels by default
      NSNavPanelExpandedStateForSaveMode = true;
      PMPrintingExpandedStateForPrint = true;
    };

    dock = {
      autohide = true;
      show-recents = false;
      mru-spaces = false;
    };

    finder = {
      AppleShowAllExtensions = true;
      FXPreferredViewStyle = "clmv"; # Column view
      ShowPathbar = true;
    };

    # Disable the quarantine dialog for downloaded apps
    LaunchServices.LSQuarantine = false;
  };
}
```

**Step 4: Commit**

```bash
git add modules/darwin/
git commit -m "feat: add nix-darwin system modules at modules/darwin/"
```

---

## Task 6: Create AeroSpace user module

AeroSpace is a tiling window manager for macOS. It lives in nixpkgs as `pkgs.aerospace`.

**Files:**
- Create: `modules/user/desktop/aerospace/default.nix`
- Create: `modules/user/desktop/aerospace/aerospace.toml`

**Step 1: Create modules/user/desktop/aerospace/default.nix**

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.aerospace;
in {
  options.modules.desktop.aerospace = mkOption {
    description = "AeroSpace tiling window manager for macOS";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "aerospace";
          }
        ];
      });
    default = {};
  };

  config = mkIf cfg.enable {
    home.packages = [pkgs.aerospace];

    xdg.configFile."aerospace/aerospace.toml".source = ./aerospace.toml;
  };
}
```

**Step 2: Create modules/user/desktop/aerospace/aerospace.toml**

```toml
# AeroSpace configuration
# Docs: https://nikitabobko.github.io/AeroSpace/guide

# Start at login
start-at-login = true

# Key mappings use the format: alt-key or ctrl-alt-key
# SUPER = alt (remap in skhd or use alt directly)

[mode.main.binding]
# Application launchers
alt-return = 'exec-and-forget open -n /Applications/kitty.app'

# Focus movement (vim-style)
alt-h = 'focus left'
alt-j = 'focus down'
alt-k = 'focus up'
alt-l = 'focus right'

# Move windows
alt-shift-h = 'move left'
alt-shift-j = 'move down'
alt-shift-k = 'move up'
alt-shift-l = 'move right'

# Resize
alt-ctrl-h = 'resize width -50'
alt-ctrl-l = 'resize width +50'
alt-ctrl-k = 'resize height -50'
alt-ctrl-j = 'resize height +50'

# Layouts
alt-slash = 'layout tiles horizontal vertical'
alt-comma = 'layout accordion horizontal vertical'

# Workspace switching
alt-1 = 'workspace 1'
alt-2 = 'workspace 2'
alt-3 = 'workspace 3'
alt-4 = 'workspace 4'
alt-5 = 'workspace 5'

# Move window to workspace
alt-shift-1 = 'move-node-to-workspace 1'
alt-shift-2 = 'move-node-to-workspace 2'
alt-shift-3 = 'move-node-to-workspace 3'
alt-shift-4 = 'move-node-to-workspace 4'
alt-shift-5 = 'move-node-to-workspace 5'

# Close window
alt-q = 'close'

# Fullscreen
alt-f = 'fullscreen'

[[on-window-detected]]
if.app-name-regex-substring = 'kitty'
run = 'move-node-to-workspace 1'

[[on-window-detected]]
if.app-name-regex-substring = 'Slack'
run = 'move-node-to-workspace 3'
```

**Step 3: Commit**

```bash
git add modules/user/desktop/aerospace/
git commit -m "feat: add AeroSpace window manager user module for macOS"
```

---

## Task 7: Create skhd user module

skhd is a hotkey daemon for macOS that handles global keybindings.

**Files:**
- Create: `modules/user/desktop/skhd/default.nix`
- Create: `modules/user/desktop/skhd/skhdrc`

**Step 1: Create modules/user/desktop/skhd/default.nix**

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.skhd;
in {
  options.modules.desktop.skhd = mkOption {
    description = "skhd hotkey daemon for macOS";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "skhd";
          }
        ];
      });
    default = {};
  };

  config = mkIf cfg.enable {
    home.packages = [pkgs.skhd];

    xdg.configFile."skhd/skhdrc".source = ./skhdrc;
  };
}
```

**Step 2: Create modules/user/desktop/skhd/skhdrc**

```bash
# skhdrc — skhd hotkey daemon configuration
# Docs: https://github.com/koekeishiya/skhd

# Application shortcuts
# alt - return : open -n /Applications/kitty.app

# Most keybindings are handled in AeroSpace's aerospace.toml.
# Use skhd for system-level bindings that AeroSpace can't handle,
# such as launching applications or controlling system features.

# Screenshot (mirrors Hyprland binding)
# fn - home : screencapture -i ~/Pictures/screenshot-$(date +%s).png
```

**Step 3: Commit**

```bash
git add modules/user/desktop/skhd/
git commit -m "feat: add skhd hotkey daemon user module for macOS"
```

---

## Task 8: Create the Darwin work host

**Files:**
- Create: `hosts/darwin/work/default.nix`
- Create: `hosts/darwin/work/home.nix`

**Step 1: Create hosts/darwin/work/default.nix**

```nix
# hosts/darwin/work/default.nix
#
# nix-darwin system configuration for the work MacBook.
# Activate with: darwin-rebuild switch --flake .#work
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  # User account — must match the macOS account name exactly
  users.users.jordan = {
    home = "/Users/jordan";
    shell = pkgs.zsh;
  };

  # Host-specific homebrew additions (supplements modules/darwin/homebrew.nix)
  homebrew.casks = [
    # Add work-specific apps here
  ];
}
```

**Step 2: Create hosts/darwin/work/home.nix**

```nix
# hosts/darwin/work/home.nix
#
# Home Manager configuration for the work MacBook.
{
  pkgs,
  inputs,
  config,
  lib,
  osConfig,
  system,
  ...
}:
with lib;
with lib.my; let
  username = "jordan";
in {
  imports =
    [inputs.catppuccin.homeModules.catppuccin]
    ++ (mapModulesRec'
      (toString ../../../modules/user)
      (path: import path {inherit pkgs inputs config lib username osConfig system;}));

  modules = {
    workstation.enable = true;

    desktop = {
      aerospace.enable = true;
      skhd.enable = true;
    };

    themes = {
      catppuccin.enable = true;
      gtk.enable = false;
    };

    development = {
      node.enable = true;
      golang.enable = true;
      rust.enable = true;
    };
  };
}
```

**Step 3: Verify the darwin host appears in flake outputs**

```bash
nix flake show 2>&1 | grep darwin
```
Expected: `darwinConfigurations.work` appears in output.

**Step 4: Commit**

```bash
git add hosts/darwin/work/
git commit -m "feat: add darwin work host configuration"
```

---

## Task 9: Verify Darwin config evaluates cleanly

This can be done on Linux via `nix eval` to check the attribute tree evaluates without errors, even though the actual system build requires an Apple Silicon Mac.

**Step 1: Evaluate the Darwin config**

```bash
nix eval .#darwinConfigurations.work.config.networking.hostName
```
Expected: `"work"`

**Step 2: Evaluate home-manager user config**

```bash
nix eval .#darwinConfigurations.work.config.home-manager.users.jordan.home.username
```
Expected: `"jordan"`

**Step 3: Verify no NixOS regressions**

```bash
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
nix build .#nixosConfigurations.thelio.config.system.build.toplevel --dry-run
```
Expected: both complete without errors.

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: complete macOS/Darwin support — nix-darwin + home-manager"
```

---

## First-Time Bootstrap on the Mac

Once on the actual Apple Silicon Mac:

```bash
# 1. Install Nix via Determinate (company-managed Mac safe)
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# 2. Install Homebrew (required by modules/darwin/homebrew.nix)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# 3. Clone dotfiles
git clone git@github.com:JordanFaust/dotfiles.git /etc/dotfiles

# 4. Bootstrap nix-darwin (first time — nix-darwin not yet on PATH)
nix run nix-darwin -- switch --flake /etc/dotfiles#work

# 5. Subsequent rebuilds
darwin-rebuild switch --flake /etc/dotfiles#work
```
