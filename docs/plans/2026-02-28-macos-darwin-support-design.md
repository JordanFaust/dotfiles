# macOS / Darwin Support Design

**Date:** 2026-02-28
**Branch:** nov-updates

## Context

This dotfiles repo manages NixOS configurations for multiple machines. A work Mac (Apple Silicon, company-managed MDM) is being added. The goal is to reuse the existing `modules/user/` Home Manager layer on macOS while adding macOS-specific alternatives for the desktop layer.

**Key decisions made:**
- Hardware: Apple Silicon (`aarch64-darwin`)
- Nix installation: Determinate Nix (avoids MDM conflicts with the Nix daemon)
- System layer: Full `nix-darwin` (works well alongside Determinate Nix)
- Homebrew: Managed declaratively via nix-darwin's `homebrew` module
- Status bar: Native macOS menu bar only (no SketchyBar)
- Window manager: AeroSpace (replaces Hyprland)
- Keybindings: skhd (replaces Hyprland keybindings)
- Host structure: Reorganize `hosts/` into `hosts/nixos/` and `hosts/darwin/`

---

## Repository Structure Changes

### Host Directory Reorganization

```
hosts/
  nixos/               ← renamed from hosts/ (all existing hosts move here)
    faust/
    thelio/
    system76/
    local.nix
    niximg.nix
  darwin/              ← new
    work/
      default.nix
      home.nix
```

**Cascading path change:** Every NixOS `home.nix` currently imports `../../modules/user`. After moving to `hosts/nixos/`, this becomes `../../../modules/user`.

### New Directories

```
modules/
  system/
    darwin/            ← new (nix-darwin system config)
      default.nix
      homebrew.nix
      defaults.nix
  user/
    desktop/
      aerospace/       ← new (replaces hyprland on macOS)
        default.nix
        config.toml
      skhd/            ← new (keybindings)
        default.nix
        skhdrc

lib/
  darwin.nix           ← new (auto-loaded by lib/default.nix via mapModules ./)
```

---

## `flake.nix` Changes

### 1. Add `nix-darwin` input

```nix
nix-darwin = {
  url = "github:lnl7/nix-darwin";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

### 2. Generalize `mkPkgs` to accept `system`

The current `mkPkgs` closes over the hardcoded `"x86_64-linux"`. This must become a parameter:

```nix
mkPkgs = system: pkgs: extraOverlays:
  import pkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = extraOverlays ++ (lib.attrValues self.overlays);
  };

linuxSystem  = "x86_64-linux";
darwinSystem = "aarch64-darwin";

pkgs         = mkPkgs linuxSystem  nixpkgs [ self.overlay inputs.nix-vscode-extensions.overlays.default ];
pkgs'        = mkPkgs linuxSystem  nixpkgs-unstable [];
pkgs-stable' = mkPkgs linuxSystem  nixpkgs-stable [];
pkgs-darwin  = mkPkgs darwinSystem nixpkgs [ self.overlay inputs.nix-vscode-extensions.overlays.default ];
```

### 3. Update host mappings

```nix
nixosConfigurations  = mapHosts        ./hosts/nixos  {};
darwinConfigurations = mapDarwinHosts  ./hosts/darwin { pkgs = pkgs-darwin; };
```

`pkgs-darwin` is passed explicitly through `attrs` because `lib/default.nix` only receives the Linux `pkgs` — Darwin pkgs are a different architecture and must be built separately.

---

## `lib/darwin.nix` (new file)

`lib/default.nix` uses `mapModules ./.` to auto-load all files in `lib/`, so this file is picked up with no changes to `lib/default.nix`.

Mirrors `lib/nixos.nix` with three key differences:
- Calls `inputs.nix-darwin.lib.darwinSystem` instead of `nixosSystem`
- Uses `home-manager.darwinModules.home-manager` instead of `nixosModules`
- Imports `../modules/system/darwin` instead of `../. # /default.nix`
- Accepts `pkgs` via `attrs` (different architecture)

```nix
{ inputs, lib, pkgs, home-manager, ... }:
with lib;
with lib.my; let
  sys = "aarch64-darwin";
in {
  mkDarwinHost = path: attrs @ { system ? sys, pkgs ? (throw "pkgs-darwin required"), ... }:
    let
      specialArgs = { inherit lib inputs system home-manager; };
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
          ../modules/system/darwin
          inputs.home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.jordan = import "${path}/home.nix";
              extraSpecialArgs = { inherit (specialArgs) inputs system; };
            };
          }
          (import path)
        ];
      };

  mapDarwinHosts = dir: attrs @ { system ? sys, ... }:
    mapModules dir (hostPath: mkDarwinHost hostPath attrs);
}
```

---

## Shared User Module Portability

These existing modules need platform guards. None are deleted — they become conditional.

### `modules/user/common.nix`

```nix
{ username, pkgs, ... }:
let
  homeDirectory =
    if pkgs.stdenv.isDarwin
    then "/Users/${username}"
    else "/home/${username}";
in {
  targets.genericLinux.enable = pkgs.stdenv.isLinux;
  # rest unchanged
}
```

### `modules/user/workstation.nix`

```nix
config = lib.mkIf cfg.enable {
  modules.desktop.dconf.enable = lib.mkIf pkgs.stdenv.isLinux true;
  modules.desktop.gtk.enable   = lib.mkIf pkgs.stdenv.isLinux config.modules.themes.gtk.enable;
};
```

### `modules/user/desktop/gtk.nix` and `dconf.nix`

Add `lib.mkIf pkgs.stdenv.isLinux` guards around their `config` blocks so they are inert on macOS even if accidentally included.

### `modules/user/desktop/hyprland/`

Add a `pkgs.stdenv.isLinux` guard on the `config` block to prevent evaluation errors from Linux-only nixpkgs attributes on macOS. Already guarded by `cfg.enable` defaulting to false, but this prevents accidental breakage.

---

## New macOS User Modules

### `modules/user/desktop/aerospace/default.nix`

Follows the same option/config pattern as the Hyprland module:

```nix
options.modules.desktop.aerospace.enable = mkEnableOption "AeroSpace window manager";

config = mkIf cfg.enable {
  home.packages = [ pkgs.aerospace ];
  xdg.configFile."aerospace/aerospace.toml".source = ./config.toml;
};
```

`config.toml` lives alongside with AeroSpace workspace layout configuration.

### `modules/user/desktop/skhd/default.nix`

```nix
options.modules.desktop.skhd.enable = mkEnableOption "skhd hotkey daemon";

config = mkIf cfg.enable {
  home.packages = [ pkgs.skhd ];
  xdg.configFile."skhd/skhdrc".source = ./skhdrc;
};
```

`skhdrc` lives alongside with keybind configuration.

**Note on hyprpanel:** It is a Hyprland extension pulled in as a flake input. With the Hyprland module guarded by `isLinux`, hyprpanel config never activates on macOS. The flake input stays harmless.

---

## nix-darwin System Modules

These are imported explicitly by `lib/darwin.nix`, not loaded via `mapModulesRec` like NixOS system modules.

### `modules/system/darwin/default.nix`

```nix
{ config, pkgs, lib, inputs, ... }: {
  imports = [ ./homebrew.nix ./defaults.nix ];

  # Leave Nix daemon to Determinate — do not conflict with it
  security.pam.enableSudoTouchIdAuth = true;
  environment.systemPackages = with pkgs; [ git curl wget ];
  programs.zsh.enable = true;
  system.stateVersion = 5;
}
```

### `modules/system/darwin/homebrew.nix`

```nix
homebrew = {
  enable = true;
  onActivation = { autoUpdate = true; cleanup = "zap"; };
  casks = [ /* work GUI apps: zoom, slack, 1password, etc. */ ];
  brews = [ /* anything not in nixpkgs */ ];
};
```

### `modules/system/darwin/defaults.nix`

`system.defaults.*` for keyboard repeat, dock, finder, etc. Starts as an empty skeleton filled in after the first activation.

---

## First Darwin Host

### `hosts/darwin/work/default.nix`

```nix
{ config, lib, pkgs, inputs, ... }: {
  modules.user = {
    name = "jordan";
    email = "jordan@<work>.com";
  };
  # host-specific overrides: homebrew casks, system.defaults, etc.
}
```

### `hosts/darwin/work/home.nix`

```nix
{ pkgs, inputs, config, lib, osConfig, ... }:
let username = "jordan"; in {
  imports = (mapModulesRec' (toString ../../../modules/user)
    (path: import path { inherit pkgs inputs config lib username osConfig; }));

  modules = {
    workstation.enable = true;
    desktop = {
      aerospace.enable = true;
      skhd.enable = true;
    };
    themes.catppuccin.enable = true;
    development = {
      node.enable = true;
      golang.enable = true;
      rust.enable = true;
    };
  };
}
```

---

## Activation Flow (new Mac)

```bash
# 1. Install Nix via Determinate
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# 2. Bootstrap nix-darwin (first time only — nix-darwin not yet installed)
nix run nix-darwin -- switch --flake .#work

# 3. Subsequent updates
darwin-rebuild switch --flake .#work
```

---

## What Carries Over From NixOS Without Changes

The following `modules/user/` modules work on macOS with zero modifications:
- All development tools (`node`, `golang`, `rust`, `python`, `java`)
- `applications/neovim.nix`
- `applications/vscode.nix`
- `applications/kitty.nix`
- `applications/browsers.nix`
- `themes/catppuccin/`
- `shell/` (zsh, tmux, git, gnupg, direnv)

This is the primary payoff of the existing user/system separation.
