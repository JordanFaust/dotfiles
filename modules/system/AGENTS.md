# modules/system/AGENTS.md

NixOS system-level modules. Configured via `modules.*` options in each host's `default.nix`.

## CRITICAL: Auto-import behavior

`default.nix` (repo root) line 15:
```nix
mapModulesRec' (toString ./modules/system) import
```
**Every `.nix` file in this directory tree is evaluated for every NixOS host.**

Consequences:
- **Never add darwin-specific modules here.** nix-darwin options (`homebrew`, `system.defaults.*`) do not exist in NixOS — evaluation will fail.
- Darwin system modules belong in `modules/darwin/` (imported explicitly by `lib/darwin.nix`).
- Prefixing a file or directory name with `_` causes `mapModulesRec'` to skip it.

## Directory structure

```
modules/system/
├── options.nix            # Top-level option definitions (modules.user, modules.dotfiles, modules.home)
├── xdg.nix
├── security.nix
├── desktop/
│   ├── default.nix        # Desktop base (fonts, packages, portal)
│   ├── hyprland.nix       # Hyprland WM
│   ├── greeter.nix
│   └── vm/                # Virtualisation (qemu, virtualbox, lxd)
├── shell/                 # zsh, tmux, git, gnupg, direnv
├── services/              # ai, docker, ssh, dbus
├── hardware/              # audio, nvidia, amd, bluetooth, sensors, etc.
└── themes/catppuccin/
```

## Module convention

```nix
{ config, lib, pkgs, ... }:
with lib;
with lib.my; let
  cfg = config.modules.<area>.<name>;
in {
  options.modules.<area>.<name> = {
    enable = mkBoolOpt false;
    # additional options via mkOpt / mkOpt'
  };

  config = mkIf cfg.enable {
    # NixOS options here
  };
}
```

## Enabling modules in a host

In `hosts/nixos/<host>/default.nix`:
```nix
modules = {
  desktop.hyprland.enable = true;
  shell = { zsh.enable = true; git.enable = true; };
  hardware.audio.enable = true;
};
```

## Adding a new system module

1. Create `modules/system/<area>/<name>.nix` — auto-loaded, no import needed.
2. Define `options.modules.<area>.<name>.*` in the new file.
3. Enable in the relevant host's `default.nix`.
4. Verify: `nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run`
