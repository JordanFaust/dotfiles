# modules/user/AGENTS.md

Home Manager user modules. Loaded by every host's `home.nix` (both NixOS and Darwin) via `mapModulesRec'`. Configured via `modules.*` options in each host's `home.nix`.

## Auto-load behavior

Each `home.nix` contains:
```nix
mapModulesRec' (toString ../../../modules/user)   # hosts/darwin/work/home.nix (3 levels)
  (path: import path { inherit pkgs inputs config lib username osConfig system; })
```
Every `.nix` file in `modules/user/` (recursively) is imported into every host.
**No manual imports needed** — place a new `.nix` file in the directory and it's live.
Prefix with `_` to exclude a file from auto-loading.

## Function signature

Every user module receives:
```nix
{ pkgs, inputs, config, lib, username, osConfig, system, ... }
```

| Arg | Type | Notes |
|-----|------|-------|
| `pkgs` | pkgs | Platform-appropriate (`x86_64-linux` on NixOS, `aarch64-darwin` on macOS) |
| `inputs` | attrset | All flake inputs |
| `config` | attrset | Home Manager config (for cross-module references) |
| `lib` | attrset | nixpkgs lib + `lib.my.*` helpers |
| `username` | string | `"jordan"` |
| `osConfig` | attrset | The NixOS or nix-darwin system config (use for system→user bridging) |
| `system` | string | `"x86_64-linux"` or `"aarch64-darwin"` |

## Option pattern

All modules use `nullOr (submoduleWith {...})` with `default = {}` so the `enable` option defaults to `false`:

```nix
{ config, lib, pkgs, ... }:
with lib;
with lib.my; let
  cfg = config.modules.desktop.mymodule;
in {
  options.modules.desktop.mymodule = mkOption {
    description = "Short description";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
          options.enable = mkEnableOption "mymodule";
          # Add more sub-options here
        }];
      });
    default = {};   # <-- makes cfg.enable = false unless overridden
  };

  config = mkIf cfg.enable {
    home.packages = [ ... ];
  };
}
```

## Platform guards

Because every module is loaded on every platform, Linux-only code must be gated:

```nix
# Config block only applies on Linux
config = mkIf (cfg.enable && pkgs.stdenv.isLinux) { ... };

# Conditional packages in a list
home.packages = with pkgs; [
  someCrossPlatformTool
] ++ lib.optionals pkgs.stdenv.isLinux [
  linuxOnlyTool
];

# Conditional option values
someOption = lib.mkIf pkgs.stdenv.isLinux true;
```

**Known Linux-only areas that are already guarded:**
- `modules/user/desktop/hyprland/` — entire config block
- `modules/user/desktop/gtk.nix` — entire config block
- `modules/user/desktop/dconf.nix` — entire config block
- `modules/user/themes/catppuccin/` — KDE, Kvantum, cursor, GTK portions

**`common.nix` platform handling:**
```nix
homeDirectory = if pkgs.stdenv.isDarwin then "/Users/${username}" else "/home/${username}";
targets.genericLinux.enable = pkgs.stdenv.isLinux;
```

## Directory structure

```
modules/user/
├── options.nix            # modules.minimal, modules.themes.*
├── common.nix             # Base home-manager config (stateVersion, sessionVars, homeDirectory)
├── workstation.nix        # High-level toggle; enables gtk + dconf (Linux only)
├── desktop/
│   ├── hyprland/          # Hyprland WM + hypridle + hyprlock (Linux only)
│   ├── aerospace/         # AeroSpace WM (macOS, pkgs.stdenv.isDarwin guard)
│   ├── skhd/              # skhd hotkey daemon (macOS, pkgs.stdenv.isDarwin guard)
│   ├── gtk.nix            # GTK theming (Linux only)
│   └── dconf.nix          # dconf settings (Linux only)
├── development/           # node, python, rust, golang, java — all cross-platform
├── applications/          # browsers, neovim, vscode, kitty, streaming
└── themes/catppuccin/     # Catppuccin theme (Linux-only portions guarded)
```

## Enabling modules in a host

In `hosts/darwin/work/home.nix` or `hosts/nixos/<host>/home.nix`:
```nix
modules = {
  workstation.enable = true;
  desktop.aerospace.enable = true;   # macOS only
  desktop.hyprland.enable = true;    # NixOS only
  themes.catppuccin.enable = true;
  development.node.enable = true;
};
```

## Adding a new user module

1. Create `modules/user/<area>/<name>.nix` — auto-loaded, no import needed.
2. Use the option pattern above.
3. Add a platform guard if the module uses Linux-only packages or Home Manager options.
4. Enable in the relevant host's `home.nix`.
5. Verify: `nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run`
