# AGENTS.md — JordanFaust/dotfiles

Declarative NixOS + macOS (nix-darwin) configuration using flakes and Home Manager.
Dotfiles live at `/etc/dotfiles` (symlinked from `~/github.com/JordanFaust/dotfiles`).

## Discovery

**Use when** asked to find, understand, or change anything. Do not run codebase search first. Use the **Layout map** → pick the area → read that area's **AGENTS.md** → answer from it or do 0–1 targeted file reads. Area AGENTS.md files are the authoritative source for conventions and constraints.

| Task | Start here |
|------|-----------|
| Add/change a NixOS system module | `modules/system/AGENTS.md` |
| Add/change a user/home-manager module | `modules/user/AGENTS.md` |
| Add/change a macOS system module | `modules/darwin/AGENTS.md` |
| Add a new host (NixOS or macOS) | `hosts/AGENTS.md` |
| Add a lib helper | `lib/AGENTS.md` |
| Change flake inputs or outputs | Read `flake.nix` directly |

## Layout map

```
/etc/dotfiles/
├── flake.nix              # Flake entry — inputs, mkPkgs, nixosConfigurations, darwinConfigurations
├── default.nix            # NixOS root — auto-imports ALL of modules/system/ (see CRITICAL below)
├── lib/                   # Nix helpers — auto-loaded; see lib/AGENTS.md
├── modules/
│   ├── system/            # NixOS system modules — auto-imported into every NixOS build
│   ├── user/              # Home Manager user modules — loaded by every host's home.nix
│   └── darwin/            # nix-darwin system modules — imported explicitly by lib/darwin.nix only
├── hosts/
│   ├── nixos/             # NixOS hosts (faust, thelio, system76) — see hosts/AGENTS.md
│   └── darwin/            # macOS hosts (work) — see hosts/AGENTS.md
├── packages/              # Custom packages (callPackage'd for x86_64-linux only)
└── overlays/              # Nix overlays
```

## CRITICAL architectural constraints

### 1. `modules/system/` is auto-imported into every NixOS build

`default.nix` line 15:
```nix
mapModulesRec' (toString ./modules/system) import
```
Every `.nix` file under `modules/system/` (recursively) is evaluated for **every** NixOS host.
**Darwin-specific system modules must go in `modules/darwin/`**, not `modules/system/darwin/`.
If a nix-darwin option (`homebrew`, `system.defaults.*`) ends up in `modules/system/`, NixOS builds break.

### 2. `lib/` files are auto-loaded

`lib/default.nix` line 20:
```nix
mapModules ./. (file: import file {inherit self lib pkgs inputs home-manager;})
```
Every `.nix` file in `lib/` (non-recursively, non-`default.nix`) is loaded automatically.
Adding `lib/darwin.nix` requires **no changes to `lib/default.nix`**.

### 3. Host directory name = flake output name

`lib/nixos.nix` and `lib/darwin.nix` derive hostname from the directory:
```nix
networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
```
`hosts/nixos/faust/` → `nixosConfigurations.faust`
`hosts/darwin/work/` → `darwinConfigurations.work`

### 4. Darwin system modules must not conflict with Determinate Nix

`modules/darwin/default.nix` sets `nix.enable = false` to disable nix-darwin's own
Nix management entirely. Determinate Nix manages the daemon. Do **not** set
`nix.package` or `nix.settings` — doing so causes "Determinate detected, aborting
activation" on switch.

### 5. nix-darwin API (current)

| Intent | Option |
|--------|--------|
| Touch ID for sudo | `security.pam.services.sudo_local.touchIdAuth = true` |
| macOS system defaults | `system.defaults.*` |
| Homebrew declarative | `homebrew.enable = true` (see `modules/darwin/AGENTS.md`) |

The old `security.pam.enableSudoTouchIdAuth` no longer exists in current nix-darwin.

## Multi-system setup

```nix
linuxSystem  = "x86_64-linux";
darwinSystem = "aarch64-darwin";   # Apple Silicon
```

`pkgs-darwin` is built separately and passed explicitly to `mapDarwinHosts`:
```nix
darwinConfigurations = mapDarwinHosts ./hosts/darwin { pkgs = pkgs-darwin; };
```

The overlay guards Linux-only packages:
```nix
my = if final.stdenv.hostPlatform.system == linuxSystem
     then self.packages."${linuxSystem}"
     else {};
```

## Verification commands

Always run these after changes to confirm nothing broke:

```bash
# Verify NixOS builds still evaluate (fast — no actual build)
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run

# Verify Darwin config evaluates
nix eval .#darwinConfigurations.work.config.networking.hostName
nix eval .#darwinConfigurations.work.config.home-manager.users."jordan.faust".home.homeDirectory

# Check flake structure loads
nix flake show

# Full flake check (slower)
nix flake check
```

## Activation

| Platform | First time | Subsequent |
|----------|-----------|-----------|
| NixOS | `nixos-install --flake .#<host>` | `nixos-rebuild switch --flake .#<host>` or `hey rebuild` |
| macOS | `sudo nix run nix-darwin -- switch --flake .#work` | `sudo darwin-rebuild switch --flake /etc/dotfiles#work` |
