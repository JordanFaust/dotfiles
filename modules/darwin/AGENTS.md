# modules/darwin/AGENTS.md

nix-darwin system modules for macOS hosts. **NOT under `modules/system/`** — that directory is auto-imported into every NixOS build. These files are only evaluated when `lib/darwin.nix` builds a Darwin host.

## Load behavior

`lib/darwin.nix` imports this directory explicitly:
```nix
../modules/darwin   # relative to lib/darwin.nix → /etc/dotfiles/modules/darwin/
```
Only Darwin hosts evaluate this. NixOS builds never see it.

## Directory structure

```
modules/darwin/
├── default.nix        # Entry point — imports all below, sets stateVersion
├── homebrew.nix       # Declarative Homebrew (shared across all Darwin hosts)
├── defaults.nix       # macOS system preferences (dock, finder, keyboard, quarantine)
└── services/
    └── docker.nix     # Docker via Colima (installs colima + docker CLI tools)
```

## Conventions

- Do **not** set `nix.package` or `nix.settings` — Determinate Nix manages the daemon.
- System packages go in `default.nix` under `environment.systemPackages`.
- Host-specific casks belong in `hosts/darwin/<host>/default.nix` as `homebrew.casks = [...]` — merged with the shared list here by nix-darwin's module system.

## `system.stateVersion` — nix-darwin vs NixOS

nix-darwin uses **integers** (current range: 1–6), NOT year-based strings like NixOS:

```nix
# nix-darwin — integer
system.stateVersion = 5;

# NixOS — string (completely different system)
system.stateVersion = "21.05";
```

Only increment when a nix-darwin release explicitly instructs you to (breaking migration). Keep it at the value set when the machine was first bootstrapped.

## Key nix-darwin options (current API)

```nix
# Touch ID for sudo
# NOTE: security.pam.enableSudoTouchIdAuth no longer exists in current nix-darwin.
security.pam.services.sudo_local.touchIdAuth = true;

# macOS system defaults
system.defaults.NSGlobalDomain.KeyRepeat = 1;           # NOT InitialKeyRepeatDelay
system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;   # NOT InitialKeyRepeatDelay
system.defaults.dock.autohide = true;
system.defaults.finder.AppleShowAllExtensions = true;
system.defaults.LaunchServices.LSQuarantine = false;

# Homebrew
homebrew.enable = true;
homebrew.onActivation.cleanup = "zap";  # removes unlisted casks on activation
homebrew.casks = [ "zoom" "slack" ];
homebrew.brews = [ ];
```

Full option reference: https://daiderd.com/nix-darwin/manual/index.html

## Adding a new shared Darwin system module

1. Create `modules/darwin/<name>.nix`.
2. Add it to the `imports` list in `modules/darwin/default.nix`.
3. Verify: `nix eval .#darwinConfigurations.work.config.<option>`

## Adding a host-specific system option

Override or extend in `hosts/darwin/<host>/default.nix`. nix-darwin's module system merges list options (like `homebrew.casks`) automatically — host-specific casks stack on top of the shared list in `homebrew.nix`.
