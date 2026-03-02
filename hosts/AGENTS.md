# hosts/AGENTS.md

Per-machine configurations. Each subdirectory name becomes the flake output name.

## Structure

```
hosts/
├── nixos/                      # NixOS hosts → nixosConfigurations.<name>
│   ├── local.nix               # Shared: timezone, locale (imported by each host)
│   ├── niximg.nix              # Minimal install image
│   ├── faust/                  # Desktop — AMD, Hyprland
│   │   ├── default.nix         # System config — modules.* enable flags, networking
│   │   ├── home.nix            # Home Manager config — modules.* enable flags
│   │   └── hardware-configuration.nix
│   ├── thelio/                 # Desktop — AMD, Hyprland, AI
│   └── system76/               # Laptop — System76 hardware
└── darwin/                     # macOS hosts → darwinConfigurations.<name>
    └── work/                   # Apple Silicon MacBook
        ├── default.nix         # nix-darwin system config — users, host-specific casks
        ├── home.nix            # Home Manager config — modules.* enable flags
        └── README.md           # Bootstrap instructions
```

## Directory name = flake output name

`lib/nixos.nix` and `lib/darwin.nix` derive the hostname from the directory:
```nix
networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
```
`hosts/nixos/faust/` → `nixosConfigurations.faust` → activate with `nixos-rebuild switch --flake .#faust`
`hosts/darwin/work/` → `darwinConfigurations.work` → activate with `darwin-rebuild switch --flake .#work`

## NixOS host files

### `default.nix` — system configuration

```nix
{ config, lib, ... }: {
  imports = [
    ../local.nix               # shared timezone/locale
    ./hardware-configuration.nix
  ];

  modules = {
    desktop.hyprland.enable = true;
    shell = { zsh.enable = true; git.enable = true; };
    hardware.audio.enable = true;
    services.ssh.enable = true;
  };
}
```

### `home.nix` — Home Manager configuration

```nix
{ pkgs, inputs, config, lib, osConfig, system, ... }:
with lib;
with lib.my; let
  username = "jordan";
in {
  imports =
    [ inputs.catppuccin.homeModules.catppuccin ]
    ++ (mapModulesRec'
      (toString ../../../modules/user)         # THREE levels up from hosts/nixos/<host>/
      (path: import path { inherit pkgs inputs config lib username osConfig system; }));

  modules = {
    workstation.enable = true;
    desktop.hyprland.enable = true;
    themes.catppuccin.enable = true;
  };
}
```

**Path depth:** `hosts/nixos/<host>/home.nix` → `../../../modules/user` (3 `../` levels).

## Darwin host files

### `default.nix` — nix-darwin system configuration

```nix
{ pkgs, ... }: {
  users.users.jordan = {
    home = "/Users/jordan";    # must be /Users/, not /home/
    shell = pkgs.zsh;
  };

  homebrew.casks = [           # host-specific casks; merged with modules/darwin/homebrew.nix
    "zoom"
    "slack"
  ];

  modules = {
    services.docker.enable = true;
  };
}
```

### `home.nix` — Home Manager configuration

```nix
{ pkgs, inputs, config, lib, osConfig, system, ... }:
with lib;
with lib.my; let
  username = "jordan";
in {
  imports =
    [ inputs.catppuccin.homeModules.catppuccin ]
    ++ (mapModulesRec'
      (toString ../../../modules/user)         # THREE levels up from hosts/darwin/<host>/
      (path: import path { inherit pkgs inputs config lib username osConfig system; }));

  modules = {
    workstation.enable = true;
    desktop = { aerospace.enable = true; skhd.enable = true; };
    themes = { catppuccin.enable = true; gtk.enable = false; };  # no GTK on macOS
  };
}
```

**`home.stateVersion`** — required in Darwin home.nix; use current NixOS release string (e.g. `"24.11"`).

## Adding a new NixOS host

1. `mkdir -p hosts/nixos/<hostname>`
2. Generate hardware config: `nixos-generate-config --root /mnt --dir /etc/dotfiles/hosts/nixos/<hostname>`; delete `configuration.nix`.
3. Write `default.nix` and `home.nix` following the patterns above.
4. Verify: `nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel --dry-run`

## Adding a new Darwin host

1. `mkdir -p hosts/darwin/<hostname>`
2. Write `default.nix` and `home.nix` following the Darwin patterns above.
3. Verify: `nix eval .#darwinConfigurations.<hostname>.config.networking.hostName`
4. On the machine: `nix run nix-darwin -- switch --flake /etc/dotfiles#<hostname>`
