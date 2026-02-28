# work — Apple Silicon MacBook (Darwin)

Work machine running macOS on Apple Silicon (`aarch64-darwin`).

## First-time setup

### 1. Install Determinate Nix

Safe on company-managed Macs — uses a separate APFS volume, doesn't conflict with MDM.

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

Restart your shell after this completes.

### 2. Install Homebrew

Required for GUI casks managed declaratively by nix-darwin.

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Follow the post-install instructions to add Homebrew to your PATH (usually adding a line to `~/.zprofile`).

### 3. Clone dotfiles

```sh
sudo mkdir -p /etc/dotfiles
sudo chown $(whoami) /etc/dotfiles
git clone https://github.com/JordanFaust/dotfiles /etc/dotfiles
```

### 4. Bootstrap nix-darwin

First-time only — `darwin-rebuild` doesn't exist yet, so use `nix run`:

```sh
cd /etc/dotfiles
nix run nix-darwin -- switch --flake .#work
```

This will:
- Install nix-darwin and put `darwin-rebuild` on your PATH
- Apply macOS system defaults (key repeat, dock, finder preferences)
- Enable Touch ID for `sudo`
- Install Homebrew casks listed in `default.nix`
- Activate your Home Manager config (AeroSpace, skhd, neovim, zsh, etc.)

### 5. Restart

Some macOS defaults (keyboard repeat speed, dock changes) require a logout or reboot to take full effect.

## Subsequent rebuilds

```sh
darwin-rebuild switch --flake /etc/dotfiles#work
```

## Customization

| File | Purpose |
|------|---------|
| `default.nix` | System config, Homebrew casks, user account |
| `home.nix` | Home Manager modules — enable/disable features |
| `../../modules/darwin/homebrew.nix` | Shared Homebrew config for all Darwin hosts |
| `../../modules/darwin/defaults.nix` | Shared macOS system defaults |
| `../../modules/user/desktop/aerospace/aerospace.toml` | AeroSpace window manager keybindings |
| `../../modules/user/desktop/skhd/skhdrc` | skhd global hotkeys |

### Adding work GUI apps (Homebrew casks)

In `default.nix`:
```nix
homebrew.casks = [
  "zoom"
  "slack"
  "1password"
];
```

### AeroSpace keybindings

| Key | Action |
|-----|--------|
| `alt-h/j/k/l` | Focus left/down/up/right |
| `alt-shift-h/j/k/l` | Move window left/down/up/right |
| `alt-1` through `alt-5` | Switch workspace |
| `alt-shift-1` through `alt-shift-5` | Move window to workspace |
| `alt-f` | Fullscreen |
| `alt-q` | Close window |
| `alt-/` | Toggle tile layout |
| `alt-shift-;` | Enter service mode |

## Notes

- **Nix daemon:** Managed by Determinate Nix — do not set `nix.package` or `nix.settings` in nix-darwin config, it will conflict.
- **Homebrew cleanup:** `onActivation.cleanup = "zap"` removes casks not listed in config. Set to `"none"` temporarily if you need to test without removing apps.
- **Touch ID sudo:** Enabled via `security.pam.services.sudo_local.touchIdAuth = true`. This survives macOS updates (unlike the manual `/etc/pam.d/sudo` edit).
