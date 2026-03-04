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

### 4. Configure SSH for root (required for private flake inputs)

nix-darwin's system activation runs as `root`, so Nix fetches private SSH flake
inputs (like `private-fonts`) using root's SSH config. On a fresh Mac, root has
no SSH keys or known hosts configured.

```sh
sudo mkdir -p /root/.ssh && sudo chmod 700 /root/.ssh

# Trust GitHub's host key as root
sudo ssh-keyscan -H github.com | sudo tee /root/.ssh/known_hosts
sudo chmod 600 /root/.ssh/known_hosts

# Use your existing SSH key (adjust filename if yours differs)
printf "Host github.com\n  User git\n  IdentityFile /Users/jordan.faust/.ssh/id_ed25519\n  StrictHostKeyChecking accept-new\n" | sudo tee /root/.ssh/config
sudo chmod 600 /root/.ssh/config
```

### 5. Bootstrap nix-darwin

First-time only — `darwin-rebuild` doesn't exist yet, so use `nix run`.
System activation requires `sudo`:

```sh
cd /etc/dotfiles
sudo nix run nix-darwin -- switch --flake .#work
```

This will:
- Install nix-darwin and put `darwin-rebuild` on your PATH
- Apply macOS system defaults (key repeat, dock, finder preferences)
- Enable Touch ID for `sudo`
- Install Homebrew casks listed in `default.nix`
- Activate your Home Manager config (AeroSpace, skhd, neovim, zsh, etc.)

### 6. Restart

Some macOS defaults (keyboard repeat speed, dock changes) require a logout or reboot to take full effect.

## Subsequent rebuilds

```sh
sudo darwin-rebuild switch --flake /etc/dotfiles#work
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
