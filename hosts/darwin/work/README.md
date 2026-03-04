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

### 4. Private flake inputs (e.g. `private-fonts`)

The flake pulls a private repo over `git+ssh`. The **Nix daemon** (not your `sudo` session) performs the fetch, so it never sees your SSH agent and root's home is read-only on macOS — copying keys into `/var/root` is not an option.

**Workaround:** fetch the input as your user so the store and lock file are populated; then the daemon can use the existing store paths when you run `darwin-rebuild`.

**Before the first bootstrap (and whenever `private-fonts` or the lock file changes), run as your user** (no sudo):

```sh
cd /etc/dotfiles
ssh-add -l || ssh-add   # ensure your GitHub key is in the agent
nix flake update private-fonts
```

Then run the bootstrap (step 5) as usual. For subsequent rebuilds, run `nix flake update private-fonts` as your user first if you've changed the flake or lock file; otherwise `sudo darwin-rebuild switch` alone is enough.

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

If you changed the flake or lock file (or get "Permission denied" when fetching `private-fonts`), update the private input as your user first:

```sh
cd /etc/dotfiles
nix flake update private-fonts   # uses your SSH; no sudo
```

Then run the switch (as root):

```sh
sudo darwin-rebuild switch --flake /etc/dotfiles#work
```

If nothing changed in the flake inputs, `sudo darwin-rebuild switch` alone is enough.

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

- **“Git tree has uncommitted changes”:** Normal when you have local edits. Nix still uses your working tree; you can ignore the warning or commit before switching for a clean evaluation.
- **Nix daemon:** Managed by Determinate Nix — do not set `nix.package` or `nix.settings` in nix-darwin config, it will conflict.
- **Private flake inputs (SSH):** The Nix daemon does the fetch and has no access to your SSH agent. Run `nix flake update private-fonts` as your user (no sudo) so the store and lock file are populated; then `sudo darwin-rebuild switch` will use the existing store paths.
- **Homebrew cleanup:** `onActivation.cleanup = "zap"` removes casks not listed in config. Set to `"none"` temporarily if you need to test without removing apps.
- **Touch ID sudo:** Enabled via `security.pam.services.sudo_local.touchIdAuth = true`. This survives macOS updates (unlike the manual `/etc/pam.d/sudo` edit).
