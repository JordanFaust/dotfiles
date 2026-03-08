# Cursor Extensions

Extensions are managed manually via Cursor's marketplace, not through Nix.

## Why not Nix-managed?

Home Manager's `mutableExtensionsDir = true` creates symlinks without version
suffixes (e.g. `asvetliakov.vscode-neovim`). Cursor's headless extension scanner
only discovers directories with version suffixes (e.g. `bbenoist.nix-1.0.1`),
so Nix-only extensions become invisible after each rebuild. Until this friction
is resolved upstream, extensions are installed via `cursor --install-extension`.

## Required Extensions

Install after a fresh setup:

```bash
cursor --install-extension asvetliakov.vscode-neovim
cursor --install-extension bbenoist.nix
cursor --install-extension golang.go
cursor --install-extension sumneko.lua
cursor --install-extension ms-azuretools.vscode-docker
cursor --install-extension catppuccin.catppuccin-vsc-icons
cursor --install-extension guichina.code-telescope
```

## Optional Extensions

Installed per-project as needed:

```bash
cursor --install-extension ms-azuretools.vscode-containers
cursor --install-extension hbenl.vscode-test-explorer
cursor --install-extension ms-vscode.test-adapter-converter
cursor --install-extension ms-vscode.makefile-tools
```

## Theme

The Catppuccin theme extension (`catppuccin.catppuccin-vsc`) is managed by the
`catppuccin` Home Manager module — not listed here because that integration
works reliably.
