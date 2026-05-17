# Ghostty Module Design

**Date:** 2026-05-17  
**Scope:** Add `modules/user/applications/ghostty.nix` as a new Home Manager module alongside the existing `kitty.nix`. Both coexist during a transition period; kitty remains available as fallback.

---

## Goals

- Ghostty configured declaratively via `programs.ghostty` (Home Manager)
- Catppuccin Macchiato theme applied via the existing `catppuccin` HM module
- MonoLisa Variable font with Nerd Font Mono fallback via existing fontconfig chain
- Core kitty settings translated to Ghostty equivalents
- `kitty.nix` untouched — both modules enabled simultaneously until full switch

---

## Module Location

```
modules/user/applications/ghostty.nix   ← new file
```

Auto-loaded by Home Manager's `mapModulesRec'` — no import changes needed.

---

## Option Pattern

Follows the repo convention (`nullOr submoduleWith`, `default = { enable = true }`):

```nix
options.modules.applications.ghostty = mkOption {
  type = nullOr (submoduleWith {
    modules = [{ options.enable = mkEnableOption "ghostty"; }];
  });
  default = { enable = true; };
};
```

The `default = { enable = true }` means it activates on all hosts without explicit opt-in, matching `kitty.nix` behavior. Hosts can set `modules.applications.ghostty.enable = false` to suppress it.

---

## Package Install

```nix
home.packages = if pkgs.stdenv.isLinux
  then [ desktop pkgs.ghostty ]
  else [ pkgs.ghostty ];
```

Linux gets a uwsm `.desktop` wrapper (same pattern as `kitty.nix`) to avoid duplicate entries in rofi. macOS installs the package directly.

---

## Session Variables

```nix
home.sessionVariables = {
  TERMINAL = "ghostty";
};
```

`TERM` is not set globally — Ghostty sets it when spawning a shell. Ghostty ships its own terminfo (`xterm-ghostty`); no `TERMINFO_DIRS` export needed as Ghostty bundles it.

---

## `programs.ghostty` Settings

All settings expressed as Nix attributes under `programs.ghostty.settings`:

| Setting | Value | Source |
|---|---|---|
| `font-family` | `"MonoLisa Variable"` | kitty `font_family MonoLisa Variable Medium` |
| `font-size` | `18` | kitty `font_size 18` |
| `adjust-cell-height` | `"50%"` | kitty `adjust_line_height 150%` (Ghostty counts delta above 100%) |
| `cursor-style` | `"block"` | kitty `cursor_shape block` |
| `shell-integration` | `"detect"` | kitty `shell_integration enabled` |
| `background-opacity` | `1.0` | kitty `background_opacity 1.0` |
| `macos-option-as-alt` | `true` | kitty `macos_option_as_alt yes` (macOS only) |
| `macos-titlebar-style` | `"hidden"` | kitty `hide_window_decorations titlebar-only` (macOS only) |
| `copy-on-select` | `false` | kitty `copy_on_select no` |
| `term` | `"xterm-ghostty"` | replaces `xterm-kitty` |

macOS-only settings are gated: `lib.mkIf pkgs.stdenv.isDarwin { ... }`.

---

## Font Fallback (Nerd Font / nonicons)

No `symbol_map` needed. Ghostty is fontconfig-aware on both platforms and picks up the existing fallback chain declared in `catppuccin.nix`:

```
MonoLisa Variable → Symbols Nerd Font Mono → Symbols Nerd Font
```

This fontconfig rule lives at `~/.config/fontconfig/conf.d/99-nerd-font-fallback.conf` and is already managed by `catppuccin.nix`. No changes required.

`nonicons` remains Linux-only (`my.nonicons` in `catppuccin.nix`) — no change.

---

## Catppuccin Theme

The catppuccin HM module (`inputs.catppuccin.homeModules.catppuccin`) has a `catppuccin.ghostty` sub-option that:

1. Drops `~/.config/ghostty/themes/catppuccin-macchiato` from the catppuccin sources
2. Sets `programs.ghostty.settings.theme = "light:catppuccin-macchiato,dark:catppuccin-macchiato"`

This is wired in `catppuccin.nix` alongside the existing kitty block. `catppuccin.ghostty.enable` is set to `true` unconditionally within the `mkIf cfg.enable` block that already gates the whole catppuccin module. Since `programs.ghostty.enable` must be `true` for the catppuccin ghostty hook to activate (enforced by the catppuccin HM module internally), the ghostty module sets `programs.ghostty.enable = true` and the catppuccin module sets `catppuccin.ghostty.enable = true`:

```nix
# in ghostty.nix
programs.ghostty.enable = true;

# in catppuccin.nix (inside the existing mkIf cfg.enable block)
catppuccin.ghostty.enable =
  config.modules.applications.ghostty != null &&
  config.modules.applications.ghostty.enable;
```

The flavor is inherited from the existing `catppuccin.flavor = "macchiato"` set globally in `catppuccin.nix`.

---

## Files Changed

| File | Change |
|---|---|
| `modules/user/applications/ghostty.nix` | New file — full module |
| `modules/user/themes/catppuccin/catppuccin.nix` | Add `catppuccin.ghostty` block alongside kitty block |

No changes to:
- `kitty.nix` — untouched
- `hosts/darwin/work/home.nix` — no opt-in needed (default enables it)
- `flake.nix` — no new inputs

---

## Verification

```bash
# Darwin config evaluates without error
nix eval .#darwinConfigurations.work.config.home-manager.users."jordan.faust".programs.ghostty.settings

# NixOS dry-run still passes
nix build .#nixosConfigurations.faust.config.system.build.toplevel --dry-run
```
