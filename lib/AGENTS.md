# lib/AGENTS.md

Nix helper library. All files here are **auto-loaded** by `lib/default.nix` via `mapModules ./.` — adding a new `.nix` file here requires no changes to `default.nix`.

## Auto-load signature

Every lib file receives these arguments:
```nix
{ self, lib, pkgs, inputs, home-manager }
```
`self` is the extensible lib attrset itself (for cross-referencing helpers).
`pkgs` is `x86_64-linux` nixpkgs — do not use it for darwin-specific builds.

## Available helpers (from `lib.my.*`)

### Module traversal

| Helper | Behavior |
|--------|----------|
| `mapModules dir fn` | Maps `fn` over immediate children of `dir` (directories with `default.nix`, `.nix` files except `default.nix`). Skips names prefixed with `_`. |
| `mapModules' dir fn` | Same as `mapModules` but returns a list (attrValues). |
| `mapModulesRec dir fn` | Like `mapModules` but recurses into subdirectories. Returns nested attrset. |
| `mapModulesRec' dir fn` | Recursively collects all `.nix` paths (flat list). Used by `home.nix` and `default.nix` to import all modules. |
| `mapHosts dir attrs` | `mapModules dir (hostPath: mkHost hostPath attrs)` — builds NixOS configs. |
| `mapDarwinHosts dir attrs` | `mapModules dir (hostPath: mkDarwinHost hostPath attrs)` — builds Darwin configs. |

### Option helpers (`lib/options.nix`)

```nix
mkOpt  type default        # mkOption with type and default
mkOpt' type default desc   # mkOption with description
mkBoolOpt default          # mkOption type bool
```

### Attribute helpers (`lib/attrs.nix`)

`mapFilterAttrs`, `attrsToList`, `genAttrs'`, `anyAttrs`, `countAttrs`

## Adding a new lib file

1. Create `lib/<name>.nix` — it is auto-loaded.
2. Function signature: `{ self, lib, pkgs, inputs, home-manager, ... }: { ... }`
3. Return an attrset of functions.
4. Access other lib helpers via `lib.my.<helper>` or `with lib.my;`.
