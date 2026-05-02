{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.cmux;

  # ─────────────────────────────────────────────────────────────────────────
  # cmux settings.json
  #
  # Keybinding philosophy:
  #   All intra-cmux navigation uses a ctrl+k chord prefix, matching the
  #   ctrl+k leader established in Cursor/VSCode keybindings.nix.  Chords
  #   are intercepted at the app level before reaching the terminal, so they
  #   work regardless of what neovim mode is active.
  #
  # AeroSpace conflicts avoided:
  #   AeroSpace owns cmd+1-5 (workspace switch) and cmd+shift/ctrl+h/j/k/l
  #   (window move/resize).  cmux defaults of cmd+1-9 (workspace selection)
  #   would shadow those, so they are remapped to ctrl+k 1-9.
  # ─────────────────────────────────────────────────────────────────────────
  settings = {
    shortcuts.bindings = {
      # ── Pane focus ────────────────────────────────────────────────────────
      # Mirrors ctrl+k h/j/k/l from Cursor:
      #   ctrl+k h  ←  focusPaneLeft   (was opt+cmd+left)
      #   ctrl+k l  →  focusPaneRight  (was opt+cmd+right)
      #   ctrl+k k  ↑  focusPaneUp     (was opt+cmd+up)
      #   ctrl+k j  ↓  focusPaneDown   (was opt+cmd+down)
      focusPaneLeft = ["ctrl+k" "h"];
      focusPaneRight = ["ctrl+k" "l"];
      focusPaneUp = ["ctrl+k" "k"];
      focusPaneDown = ["ctrl+k" "j"];

      # ── Pane zoom / browser split ─────────────────────────────────────────
      # ctrl+k f  — toggle pane zoom   (mirrors workbench.action.toggleMaximizedPanel)
      # ctrl+k a  — split browser right (mirrors composer.openAsPane / Ctrl+K A)
      togglePaneZoom = ["ctrl+k" "f"];
      splitBrowserRight = ["ctrl+k" "a"];

      # ── Workspace selection ────────────────────────────────────────────────
      # Remapped from cmd+1-9 → ctrl+k 1-9.
      # AeroSpace owns cmd+1-5 for its own workspace switching; leaving cmd+1-9
      # in place would cause AeroSpace to intercept before cmux sees the event.
      selectWorkspace1 = ["ctrl+k" "1"];
      selectWorkspace2 = ["ctrl+k" "2"];
      selectWorkspace3 = ["ctrl+k" "3"];
      selectWorkspace4 = ["ctrl+k" "4"];
      selectWorkspace5 = ["ctrl+k" "5"];
      selectWorkspace6 = ["ctrl+k" "6"];
      selectWorkspace7 = ["ctrl+k" "7"];
      selectWorkspace8 = ["ctrl+k" "8"];
      selectWorkspace9 = ["ctrl+k" "9"];

      # ── Surface (tab) selection ────────────────────────────────────────────
      # ctrl+1-9 is unused by AeroSpace and skhd; safe to bind here.
      # Matches the ctrl+1-9 surface-selection convention from VSCode.
      selectSurface1 = "ctrl+1";
      selectSurface2 = "ctrl+2";
      selectSurface3 = "ctrl+3";
      selectSurface4 = "ctrl+4";
      selectSurface5 = "ctrl+5";
      selectSurface6 = "ctrl+6";
      selectSurface7 = "ctrl+7";
      selectSurface8 = "ctrl+8";
      selectSurface9 = "ctrl+9";
    };
  };
in {
  options.modules.applications.cmux = mkOption {
    description = "cmux — macOS terminal for AI coding agent sessions (homebrew cask).";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "cmux";
          }
        ];
      });
    default = {};
  };

  config = mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
    # cmux reads settings from ~/.config/cmux/settings.json (takes precedence
    # over ~/Library/Application Support/com.cmuxterm.app/settings.json).
    xdg.configFile."cmux/settings.json".text = builtins.toJSON settings;
  };
}
