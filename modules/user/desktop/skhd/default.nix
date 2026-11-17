{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.skhd;

  # Native window commands (focus/move/tile with gaps) — see wm.js. Uses only
  # Apple APIs so it works on machines where third-party WMs are blocked.
  wm = pkgs.writeShellScriptBin "wm" ''
    exec /usr/bin/osascript -l JavaScript ${./wm.js} "$@"
  '';
  w = "${wm}/bin/wm";
in {
  options.modules.desktop.skhd = mkOption {
    description = "skhd hotkey daemon for macOS";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "skhd";
            options.windowManagement = mkEnableOption ''
              AeroSpace-style window hotkeys (cmd-h/j/k/l focus, cmd-shift-h/j/k/l
              move, cmd-/ tile) implemented with native macOS APIs via wm.js
            '';
          }
        ];
      });
    default = {};
  };

  config = mkIf (cfg.enable && pkgs.stdenv.hostPlatform.isDarwin) {
    home.packages = [pkgs.skhd] ++ optional cfg.windowManagement wm;

    xdg.configFile."skhd/skhdrc".text =
      builtins.readFile ./skhdrc
      + optionalString cfg.windowManagement ''

        # --- Window management (replaces AeroSpace bindings) -----------------
        # Workspace switching (cmd-1..5, cmd-left/right) is handled natively by
        # macOS Mission Control shortcuts — see modules/user/desktop/spaces.nix.

        # Focus — cmd-h/j/k/l
        cmd - h : ${w} focus left
        cmd - j : ${w} focus down
        cmd - k : ${w} focus up
        cmd - l : ${w} focus right

        # Move (swap with neighbor; at the edge, snap to that half)
        cmd + shift - h : ${w} move left
        cmd + shift - j : ${w} move down
        cmd + shift - k : ${w} move up
        cmd + shift - l : ${w} move right

        # Resize focused window
        cmd + ctrl - h : ${w} resize -50 0
        cmd + ctrl - l : ${w} resize 50 0
        cmd + ctrl - k : ${w} resize 0 -50
        cmd + ctrl - j : ${w} resize 0 50

        # Send focused window to desktop N (drag-and-switch; see wm.js `send`)
        ${concatMapStrings (n: "cmd + ctrl - ${toString n} : ${w} send ${toString n}\n") (range 1 5)}
        # Tile every window on this Space (1 → full, 2 → halves, 3+ → master/stack)
        cmd - 0x2C : ${w} tile

        # Fill screen (with gaps)
        cmd + ctrl - f : ${w} place full

        # Terminal
        cmd - return : open -a Ghostty

        # Clipboard history via Raycast
        cmd + shift - v : open "raycast://extensions/raycast/clipboard-history/clipboard-history"
      '';
  };
}
