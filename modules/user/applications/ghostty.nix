# modules/user/applications/ghostty.nix
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.ghostty;
  desktop = pkgs.makeDesktopItem {
    name = "Ghostty";
    desktopName = "Ghostty";
    genericName = "Terminal emulator";
    icon = "utilities-terminal";
    exec = "uwsm app -- ${pkgs.ghostty}/bin/ghostty";
    categories = ["Development" "System" "Utility"];
  };
in {
  options.modules.applications.ghostty = mkOption {
    description = ''
      Configuration for the Ghostty terminal.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "ghostty";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  config = mkIf cfg.enable {
    home = {
      packages =
        if pkgs.stdenv.isLinux
        # Linux: custom .desktop wrapper only — avoids duplicate entry in rofi
        then [desktop pkgs.ghostty]
        # macOS: install directly (no rofi, no duplicate concern)
        else [pkgs.ghostty];

      sessionVariables = {
        TERMINAL = "ghostty";
        # TERM is intentionally not set here — Ghostty sets it when it spawns a
        # shell. Ghostty bundles its own terminfo (xterm-ghostty); no
        # TERMINFO_DIRS export needed unlike kitty.
      };
    };

    # Desktop item autostart — Linux/XDG only
    xdg.configFile = lib.mkIf pkgs.stdenv.isLinux {
      "autostart/ghostty.desktop".source = "${desktop}/share/applications/Ghostty.desktop";
    };

    programs.ghostty = {
      enable = true;
      settings =
        {
          font-family = "MonoLisa Variable";
          font-size = 18;
          # kitty adjust_line_height 150% → Ghostty counts the delta above 100%
          adjust-cell-height = "50%";
          cursor-style = "block";
          shell-integration = "detect";
          background-opacity = 1.0;
          copy-on-select = false;
          term = "xterm-ghostty";
        }
        // lib.optionalAttrs pkgs.stdenv.isDarwin {
          # macOS-specific settings
          macos-option-as-alt = true;
          macos-titlebar-style = "hidden";
        };
    };
  };
}
