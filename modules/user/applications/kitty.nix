{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.kitty;
  desktop = pkgs.makeDesktopItem {
    name = "Kitty";
    desktopName = "Kitty";
    genericName = "Terminal emulator";
    icon = "utilities-terminal";
    exec = "uwsm app -- ${pkgs.kitty}/bin/kitty";
    categories = ["Development" "System" "Utility"];
  };
in {
  options.modules.applications.kitty = mkOption {
    description = ''
      Configuration for the Kitty terminal.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "kitty";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  # Always enable kitty, no matter the installation
  config = mkIf cfg.enable {
    home = {
      packages =
        if pkgs.stdenv.isLinux
        # Linux: custom .desktop wrapper only — avoids duplicate entry in rofi
        then [desktop pkgs.ghostty]
        # macOS: install directly (no rofi, no duplicate concern)
        else [pkgs.kitty pkgs.ghostty];

      sessionVariables = {
        TERMINAL = "kitty";
        TERM = "kitty";
      };
    };

    # Desktop item and autostart are Linux/XDG only
    xdg.configFile = lib.mkIf pkgs.stdenv.isLinux {
      "autostart/kitty.desktop".source = "${desktop}/share/applications/Kitty.desktop";
    };
  };
}
