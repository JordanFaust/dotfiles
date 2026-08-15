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
        if pkgs.stdenv.hostPlatform.isLinux
        # Linux: custom .desktop wrapper only — avoids duplicate entry in rofi
        then [desktop]
        # macOS: install directly (no rofi, no duplicate concern)
        else [pkgs.kitty];

      sessionVariables = {
        TERMINAL = "kitty";
        # TERM is intentionally not set here — the terminal emulator sets it when
        # it spawns a shell. Setting it globally breaks tools (like hm-session-vars.sh)
        # that run outside of an actual kitty window and can't find the terminfo entry.
        # Expose kitty's bundled terminfo so xterm-kitty is resolvable system-wide.
        TERMINFO_DIRS = "${pkgs.kitty.terminfo}/share/terminfo";
      };
    };

    # Desktop item and autostart are Linux/XDG only
    xdg.configFile = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      "autostart/kitty.desktop".source = "${desktop}/share/applications/Kitty.desktop";
    };
  };
}
