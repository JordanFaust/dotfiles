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
    exec = "uwsm app -- ${pkgs.kitty}/bin/kitty -e bash -c \"(tmux ls | grep -qEv 'attached|scratch' && tmux at) || tmux\"";
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
    # # We want better tmux integration since that is my primary driver
    # modules.shell.zsh.rcInit = ''
    #   [ "$TERM" = xterm-256color ] && export TERM=screen-256color
    # '';

    home = {
      packages = [
        desktop
      ];

      sessionVariables = {
        TERMINAL = "kitty";
        TERM = "kitty";
      };
    };

    # Add Kitty as a startup application
    xdg.configFile = {
      "autostart/kitty.desktop".source = "${desktop}/share/applications/Kitty.desktop";
    };
  };
}
