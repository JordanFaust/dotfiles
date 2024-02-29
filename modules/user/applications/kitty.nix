{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.kitty;
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
      packages = with pkgs; [
        kitty
        (makeDesktopItem {
          name = "Kitty";
          desktopName = "Kitty";
          genericName = "Kitty terminal";
          icon = "utilities-terminal";
          exec = "${kitty}/bin/kitty";
          categories = ["Development" "System" "Utility"];
        })
      ];

      sessionVariables = {
        TERMINAL = "kitty";
        TERM = "kitty";
      };
    };
  };
}
