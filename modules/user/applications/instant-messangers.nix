{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.instant-messangers;
  minimal = config.modules.minimal;
  desktop = pkgs.makeDesktopItem {
    name = "Slack";
    desktopName = "Slack";
    genericName = "Slack Client for Linux";
    icon = "slack";
    categories = ["GNOME" "GTK" "Network" "InstantMessaging"];
    exec = "uwsm app -- ${pkgs.slack}/bin/slack -s %U";
    mimeTypes = [ "x-scheme-handler/slack" ];
    startupNotify = true;
    startupWMClass = "Slack";
  };
in {
  options.modules.applications.instant-messangers = mkOption {
    description = ''
      Configurations for instant messangers, such as slack.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              #
              # Launcher
              #
              enable = mkEnableOption "instant-messangers";

              #
              # Slack
              #
              slack = {
                enable = mkEnableOption "slack";
              };
            };
          }
        ];
      });
    default = {
      enable = true;
      slack.enable = true;
    };
  };

  # Add configured instant messangers if this isn't a minimal install and instant messangers are enabled.
  config = mkIf (!minimal && cfg.enable) {
    home = {
      packages =[
        desktop
      ];
    };

    # Add Slack as a startup application
    xdg.configFile = {
      "autostart/slack.desktop".source = "${desktop}/share/applications/Slack.desktop";
    };
  };
}
