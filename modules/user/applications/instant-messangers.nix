{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.instant-messangers;
  desktop = pkgs.makeDesktopItem {
    name = "Slack";
    desktopName = "Slack";
    genericName = "Slack Client for Linux";
    icon = "slack";
    categories = [
      "GNOME"
      "GTK"
      "Network"
      "InstantMessaging"
    ];
    exec = "uwsm app -- ${pkgs.slack}/bin/slack --enable-features=UseOzonePlatform --ozone-platform=wayland -s %U";
    mimeTypes = ["x-scheme-handler/slack"];
    startupNotify = true;
    startupWMClass = "Slack";
  };
  inherit (config.modules) minimal;
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

  config = mkIf (!minimal && cfg.enable) {
    home.packages =
      if pkgs.stdenv.hostPlatform.isLinux
      # Linux: custom .desktop wrapper only — avoids duplicate entry in rofi
      then [desktop]
      # macOS: managed install, don't use nixpkgs
      else [];

    # Autostart is Linux/XDG only
    xdg.configFile = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      "autostart/slack.desktop".source = "${desktop}/share/applications/Slack.desktop";
    };
  };
}
