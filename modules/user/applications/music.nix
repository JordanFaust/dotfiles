{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.music;
  desktop = pkgs.makeDesktopItem {
    name = "Spotify";
    desktopName = "spotify";
    genericName = "Music Player";
    icon = "spotify-client";
    categories = ["Audio" "Music" "Player" "AudioVideo"];
    exec = "uwsm app -- ${pkgs.spotify}/bin/spotify %U";
    mimeTypes = ["x-scheme-handler/spotify"];
    startupNotify = true;
    startupWMClass = "spotify";
    terminal = false;
  };
  inherit (config.modules) minimal;
in {
  options.modules.applications.music = mkOption {
    description = ''
      Configurations for music players.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              #
              # Music
              #
              enable = mkEnableOption "music";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  config = mkIf (!minimal && cfg.enable) {
    home.packages =
      if pkgs.stdenv.hostPlatform.isLinux
      # Linux: custom .desktop wrapper only — avoids duplicate entry in rofi
      then [desktop]
      # macOS: install directly (no rofi, no duplicate concern)
      else [pkgs.spotify];

    # Autostart is Linux/XDG only
    xdg.configFile = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      "autostart/spotify.desktop".source = "${desktop}/share/applications/Spotify.desktop";
    };
  };
}
