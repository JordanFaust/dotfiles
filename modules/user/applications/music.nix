{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.music;
  minimal = config.modules.minimal;
  desktop = pkgs.makeDesktopItem {
    name = "Spotify";
    desktopName = "spotify";
    genericName = "Music Player";
    icon = "spotify-client";
    categories = ["Audio" "Music" "Player" "AudioVideo"];
    exec = "uwsm app -- ${pkgs.spotify}/bin/spotify %U";
    mimeTypes = [ "x-scheme-handler/spotify" ];
    startupNotify = true;
    startupWMClass = "spotify";
    terminal = false;
  };
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
    home = {
      packages = [
        # spotify-tui is fine for selecting and playing music, but incomplete. We
        # still occasionally need the official client for more sophisticated
        # search and the "made for you" playlists.
        desktop
      ];
    };

    # Add Spotify as a startup application
    xdg.configFile = {
      "autostart/spotify.desktop".source = "${desktop}/share/applications/Spotify.desktop";
    };
  };
}
