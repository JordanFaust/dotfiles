{ config, lib, pkgs, inputs, osConfig, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.applications.music;
  minimal = config.modules.minimal;
in
{
  options.modules.applications.music = mkOption {
    description = ''
      Configurations for music players.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
          options = {
            #
            # Music
            #
            enable = mkEnableOption "music";
          };
        }];
      });
    default = {
      enable = true;
    };
  };

  config = mkIf (!minimal && cfg.enable) {
    home = {
      packages = with pkgs; [
        # spotify-tui is fine for selecting and playing music, but incomplete. We
        # still occasionally need the official client for more sophisticated
        # search and the "made for you" playlists.
        spotify
      ];
    };
  };
}
