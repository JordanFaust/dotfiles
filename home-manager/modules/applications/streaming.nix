{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.applications.streaming;
in
{
  options.applications.streaming = mkOption {
    description = ''
      Configurations for streaming video and recording the desktop.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
          options = {
            #
            # Streaming
            #
            enable = mkEnableOption "streaming";

            #
            # OBS
            #
            obs = {
              enable = mkEnableOption "obs";
            };

            #
            # Zoom
            #
            zoom = {
              enable = mkEnableOption "zoom";
            };
          };
        }];
      });
    default = {
      enable = false;
      obs.enable = true;
      zoom.enable = true;
    };
  };

  config = lib.mkIf (cfg.enable) {
    home = {
      packages = with pkgs; [
        zoom-us
      ];
    };

    programs.obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-backgroundremoval
        obs-pipewire-audio-capture
      ];
    };
  };
}

