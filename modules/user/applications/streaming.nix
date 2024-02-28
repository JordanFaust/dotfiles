{ config, lib, pkgs, inputs, osConfig, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.applications.streaming;
  minimal = config.modules.minimal;
in
{
  options.modules.applications.streaming = mkOption {
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
      enable = true;
      obs.enable = true;
      zoom.enable = true;
    };
  };

  config = lib.mkIf (!minimal && cfg.enable) {
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

