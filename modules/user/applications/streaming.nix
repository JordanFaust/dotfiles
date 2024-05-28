{
  config,
  lib,
  pkgs,
  inputs,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.streaming;
  minimal = config.modules.minimal;
in {
  options.modules.applications.streaming = mkOption {
    description = ''
      Configurations for streaming video and recording the desktop.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "streaming";
              obs = {
                enable = mkEnableOption "obs";
              };
              zoom = {
                enable = mkEnableOption "zoom";
              };
            };
          }
        ];
      });
    default = {
      enable = true;
      obs.enable = true;
      zoom.enable = true;
    };
  };

  config = lib.mkIf (!minimal && cfg.enable) {
    # Force Zoom to open in the browser until issues are resolved
    home = {
      packages = with pkgs; [
        # zoom-us
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
