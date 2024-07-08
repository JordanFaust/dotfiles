{
  config,
  lib,
  pkgs,
  system,
  inputs,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.streaming;
  minimal = config.modules.minimal;
  # Configure allow unfree for the pinned packages sha
  mkPkgs = pkgs:
    import pkgs {
      inherit system;
      config.allowUnfree = true;
    };
  # The pinned packages set containing a functioning zoom-us package
  pinnedZoomPkgs = mkPkgs (builtins.getFlake "github:NixOS/nixpkgs/4a3fc4cf736b7d2d288d7a8bf775ac8d4c0920b4");
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
        pinnedZoomPkgs.zoom-us
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
