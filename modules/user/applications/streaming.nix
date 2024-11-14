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
  pinnedZoomPkgs = mkPkgs (builtins.getFlake "github:NixOS/nixpkgs/0c19708cf035f50d28eb4b2b8e7a79d4dc52f6bb");
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
      obs.enable = false;
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
      enable = cfg.obs.enable;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-backgroundremoval
        obs-pipewire-audio-capture
      ];
    };
  };
}
