{
  config,
  lib,
  pkgs,
  # system,
  # inputs,
  # osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.streaming;
  minimal = config.modules.minimal;
  # # Configure allow unfree for the pinned packages sha
  # mkPkgs = pkgs:
  #   import pkgs {
  #     inherit system;
  #     config.allowUnfree = true;
  #   };
  # # The pinned packages set containing a functioning zoom-us package
  # pinnedZoomPkgs = mkPkgs (builtins.getFlake "github:NixOS/nixpkgs/0c19708cf035f50d28eb4b2b8e7a79d4dc52f6bb");
  zoomDesktop = pkgs.makeDesktopItem {
    name = "zoom";
    desktopName = "Zoom";
    genericName = "Open Zoom";
    icon = "Zoom";
    exec = "uwsm app -- ${pkgs.zoom-us}/bin/zoom --enable-features=UseOzonePlatform --ozone-platform=wayland %U";
    categories = ["Network" "Application"];
    mimeTypes = [
      "x-scheme-handler/zoommtg"
      "x-scheme-handler/zoomus"
      "x-scheme-handler/tel"
      "x-scheme-handler/callto"
      "x-scheme-handler/zoomphonecall"
      "x-scheme-handler/zoomphonesms"
      "x-scheme-handler/zoomcontactcentercall"
      "application/x-zoom"
    ];
    startupNotify = true;
    startupWMClass = "zoom";
    terminal = false;
  };
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
        zoomDesktop
        # pinnedZoomPkgs.zoom-us
      ];
    };

    # Add zoom as a startup application
    xdg.configFile = {
      "autostart/zoom.desktop".source = "${zoomDesktop}/share/applications/Zoom.desktop";
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
