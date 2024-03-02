{
  config,
  inputs,
  pkgs,
  lib,
  osConfig,
  # system,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.hyprland;
in {
  imports = [inputs.hypridle.homeManagerModules.default];
  options = {};

  config = lib.mkIf (cfg.enable) {
    services.hypridle = {
      enable = true;

      # Generate Configuration

      # Listeners
      listeners = [
        # Lock Screen Timeout
        {
          timeout = 900;
          onTimeout = "${lib.getExe inputs.hyprlock.packages."x86_64-linux".default}";
          onResume  = "${lib.getExe pkgs.libnotify} Unlocked!";
        }
        # Screen Off
        {
          timeout = 1000;
          onTimeout = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
          onResume  = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
        }
        # System Idle
        {
          timeout = 1800;
          onTimeout = "systemctl suspend";
        }
      ];
    };
  };
}
