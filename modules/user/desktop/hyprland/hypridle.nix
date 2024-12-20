{
  config,
  inputs,
  pkgs,
  lib,
  system,
  ...
}:
let
  cfg = config.modules.desktop.hyprland;
in {
  options = {};

  config = lib.mkIf (cfg.enable) {
    services.hypridle = {
      enable = true;

      # Generate Configuration
      settings = {
        # Listeners
        listener = [
          # Lock Screen Timeout
          {
            timeout = 900;
            on-timeout = "${lib.getExe inputs.hyprlock.packages.${system}.default}";
            on-resume = "${lib.getExe pkgs.libnotify} Unlocked!";
          }
          # Screen Off
          {
            timeout = 1000;
            on-timeout = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
            on-resume = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
          }
          # System Idle
          {
            timeout = 1800;
            on-timeout = "systemctl suspend";
          }
        ];
      };
    };
  };
}
