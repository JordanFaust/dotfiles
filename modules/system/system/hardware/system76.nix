{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.system76;
in {
  options.modules.hardware.system76 = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hardware.system76.kernel-modules.enable = true;
    hardware.system76.firmware-daemon.enable = true;
    hardware.system76.power-daemon.enable = true;

    # Prevent laptop from sleeping when lid is closed
    services.logind.lidSwitch = "ignore";

    environment.systemPackages = with pkgs; [
      # Respect XDG conventions, damn it!
      # (writeScriptBin "nvidia-settings" ''
      #   #!${stdenv.shell}
      #   mkdir -p "$XDG_CONFIG_HOME/nvidia"
      #   exec ${config.boot.kernelPackages.nvidia_x11.settings}/bin/nvidia-settings --config="$XDG_CONFIG_HOME/nvidia/settings"
      # '')
    ];
  };
}
