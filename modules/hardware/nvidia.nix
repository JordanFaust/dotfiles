{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.nvidia;
in {
  options.modules.hardware.nvidia = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # hardware.opengl = {
    #   enable = true;
    #   driSupport = true;
    #   driSupport32Bit = true;
    # };

    # hardware.nvidia.modesetting.enable = false;
    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.nvidia.prime = {
      sync.enable = true;
      nvidiaBusId = "PCI:1:0:0";
      intelBusId  = "PCI:0:2:0";
    };

    # programs.autorandr.enable = true;
    environment.systemPackages = with pkgs; [
      arandr
      glxinfo
      inxi
      lshw
      pciutils
      autorandr
      # Respect XDG conventions, damn it!
      (writeScriptBin "nvidia-settings" ''
        #!${stdenv.shell}
        mkdir -p "$XDG_CONFIG_HOME/nvidia"
        exec ${config.boot.kernelPackages.nvidia_x11.settings}/bin/nvidia-settings --config="$XDG_CONFIG_HOME/nvidia/settings"
      '')
    ];
  };
}
