{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.system76;
in {
  options.modules.hardware.system76 = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hardware.system76.kernel-modules.enable = true;
    hardware.system76.firmware-daemon.enable = true;
    hardware.system76.power-daemon.enable = true;
    hardware.system76.enableAll = true;
  };
}
