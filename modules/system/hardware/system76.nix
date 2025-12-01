{
  config,
  lib,
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
    hardware = {
      system76 = {
        kernel-modules.enable = true;
        firmware-daemon.enable = true;
        power-daemon.enable = true;
        enableAll = true;
      };
    };
  };
}
