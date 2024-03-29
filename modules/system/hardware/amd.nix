{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.amd;
in {
  options.modules.hardware.amd = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    services.xserver.videoDrivers = ["amdgpu"];
  };
}
