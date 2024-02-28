{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.vm.qemu;
in {
  options.modules.desktop.vm.qemu.system = lib.mkOption {
    default = {};
    description = ''
      Enable Qemu virtualization.
    '';
    type = lib.types.submodule {
      options = {
        enable = mkEnableOption "qemu";
      };
    };
  };

  config = mkIf cfg.system.enable {
    environment.systemPackages = with pkgs; [
      qemu
    ];
  };
}
