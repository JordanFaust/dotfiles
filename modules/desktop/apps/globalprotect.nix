{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.globalprotect;
in {
  options.modules.desktop.apps.globalprotect = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      unstable.globalprotect-openconnect
      unstable.openconnect
    ];

    services.globalprotect = {
      enable = true;
      csdWrapper = "${pkgs.unstable.openconnect}/libexec/openconnect/hipreport.sh";
    };
  };
}
