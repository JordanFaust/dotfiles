{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.keybase;
in {
  options.modules.desktop.apps.keybase = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.keybase.enable = true;
  };
}
