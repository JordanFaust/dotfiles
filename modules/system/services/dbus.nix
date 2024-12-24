{
  config,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.dbus;
in {
  options.modules.services.dbus = {
    enable = mkOpt types.bool true;
    apparmor = mkOpt types.str "disabled";
    implementation = mkOpt types.str "dbus";
  };

  config = mkIf cfg.enable {
    services.dbus = {
      implementation = cfg.implementation;
      apparmor = cfg.apparmor;
    };
  };
}
