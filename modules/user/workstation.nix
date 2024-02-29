{
  pkgs,
  inputs,
  config,
  lib,
  username,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.workstation;
in {
  options.modules.workstation = mkOption {
    description = ''
      High level options to enable or disable system capabilities. Defaults to a complete
      workstation environment.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "workstation";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  config = lib.mkIf (cfg.enable) {
    # Enable DConf for configuration of systems
    modules.desktop.dconf.enable = true;

    # Enable GTK
    modules.desktop.gtk.enable = config.modules.themes.gtk.enable;
    # home.stateVersion = "21.11";
  };
}
