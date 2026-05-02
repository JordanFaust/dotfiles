{
  config,
  lib,
  pkgs,
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

  config = lib.mkIf cfg.enable {
    # Enable DConf for configuration of systems (Linux only)
    modules.desktop.dconf.enable = lib.mkIf pkgs.stdenv.isLinux true;

    # Enable GTK (Linux only)
    modules.desktop.gtk.enable = lib.mkIf pkgs.stdenv.isLinux config.modules.themes.gtk.enable;
    # home.stateVersion = "21.11";
  };
}
