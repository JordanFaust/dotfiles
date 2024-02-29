{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.gnome;
  configDir = config.dotfiles.configDir;
in {

  options.modules.services.gnome = lib.mkOption {
    description = ''
      Configurations for Gnome services.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "gnome";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };
  config = mkIf cfg.enable {
    # Enable GVS for cover art caching
    services.gvfs.enable = true;

    services.gnome.at-spi2-core.enable = true;
  };
}
