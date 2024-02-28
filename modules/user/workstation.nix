{ pkgs, inputs, config, lib, username, osConfig, ... }:
with lib;
with lib.my;
let
in
{
  # imports =
  #   # Space to include configuraton that must run first
  #   # TODO: might remove
  #   [( import ./common.nix { inherit pkgs inputs config lib username; })];
  options.module.workstation = mkOption {
    description = ''
      High level options to enable or disable system capabilities. Defaults to a complete
      workstation environment.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
          options = {
            enable = mkEnableOption "workstation";
          };
        }];
      });
    default = {
      enable = true;
    };
  };

  config = lib.mkIf (cfg.enable) {
    # Enable DConf for configuration of systems
    modules.dconf.enable = true;

    # Enable GTK
    modules.gtk.enable = config.themes.gtk.enable;
    # home.stateVersion = "21.11";
  };
}

