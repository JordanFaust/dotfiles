{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.launcher;
  minimal = config.modules.minimal;
in {
  options.modules.applications.launcher = mkOption {
    description = ''
      Configurations for application launchers, such as rofi.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              #
              # Launcher
              #
              enable = mkEnableOption "launcher";

              #
              # Rofi
              #
              rofi = {
                enable = mkEnableOption "rofi";
              };
            };
          }
        ];
      });
    default = {
      enable = true;
      rofi.enable = true;
    };
  };

  # Add configured launchers if this isn't a minimal install and launchers are enabled.
  config = mkIf (!minimal && cfg.enable) {
    home = {
      packages = with pkgs; [
        (writeScriptBin "rofi" ''
          #!${stdenv.shell}
          exec ${pkgs.rofi-wayland}/bin/rofi -terminal xst -m -1 "$@"
        '')
      ];
    };
  };
}
