{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.launcher;
  inherit (config.modules) minimal;
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
  # rofi is Linux/X11/Wayland only — not available on macOS.
  config = mkIf (!minimal && cfg.enable && pkgs.stdenv.hostPlatform.isLinux) {
    home = {
      packages = with pkgs; [
        (writeScriptBin "rofi" ''
          #!${stdenv.shell}
          exec ${rofi}/bin/rofi -terminal ${kitty}/bin/kitty -m -1 "$@"
        '')
      ];
    };
  };
}
