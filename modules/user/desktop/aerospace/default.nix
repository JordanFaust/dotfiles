{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.aerospace;
in {
  options.modules.desktop.aerospace = mkOption {
    description = "AeroSpace tiling window manager for macOS";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "aerospace";
          }
        ];
      });
    default = {};
  };

  config = mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
    home.packages = [pkgs.aerospace];

    xdg.configFile."aerospace/aerospace.toml".source = ./aerospace.toml;
  };
}
