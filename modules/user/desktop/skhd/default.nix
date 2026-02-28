{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.skhd;
in {
  options.modules.desktop.skhd = mkOption {
    description = "skhd hotkey daemon for macOS";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "skhd";
          }
        ];
      });
    default = {};
  };

  config = mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
    home.packages = [pkgs.skhd];

    xdg.configFile."skhd/skhdrc".source = ./skhdrc;
  };
}
