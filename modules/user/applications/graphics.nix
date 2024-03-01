{
  config,
  lib,
  pkgs,
  inputs,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.graphics;
  minimal = config.modules.minimal;
in {
  options.modules.applications.graphics = mkOption {
    description = ''
      Configurations for graphic design.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              #
              # Graphics
              #
              enable = mkEnableOption "graphics";

              #
              # Tools
              #
              tools = {
                enable = mkEnableOption "tools";
              };

              #
              # Raster
              #
              raster = {
                enable = mkEnableOption "raster";
              };

              #
              # Vector
              #
              vector = {
                enable = mkEnableOption "vector";
              };
            };
          }
        ];
      });
    default = {
      enable = true;
      tools.enable = true;
      raster.enable = true;
      vector.enable = true;
    };
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs;
        (
          if cfg.tools.enable
          then [
            font-manager # so many damned fonts...
            imagemagick # for image manipulation from the shell
          ]
          else []
        )
        ++
        # replaces illustrator & indesign
        (
          if cfg.vector.enable
          then [
            unstable.inkscape
          ]
          else []
        )
        ++
        # Replaces photoshop
        (
          if cfg.raster.enable
          then [
            krita
            gimp
            # gimpPlugins.resynthesizer  # content-aware scaling in gimp
          ]
          else []
        );
    };

    xdg.configFile = mkIf cfg.raster.enable {
      "GIMP/2.10" = {
        source = "${osConfig.dotfiles.configDir}/gimp";
        recursive = true;
      };
    };
  };
}
