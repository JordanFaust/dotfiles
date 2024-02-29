{lib, ...}:
with lib;
with lib.my; {
  options = {
    modules = {
      # Minimal will reduce the packages, services, and programs configured to the minimal
      # set needed for a VM or a live boot disk image.
      minimal = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Only install the minimally required software";
      };

      #
      # Theme Configurations
      #
      themes = with types; {
        active = mkOption {
          type = nullOr str;
          default = null;
          apply = v: let
            theme = builtins.getEnv "THEME";
          in
            if theme != ""
            then theme
            else v;
          description = ''
            Name of the theme to enable. Can be overridden by the THEME environment
            variable. Themes can also be hot-swapped with 'hey theme $THEME'.
          '';
        };

        wallpaper = mkOpt (either path null) null;

        loginWallpaper =
          mkOpt (either path null)
          (
            if cfg.wallpaper != null
            then toFilteredImage cfg.wallpaper "-gaussian-blur 0x2 -modulate 70 -level 5%"
            else null
          );

        gtk = {
          enable = mkBoolOpt false;
          theme = mkOpt str "";
        };

        onReload = mkOpt (attrsOf lines) {};
      };
    };
  };
}
