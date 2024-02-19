{ lib, types, ... }:
with lib;
with lib.my;
{
  options = {
    themes = with types; {
      active = mkOption {
        type = nullOr str;
        default = null;
        apply = v: let theme = builtins.getEnv "THEME"; in
                   if theme != "" then theme else v;
        description = ''
          Name of the theme to enable. Can be overridden by the THEME environment
          variable. Themes can also be hot-swapped with 'hey theme $THEME'.
        '';
      };

      wallpaper = mkOpt (either path null) null;

      loginWallpaper = mkOpt (either path null)
        (if cfg.wallpaper != null
         then toFilteredImage cfg.wallpaper "-gaussian-blur 0x2 -modulate 70 -level 5%"
         else null);

      gtk = {
        enable = mkBoolOpt false;
        theme = mkOpt str "";
        iconTheme = mkOpt str "";
        cursorTheme = mkOpt str "";
      };

      onReload = mkOpt (attrsOf lines) {};
    };
  };
}
