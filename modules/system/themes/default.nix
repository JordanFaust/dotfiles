# Theme modules are a special beast. They're the only modules that are deeply
# intertwined with others, and are solely responsible for aesthetics. Disabling
# a theme module should never leave a system non-functional.
{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.theme;
in {
  options.modules.theme = with types; {
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

    wayland = {
      enable = mkBoolOpt false;
    };
    xserver = {
      enable = mkBoolOpt false;
    };

    gtk = {
      theme = mkOpt str "";
      iconTheme = mkOpt str "";
      cursorTheme = mkOpt str "";
    };

    onReload = mkOpt (attrsOf lines) {};

    fonts = {
      # TODO Use submodules
      mono = {
        name = mkOpt str "Monospace";
        size = mkOpt int 12;
      };
      sans = {
        name = mkOpt str "Sans";
        size = mkOpt int 10;
      };
    };

    colors = {
      black = mkOpt str "#000000"; # 0
      red = mkOpt str "#FF0000"; # 1
      green = mkOpt str "#00FF00"; # 2
      yellow = mkOpt str "#FFFF00"; # 3
      blue = mkOpt str "#0000FF"; # 4
      magenta = mkOpt str "#FF00FF"; # 5
      cyan = mkOpt str "#00FFFF"; # 6
      silver = mkOpt str "#BBBBBB"; # 7
      grey = mkOpt str "#888888"; # 8
      brightred = mkOpt str "#FF8800"; # 9
      brightgreen = mkOpt str "#00FF80"; # 10
      brightyellow = mkOpt str "#FF8800"; # 11
      brightblue = mkOpt str "#0088FF"; # 12
      brightmagenta = mkOpt str "#FF88FF"; # 13
      brightcyan = mkOpt str "#88FFFF"; # 14
      white = mkOpt str "#FFFFFF"; # 15

      # Color classes
      types = {
        bg = mkOpt str cfg.colors.black;
        fg = mkOpt str cfg.colors.white;
        panelbg = mkOpt str cfg.colors.types.bg;
        panelfg = mkOpt str cfg.colors.types.fg;
        border = mkOpt str cfg.colors.types.bg;
        error = mkOpt str cfg.colors.red;
        warning = mkOpt str cfg.colors.yellow;
        highlight = mkOpt str cfg.colors.white;
      };
    };
  };

  config = mkIf (cfg.active != null) (mkMerge [
    {
      fonts = {
        packages = with pkgs; [
          jetbrains-mono
          fira-code
          fira-code-symbols
          cascadia-code
          victor-mono
          # General Sans Fonts
          open-sans
          siji
          # Icon Fonts
          (nerdfonts.override {fonts = ["CascadiaCode"];})
          my.nonicons
        ];
        fontconfig.defaultFonts = {
          sansSerif = [cfg.fonts.sans.name];
          monospace = [cfg.fonts.mono.name];
        };
      };
    }
  ]);
}
