# modules/themes/catppuccin/wayland.nix --- a soothing pastel theme

{ options, config, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;

    # catppuccin-ags = pkgs.callPackage ./ags/ags.nix {};
in {
  config = mkIf (cfg.active == "catppuccin") (mkMerge [
    # Desktop (Wayland) theming
    (mkIf cfg.wayland.enable {

      environment.sessionVariables = {
        XCURSOR_THEME = cfg.gtk.cursorTheme;
        XCURSOR_SIZE = "24";
      };

      environment.systemPackages = [
      ];

      user.packages = with pkgs; [
      ];

      fonts.packages = with pkgs; [
        # General Coding Fonts
        jetbrains-mono
        fira-code
        fira-code-symbols
        cascadia-code
        victor-mono
        # General Sans Fonts
        open-sans
        siji
        # Icon Fonts
        # font-awesome
        # material-icons
        # material-design-icons
        (nerdfonts.override { fonts = [ "CascadiaCode" ]; })
        my.nonicons
      ];

      # Enable GVS for cover art caching
      services.gvfs.enable = true;


      home.file = {
        # Firefox configuration
        ".mozilla/firefox/jordan.default" = { source = ./config/firefox; recursive = true; };
        # General Scripts
        ".scripts" = { source = ./config/scripts; recursive = true; };
      };

      # Other dotfiles
      home.configFile = with config.modules; mkMerge [
        {
          # Sourced from sessionCommands in modules/themes/default.nix
          "xtheme/90-theme".source = ./config/Xresources;
        }
        (mkIf desktop.apps.rofi.enable {
          "rofi/theme" = { source = ./config/rofi; recursive = true; };
        })
        (mkIf desktop.term.kitty.enable {
          "kitty" = { source = ./config/kitty; recursive = true; };
          "kitty/themes/monokai-pro.conf".source = ./config/kitty/themes/monokai-pro.conf;
          "kitty/themes/catppuccin-macchiato.conf".source = ./config/kitty/themes/catppuccin-macchiato.conf;
        })
        (mkIf (desktop.bspwm.enable || desktop.hyprland.enable) {
          # X11 Menu
          "jgmenu" = { source = ./config/jgmenu; recursive = true; };
          # GTK Theme
        })
        (mkIf desktop.media.graphics.vector.enable {
          "inkscape/templates/default.svg".source = ./config/inkscape/default-template.svg;
        })
      ];
    })
  ]);
}

