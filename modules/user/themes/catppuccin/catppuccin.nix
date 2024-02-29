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
  cfg = config.modules.themes.catppuccin;
  gtk-theme = "Catppuccin-Macchiato-Compact-Pink-Dark";
  cursor-theme = "Qogir";
  cursor-package = pkgs.qogir-icon-theme;
in {
  options.modules.themes.catppuccin = mkOption {
    description = ''
      The GTK configuration for the user.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "catppuccin";
              variant = mkOption {
                type = with types; str;
                default = "macchiato";
                defaultText = literalExpression ''"macchiato"'';
                example = literalExpression ''"macchiato"'';
                description = "Name of the catppuccin variant";
              };
            };
          }
        ];
      });
    default = {};
  };
  #
  config = lib.mkIf (cfg.enable) {
    home = {
      packages = with pkgs; [
        paper-icon-theme # for rofi
        papirus-icon-theme # dunst
        gnome.adwaita-icon-theme
        # TODO replace pamixer with amixer
        pamixer
        alsa-utils
        playerctl
        # misc
        gpick
        neofetch
        # Dunst + EWW
        recode
        # Added utilities used in rice scripts
        moreutils
        # Fix broken nerd fonts
        nerdfix

        adw-gtk3
        font-awesome
        unstable.morewaita-icon-theme
        cantarell-fonts

        (catppuccin-kde.override {
          flavour = ["macchiato"];
          accents = ["rosewater"];
        })

        #
        # Font Config
        #

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
        (nerdfonts.override {fonts = ["CascadiaCode"];})
        my.nonicons
      ];

      file = {
        # Avatar/Face jpg
        ".face" = {source = ./assets/dalle-nixos-profile.jpg;};

        # Firefox configuration
        ".mozilla/firefox/jordan.default" = {
          source = ./config/firefox;
          recursive = true;
        };
      };
    };

    # Enable fontconfig to discover fonts added as home.packages above
    fonts.fontconfig.enable = true;

    xdg.configFile = {
      # Background Image
      "background" = {source = ./background.jpg;};
      # Rofi Themes
      "rofi/theme" = {
        source = ./config/rofi;
        recursive = true;
      };
      # Kitty Config/Theme
      "kitty" = {
        source = ./config/kitty;
        recursive = true;
      };
      "kitty/themes/catppuccin-macchiato.conf".source = ./config/kitty/themes/catppuccin-macchiato.conf;
    };

    modules.desktop.gtk = {
      enable = true;
      name = gtk-theme;
      package = pkgs.catppuccin-gtk.override {
        accents = ["rosewater"];
        size = "compact";
        tweaks = ["rimless" "black"];
        variant = "macchiato";
      };

      cursor = {
        name = "Catppuccin-Macchiato-Red-Cursors";
        package = pkgs.catppuccin-cursors.macchiatoRed;
        size = 36;
      };

      qt = {
        style = "kvantum";
        platformTheme = "qt5ct";
        name = "Catppuccin-Macchiato-Rosewater";
        package = pkgs.catppuccin-kvantum.override {
          accent = "Rosewater";
          variant = "Macchiato";
        };
      };
    };
  };
}
