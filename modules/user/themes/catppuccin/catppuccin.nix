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
        # TODO replace pamixer with amixer
        # misc
        # gpick
        neofetch
        # Dunst + EWW
        recode
        # Added utilities used in rice scripts
        moreutils
        # Fix broken nerd fonts
        nerdfix

        # adw-gtk3
        font-awesome
        cantarell-fonts

        (catppuccin-kde.override {
          flavour = ["macchiato"];
          accents = ["rosewater"];
        })

        #
        # Font Config
        #

        fontforge
        # General Coding Fonts
        jetbrains-mono
        fira-code
        fira-code-symbols
        cascadia-code
        victor-mono
        inputs.private-fonts.packages.${system}.monolisa-variable
        # monaspace
        # General Sans Fonts
        # open-sans

        siji
        nerd-fonts.caskaydia-cove
        nerd-fonts.symbols-only
        # Icon Fonts
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

      sessionVariables = {
        QT_SCALE_FACTOR = "2.6";
        ELM_SCALE = "2.6";
        GDK_SCALE = "0.5";
      };
    };

    # Enable fontconfig to discover fonts added as home.packages above
    fonts.fontconfig.enable = true;

    xdg.configFile = {
      # Background Image
      "background" = {source = "${osConfig.dotfiles.configDir}/themes/catppuccin/background.jpg";};
      # Lockscreen Image
      "lockscreen" = {source = "${osConfig.dotfiles.configDir}/themes/catppuccin/doggocat.png";};
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
      name = "Catppuccin-GTK-Red-Dark-Compact-Macchiato";
      package = pkgs.magnetic-catppuccin-gtk.override {
        accent = ["red"];
        size = "compact";
        tweaks = ["macchiato"];
        shade = "dark";
      };

      font.name = "MonoLisa Variable Regular";

      icon = {
        name = "Papirus";
        package = pkgs.catppuccin-papirus-folders.override {
          accent = "red";
          flavor = "macchiato";
        };
      };

      cursor = {
        name = "catppuccin-macchiato-red-cursors";
        package = pkgs.catppuccin-cursors.macchiatoRed;
        size = 32;
      };

      qt = {
        style = "kvantum";
        platformTheme = "qt5ct";
        name = "catppuccin-macchiato-rosewater";
        package = pkgs.catppuccin-kvantum.override {
          accent = "rosewater";
          variant = "macchiato";
        };
      };
    };
  };
}
