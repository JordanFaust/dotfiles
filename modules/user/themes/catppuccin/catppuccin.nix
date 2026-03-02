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
  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs;
        [
          # Cross-platform packages
          neofetch
          recode
          moreutils
          font-awesome
          jetbrains-mono
          fira-code
          fira-code-symbols
          cascadia-code
          victor-mono
          inputs.private-fonts.packages.${system}.monolisa-variable
          nerd-fonts.caskaydia-cove
          nerd-fonts.symbols-only
          my.nonicons
        ]
        ++ lib.optionals pkgs.stdenv.isLinux [
          # Linux-only: KDE, GTK fonts, font tools, status bar icons
          nerdfix
          cantarell-fonts
          (catppuccin-kde.override {
            flavour = ["macchiato"];
            accents = ["rosewater"];
          })
          fontforge
          siji
        ];

      file = {
        # Firefox configuration (cross-platform)
        ".mozilla/firefox/jordan.default" = {
          source = ./config/firefox;
          recursive = true;
        };
      }
      // lib.optionalAttrs pkgs.stdenv.isLinux {
        # Avatar/Face jpg (used by Linux lockscreen/GTK)
        ".face" = {source = ./assets/dalle-nixos-profile.jpg;};
      };

      sessionVariables = lib.mkIf pkgs.stdenv.isLinux {
        QT_SCALE_FACTOR = "2.6";
        ELM_SCALE = "2.6";
        GDK_SCALE = "0.5";
      };
    };

    catppuccin = {
      enable = true;
      accent = "rosewater";
      flavor = "macchiato";

      gtk.icon.enable = pkgs.stdenv.isLinux;
      cursors.enable = pkgs.stdenv.isLinux;
    };

    # Enable fontconfig to discover fonts added as home.packages above
    fonts.fontconfig.enable = true;

    xdg.configFile =
      {
        # Kitty Config/Theme (cross-platform)
        "kitty" = {
          source = ./config/kitty;
          recursive = true;
        };
        "kitty/themes/catppuccin-macchiato.conf".source = ./config/kitty/themes/catppuccin-macchiato.conf;
        # Powerlevel10k prompt (cross-platform, used when zsh module enabled)
        "zsh/.p10k.zsh".source = ./config/zsh/.p10k.zsh;
      }
      // lib.optionalAttrs pkgs.stdenv.isLinux {
        # Background Image
        "background" = {source = "${osConfig.dotfiles.configDir}/themes/catppuccin/background.jpg";};
        # Lockscreen Image
        "lockscreen" = {source = "${osConfig.dotfiles.configDir}/themes/catppuccin/doggocat.png";};
        # Rofi Themes (Linux-only)
        "rofi/theme" = {
          source = ./config/rofi;
          recursive = true;
        };
      };

    # Source p10k prompt when zsh module is enabled
    programs.zsh.initExtra = lib.mkIf (config.modules.shell.zsh != null && config.modules.shell.zsh.enable) (lib.mkAfter ''
      [[ ! -f ${config.xdg.configHome}/zsh/.p10k.zsh ]] || source ${config.xdg.configHome}/zsh/.p10k.zsh
    '');

    # Apply catppuccin tmux theme when tmux module is enabled
    programs.tmux.extraConfig = lib.mkIf (config.modules.shell.tmux != null && config.modules.shell.tmux.enable) (lib.mkAfter ''
      source-file ${./config/tmux.conf}
    '');

    modules.desktop.gtk = lib.mkIf pkgs.stdenv.isLinux {
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
        name = "Papirus-Dark";
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
        platformTheme = "qt6ct";
        name = "catppuccin-macchiato-rosewater";
        package = pkgs.catppuccin-kvantum.override {
          accent = "rosewater";
          variant = "macchiato";
        };
      };
    };
  };
}
