{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.desktop.gtk;
  cursor-theme = "Qogir";
  cursor-package = pkgs.qogir-icon-theme;
  qtTheme = strings.concatStrings (strings.splitString "-" cfg.qt.name);
  kdeTheme = (pkgs.catppuccin-kde.override { flavour = [ "macchiato" ]; accents = [ "rosewater" ]; });
in
{
  options.desktop.gtk = mkOption {
    description = ''
      The GTK configuration for the user.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
          options = {
            #
            # GTK Theme Configuration
            #
            enable = mkEnableOption "gtk";
            package = mkOption {
              type = with types; nullOr package;
              default = null;
              defaultText = literalExpression "null";
              example = literalExpression "pkgs.yaru-theme";
              description = ''
                Package providing the gtk theme. This package will be installed to your profile.
                If `null` then the gtk theme is assumed to already be available.
              '';
            };
            name = mkOption {
              type = with types; str;
              default = "Adwaita";
              defaultText = literalExpression ''"Adwaita"'';
              example = literalExpression ''"Yaru"'';
              description = "Name of the cursor theme within the package.";
            };

            #
            # Cursor Theme Configuration
            #
            cursor = {
              package = mkOption {
                type = with types; nullOr package;
                default = null;
                defaultText = literalExpression "null";
                example = literalExpression "pkgs.yaru-theme";
                description = ''
                  Package providing the curosr theme. This package will be installed to your profile.
                  If `null` then the cursor theme is assumed to already be available.
                '';
              };
              name = mkOption {
                type = with types; str;
                default = "Adwaita";
                defaultText = literalExpression ''"Adwaita"'';
                example = literalExpression ''"Yaru"'';
                description = "Name of the cursor theme within the package.";
              };

              size = mkOption {
                type = types.nullOr types.int;
                default = null;
                example = 16;
                description = ''
                  The size of the cursor.
                '';
              };
            };

            #
            # QT Theme Configuration
            #
            qt = {
              style = mkOption {
                type = with types; str;
                default = "kvantum";
                defaultText = literalExpression ''"kvantum"'';
                example = literalExpression ''"kvantum"'';
                description = "Name of the theme platform to use for qt themes.";
              };
              platformTheme = mkOption {
                type = with types; str;
                default = "kde";
                defaultText = literalExpression ''"kde"'';
                example = literalExpression ''"gtk"'';
                description = "Name of the platform to use for qt theme";
              };
              package = mkOption {
                type = with types; nullOr package;
                default = null;
                defaultText = literalExpression "null";
                example = literalExpression "pkgs.yaru-theme";
                description = ''
                  Package providing the qt theme. This package will be installed to your profile.
                  If `null` then the theme is assumed to already be available.
                '';
              };
              name = mkOption {
                type = with types; str;
                default = "Adwaita";
                defaultText = literalExpression ''"Adwaita"'';
                example = literalExpression ''"Yaru"'';
                description = "Name of the qt theme within the package.";
              };
            };
          };
        }];
      });
    default = {};
  };

  config = lib.mkIf (cfg.enable) {
    home = {
      packages = with pkgs; [
        gnome.dconf-editor

      ];

      sessionVariables = {
        GTK_THEME = cfg.name;
      };

      pointerCursor = {
        name = "${cfg.cursor.name}";
        package = cfg.cursor.package;
        size = cfg.cursor.size;

        gtk.enable = cfg.enable;
      };


# -rw-r-----  1 jordan users  44732 Jan 11  2023 all-the-icons.ttf
# -rw-r-----  1 jordan users 489672 Jan 11  2023 file-icons.ttf
# -rw-r-----  1 jordan users 152796 Jan 11  2023 fontawesome.ttf
# -rw-r-----  1 jordan users 128180 Jan 11  2023 material-design-icons.ttf
# -rw-r-----  1 jordan users  52544 Jan 11  2023 octicons.ttf
# -rw-r-----  1 jordan users  99564 Jan 11  2023 weathericons.ttf
      file = {
        ".local/share/Kvantum/${qtTheme}".source = "${cfg.qt.package}/share/Kvantum/${cfg.qt.name}";
        ".local/share/plasma/desktoptheme/${qtTheme}".source = "${kdeTheme}/share/color-schemes";
      };
    };

    xdg.configFile = {
      "Kvantum/kvantum.kvconfig".source = (pkgs.formats.ini {}).generate "kvantum.kvconfig" {
        theme.name = qtTheme;
      };
      "Kvantum/${qtTheme}".source = "${cfg.qt.package}/share/Kvantum/${cfg.qt.name}";
    };

    gtk = {
      enable = cfg.enable;
      font.name = "Cascadia Code Regular";
      theme = {
        name = cfg.name;
        package = cfg.package;
      };

      cursorTheme = {
        name = cfg.cursor.name;
        package = cfg.cursor.package;
        size = cfg.cursor.size;
      };

      iconTheme.name = "MoreWaita";

      gtk3.extraCss = ''
        headerbar, .titlebar,
        .csd:not(.popup):not(tooltip):not(messagedialog) decoration{
          border-radius: 0;
        }
      '';

      gtk3.extraConfig = {
        "gtk-application-prefer-dark-theme" = "1";
      };

      gtk4.extraConfig = {
        "gtk-application-prefer-dark-theme" = "1";
      };
    };

    qt = {
      enable = cfg.enable;
      platformTheme = "kde";
      style = {
        name = "kvantum";
        package = cfg.qt.package;
      };
    };
  };
}
