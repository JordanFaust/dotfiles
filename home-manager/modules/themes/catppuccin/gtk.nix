{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.themes.gtk;
  gtk-theme = "Catppuccin-Macchiato-Compact-Pink-Dark";
  cursor-theme = "Qogir";
  cursor-package = pkgs.qogir-icon-theme;
in
{
  options.theme.gtk = lib.mkOption {
    description = ''
      The GTK configuration for the user.
    '';
    enable = lib.mkEnableOption "enable GTK with configured themes";
    package = mkOption {
      type = with types; nullOr package;
      default = null;
      defaultText = literalExpression "null";
      example = literalExpression "pkgs.yaru-theme";
      description = ''
        Package providing the GTK theme. This package will be installed to your profile.
        If `null` then the GTK theme is assumed to already be available.
      '';
    };
    name = mkOption {
      type = with types; str;
      default = "Adwaita";
      defaultText = literalExpression ''"Adwaita"'';
      example = literalExpression ''"Yaru"'';
      description = "Name of the cursor theme within the package.";
    };
  };

  config = lib.mkIf (cfg.enable) {
    home = {
      packages = with pkgs; [
        # font-awesome
        # morewaita-icon-theme
        # cantarell-fonts
      ];
      sessionVariables = {
        XCURSOR_THEME = cursor-theme;
        XCURSOR_SIZE = "24";
      };
      pointerCursor = {
        package = cursor-package;
        name = cursor-theme;
        size = 24;
        # gtk.enable = true;
      };
      file = {
        # ".local/share/fonts" = {
        #   recursive = true;
        #   source = "${nerdfonts}/share/fonts/truetype/NerdFonts";
        # };
        # ".fonts" = {
        #   recursive = true;
        #   source = "${nerdfonts}/share/fonts/truetype/NerdFonts";
        # };
        ".config/gtk-4.0/gtk.css" = {
          text = ''
            window.messagedialog .response-area > button,
            window.dialog.message .dialog-action-area > button,
            .background.csd{
              border-radius: 0;
            }
          '';
        };
      };
    };

    gtk = {
      enable = cfg.enable;
      font.name = "Cascadia Code Regular";
      theme = {
        name = gtk-theme;
        package = pkgs.catppuccin-gtk.override {
          accents = [ "pink" ];
          size = "compact";
          tweaks = [ "rimless" "black" ];
          variant = "macchiato";
        };
      };
      # cursorTheme = {
      #   name = cursor-theme;
      #   package = cursor-package;
      # };
      iconTheme.name = "MoreWaita";
      gtk3.extraCss = ''
        headerbar, .titlebar,
        .csd:not(.popup):not(tooltip):not(messagedialog) decoration{
          border-radius: 0;
        }
      '';
    };

    qt = {
      enable = true;
      platformTheme = "kde";
    };
  };
}
