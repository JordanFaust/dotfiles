{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.desktop.gtk;
  cursor-theme = "Qogir";
  cursor-package = pkgs.qogir-icon-theme;
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
            enable = mkEnableOption "gtk";
            package = mkOption {
              type = with types; nullOr package;
              default = null;
              defaultText = literalExpression "null";
              example = literalExpression "pkgs.yaru-theme";
              description = ''
                Package providing the cursor theme. This package will be installed to your profile.
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
        XCURSOR_THEME = cursor-theme;
        XCURSOR_SIZE = "24";
        GTK_THEME = cfg.name;
      };

      pointerCursor = {
        package = cursor-package;
        name = cursor-theme;
        size = 24;
        gtk.enable = cfg.enable;
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
        # ".config/gtk-4.0/gtk.css" = {
        #   text = ''
        #     window.messagedialog .response-area > button,
        #     window.dialog.message .dialog-action-area > button,
        #     .background.csd{
        #       border-radius: 0;
        #     }
        #   '';
        # };
      };
    };

    gtk = {
      enable = cfg.enable;
      font.name = "Cascadia Code Regular";
      theme = {
        name = cfg.name;
        package = cfg.package;
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

      # gtk3.extraConfig = {
      #   Settings = ''
      #     gtk-application-prefer-dark-theme=1
      #   '';
      # };
      #
      # gtk4.extraConfig = {
      #   Settings = ''
      #     gtk-application-prefer-dark-theme=1
      #   '';
      # };
    };

    qt = {
      enable = true;
      platformTheme = "kde";
    };
  };
}
