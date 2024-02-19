{ options, config, lib, pkgs, inputs, home-manager, ... }:
with lib;
with lib.my;
let
  gtk-theme = "Catppuccin-Macchiato-Compact-Pink-Dark";

  cursor-theme = "Qogir";
  cursor-package = pkgs.qogir-icon-theme;
in
{
  options.modules.theme.gtk = {
    enable = mkBoolOpt false;
  };

  config = {
    home = {
      packages = with pkgs; [
        # font-awesome
        # nerdfonts
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
      enable = true;
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
