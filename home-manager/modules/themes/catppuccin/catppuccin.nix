{ config, lib, pkgs, inputs, ... }:
with lib;
with lib.my;
let
  cfg = config.themes.catppuccin;
  gtk-theme = "Catppuccin-Macchiato-Compact-Pink-Dark";
  cursor-theme = "Qogir";
  cursor-package = pkgs.qogir-icon-theme;
in
{
  options.themes.catppuccin = mkOption {
    description = ''
      The GTK configuration for the user.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
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
        }];
      });
    default = {};
  };
  #
  config = lib.mkIf (cfg.enable) {
    home = {
      packages = with pkgs; [
        adw-gtk3
        font-awesome
        unstable.morewaita-icon-theme
        cantarell-fonts
      ];

      file = {
        ".config/background" = { source = ./background.jpg; };
        ".face" = { source = ./dalle-nixos-profile.jpg; };
      };
    };

    desktop.gtk = {
      enable = true;
      name = gtk-theme;
      package = pkgs.catppuccin-gtk.override {
        accents = [ "sapphire" ];
        size = "compact";
        tweaks = [ "rimless" "black" ];
        variant = "macchiato";
      };
    };
  };
}
