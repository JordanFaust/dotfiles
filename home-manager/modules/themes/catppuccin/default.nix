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
  options.theme.catppuccin = lib.mkOption {
    description = ''
      Catppuccin is a community-driven pastel theme that aims to be the middle ground between low and high contrast themes.
    '';
    enable = lib.mkEnableOption "enable catppuccin theme";
  };

  config = lib.mkIf (cfg.enable) {
    config.desktop.gtk = {
      enable = true;
      name = gtk-theme;
      package = pkgs.catppuccin-gtk.override {
        accents = [ "pink" ];
        size = "compact";
        tweaks = [ "rimless" "black" ];
        variant = "macchiato";
      };
    };
  };
}
