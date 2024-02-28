{ pkgs, inputs, config, lib, home-manager, osConfig, ... }:
with lib;
with lib.my;
let
  username = "jordan";
  homeDirectory = "/home/${username}";
in
{
  imports =
    # Space to include configuraton that must run first
    [ ]
    # # All my personal modules
    ++ (mapModulesRec'
         (toString ../../modules/user)
         (path: import path { inherit pkgs inputs config lib username osConfig; }));

  #
  # Desktop and Window Manager Configuration
  #
  desktop = {
    hyprland.enable = true;
  };

  #
  # Desktop Theme Configuration
  #
  themes = {
    gtk.enable = true;
    catppuccin.enable = true;
  };

  # Applications
  applications = {
    streaming.enable = true;
  };
}
