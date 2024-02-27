{ pkgs, inputs, config, lib, home-manager, ... }:
with lib;
with lib.my;
let
  username = "jordan";
  homeDirectory = "/home/${username}";
in
{
  imports =
    # Space to include configuraton that must run first
    [( import ../../home-manager/workstation.nix { inherit pkgs inputs config lib username; } )];
    # # All my personal modules
    # ++ (mapModulesRec' (toString ../../home-manager) import);

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
  }
}
