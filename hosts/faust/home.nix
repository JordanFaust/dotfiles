{
  pkgs,
  inputs,
  config,
  lib,
  osConfig,
  system,
  ...
}:
with lib;
with lib.my; let
  username = "jordan";
in {
  imports =
    # Space to include configuration that must run first
    [
      inputs.catppuccin.homeModules.catppuccin
    ]
    # # All my personal modules
    ++ (mapModulesRec'
      (toString ../../modules/user)
      (path: import path {inherit pkgs inputs config lib username osConfig system;}));

  modules = {
    # Enable all standardized components for a full development workstation.
    workstation.enable = true;

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
  };
}
