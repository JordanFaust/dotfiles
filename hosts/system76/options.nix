{ pkgs, inputs, config, lib, home-manager, osConfig, ... }:
with lib;
with lib.my;
in
{
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
