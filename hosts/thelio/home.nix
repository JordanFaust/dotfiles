{
  pkgs,
  inputs,
  config,
  lib,
  home-manager,
  osConfig,
  system,
  ...
}:
with lib;
with lib.my; let
  username = "jordan";
  homeDirectory = "/home/${username}";
in {
  imports =
    # Space to include configuration that must run first
    []
    # # All my personal modules
    ++ (mapModulesRec'
      (toString ../../modules/user)
      (path: import path {inherit pkgs inputs config lib username osConfig system;}));
      # (path: import path));

  # extraSpecialArgs = { inherit pkgs inputs config lib osConfig system; };

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
