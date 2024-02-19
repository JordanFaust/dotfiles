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

    themes.gtk.enable = true;
    themes.catppuccin.enable = true;
}
