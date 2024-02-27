{ pkgs, inputs, config, lib, username, ... }:
with lib;
with lib.my;
let
in
{
  imports =
    # Space to include configuraton that must run first
    # TODO: might remove
    [( import ./common.nix { inherit pkgs inputs config lib username; })];

  # Enable DConf for configuration of systems
  dconf.enable = true;

  # Enable GTK
  gtk.enable = config.themes.gtk.enable;
  # home.stateVersion = "21.11";
}

