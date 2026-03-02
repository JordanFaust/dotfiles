# hosts/darwin/work/home.nix
#
# Home Manager configuration for the work MacBook.
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
  home.stateVersion = "24.11";

  imports =
    [inputs.catppuccin.homeModules.catppuccin]
    ++ (mapModulesRec'
      (toString ../../../modules/user)
      (path: import path {inherit pkgs inputs config lib username osConfig system;}));

  modules = {
    workstation.enable = true;

    shell = { direnv.enable = true; git.enable = true; gnupg.enable = true; tmux.enable = true; zsh.enable = true; };

    desktop = {
      aerospace.enable = true;
      skhd.enable = true;
    };

    themes = {
      catppuccin.enable = true;
      gtk.enable = false;
    };

    development = {
      node.enable = true;
      golang.enable = true;
      rust.enable = true;
    };
  };
}
