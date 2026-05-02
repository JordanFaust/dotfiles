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
  username = "jordan.faust";
in {
  home.stateVersion = "24.11";

  imports =
    [
      inputs.catppuccin.homeModules.catppuccin
    ]
    ++ (mapModulesRec' (toString ../../../modules/user) (
      path:
        import path {
          inherit
            pkgs
            inputs
            config
            lib
            username
            osConfig
            system
            ;
        }
    ));

  modules = {
    workstation.enable = true;

    shell = {
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      tmux = {
        enable = true;
        sessionManager.searchPaths = [
          { path = "/etc/dotfiles";                                     name = "dotfiles"; depth = 0; }
          { path = "${config.home.homeDirectory}/.config/nvim";         name = "nvim";     depth = 0; }
          { path = "/media/procore";                                    name = "";         depth = 1; }
          { path = "${config.home.homeDirectory}/github.com";           name = "";         depth = 1; }
        ];
      };
      zsh.enable = true;
    };

    desktop = {
      skhd.enable = true;
      hammerspoon.enable = true;
      sketchybar.enable = true;
    };

    themes = {
      catppuccin.enable = true;
      gtk.enable = false;
    };

    development = {
      ai.enable = true;
      node.enable = true;
      golang.enable = true;
      rust.enable = true;
    };

    services = {
      docker.enable = true;
    };
  };
}
