{
  pkgs,
  inputs,
  config,
  lib,
  home-manager,
  ...
}: let
  username = "jordan";
in rec {
  imports = [
    ../local.nix
    ./hardware-configuration.nix
    # (import ./home.nix { inherit pkgs inputs config lib home-manager; })
  ];

  ## Modules
  modules = {
    desktop = {
      hyprland.enable = true;
      applications = {
        vpn.enable = true;
      };
      term = {
        default = "kitty";
        kitty.enable = true;
      };
      vm = {
        qemu.enable = true;
      };
    };
    dev = {
      golang.enable = true;
      lua.enable = true;
      node.enable = true;
      ruby.enable = true;
      rust.enable = false;
      shell.enable = true;
      python.enable = true;
    };
    shell = {
      # vaultwarden.enable = true;
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    services = {
      ssh.enable = true;
      docker.enable = true;
    };
    theme = {
      active = "catppuccin";
      wayland.enable = true;
    };
  };

  programs.ssh.startAgent = true;
  programs.dconf.enable = true;

  services.openssh.startWhenNeeded = true;

  networking.networkmanager.enable = true;

  # Virtualisation
  virtualisation.libvirtd.enable = true;
}
