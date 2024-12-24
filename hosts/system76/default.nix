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
      vm = {
        qemu.enable = true;
      };
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
    };
  };

  programs.ssh.startAgent = true;
  programs.dconf.enable = true;

  services.openssh.startWhenNeeded = true;

  networking.networkmanager.enable = true;

  # Virtualisation
  virtualisation.libvirtd.enable = true;
}
