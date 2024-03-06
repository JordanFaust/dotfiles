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
  ];

  #users.users.jordan.isNormalUser = true;

  ## Modules
  modules = {
    desktop = {
      hyprland.enable = true;
      applications = {
        vpn.enable = true;
      };
      vm = {
        qemu.enable = true;
      };
    };
    shell = {
      direnv.enable = true;
      git.enable = true;
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

    # Hardware
    hardware = {
      audio.enable = true;
      ergodox.enable = true;
      sensors.enable = true;
      amd.enable = true;
      system76.enable = true;
    };
  };

  programs.ssh.startAgent = true;
  programs.dconf.enable = true;

  services.openssh.startWhenNeeded = true;

  networking.networkmanager.enable = true;

  # Virtualisation
  virtualisation.libvirtd.enable = true;

  # CPU
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
