{
  inputs,
  config,
  lib,
  ...
}: {
  imports = [
    # "${inputs.nixpkgs}/nixos/modules/profiles/hardened.nix"
    ../local.nix
    ./hardware-configuration.nix
  ];

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
  # Disable NetworkManager's internal DNS resolution
  networking.networkmanager.dns = "none";

  # These options are unnecessary when managing DNS ourselves
  networking.useDHCP = false;
  networking.dhcpcd.enable = false;

  # Configure DNS servers manually (this example uses Cloudflare and Google DNS)
  # IPv6 DNS servers can be used here as well.
  networking.nameservers = [
    "1.1.1.1"
    "1.0.0.1"
    "8.8.8.8"
    "8.8.4.4"
  ];

  # Virtualisation
  virtualisation.libvirtd.enable = true;

  # CPU
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
