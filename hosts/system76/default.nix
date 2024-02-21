
{ pkgs, inputs, config, lib, home-manager, ... }:
let
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
      # bspwm.enable = true;
      cursor = {
        enable = true;
        theme = "Dracula";
      };
      apps = {
        globalprotect.enable = true;
        rofi.enable = true;
        slack.enable = true;
        zoom.enable = true;
      };
      browsers = {
        default = "firefox";
        firefox.enable = true;
      };
      media = {
        documents.enable = true;
        graphics = {
          enable = true;
          sprites.enable = false;
          models.enable = false;
        };
        mpv.enable = true;
        # Disable until ffmpeg is fixed
        recording.enable = false;
        spotify.enable = true;
      };
      term = {
        default = "kitty";
        alacritty.enable = false;
        kitty.enable = true;
        st.enable = false;
      };
      vm = {
        qemu.enable = true;
      };
    };
    dev = {
      cc.enable = true;
      clojure.enable = true;
      golang.enable = true;
      lua.enable = true;
      node.enable = true;
      ruby.enable = true;
      rust.enable = false;
      shell.enable = true;
      python.enable = true;
    };
    editors = {
      default = "nvim";
      emacs = {
        enable = false;
        doom.enable = true;
      };
      vim.enable = true;
    };
    shell = {
      # vaultwarden.enable = true;
      direnv.enable = true;
      git.enable    = true;
      gnupg.enable  = true;
      tmux.enable   = true;
      zsh.enable    = true;
    };
    services = {
      ssh.enable = true;
      docker.enable = true;
    };
    theme = {
      active = "catppuccin";
      wayland = {
        enable = true;
      };
      # wayland.enable = true;
      # xserver.enable = false;
    };
  };

  programs.ssh.startAgent = true;
  programs.dconf.enable = true;

  services.openssh.startWhenNeeded = true;

  networking.networkmanager.enable = true;

  # Virtualisation
  virtualisation.libvirtd.enable = true;
}
