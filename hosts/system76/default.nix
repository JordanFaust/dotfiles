
{ pkgs, config, lib, ... }:
{
  imports = [
    ../home.nix
    ./hardware-configuration.nix
  ];

  ## Modules
  modules = {
    desktop = {
      bspwm.enable = true;
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
        default = "alacritty";
        alacritty.enable = true;
        kitty.enable = true;
        st.enable = true;
      };
      vm = {
        qemu.enable = true;
      };
    };
    dev = {
      cc.enable = true;
      clojure.enable = true;
      golang.enable = true;
      node.enable = true;
      ruby.enable = true;
      rust.enable = true;
      shell.enable = true;
      python.enable = true;
    };
    editors = {
      default = "nvim";
      emacs = {
        enable = true;
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
    theme.active = "vilebloom";
  };

  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;

  networking.networkmanager.enable = true;
}
