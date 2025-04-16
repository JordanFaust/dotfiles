{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.hyprland;
in {
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nix.settings = {
      substituters = ["https://hyprland.cachix.org"];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      ];
    };
    # Enable Hyprland
    programs.hyprland = {
      enable = true;

      xwayland.enable = false;
      withUWSM = true;
    };

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
      config.common.default = "*";
    };

    security = {
      polkit.enable = true;
      pam.services.ags = {};
    };

    environment.sessionVariables = {
      NIXOS_OZONE_WL = "1";
      WLR_NO_HARDWARE_CURSORS = "1";
      WLR_DRM_NO_ATOMIC = "1";
      KITTY_ENABLE_WAYLAND = "1";
      MOZ_ENABLE_WAYLAND = "1";
      QT_QPA_PLATFORM = "wayland";
      CHROME_WRAPPER = "1";
      CHROME_EXTRA_ARGS = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
      GDK_BACKEND = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    };

    # Install required packages for this window manager
    environment.systemPackages = with pkgs; [
      # Required for XDG Portal Hyprland
      # Uncomment once hyprland-picker incompatibility is fixed
      kdePackages.qtwayland
      kdePackages.qt6ct
      # (builtins.getFlake "github:NixOS/nixpkgs/a3ed7406349a9335cb4c2a71369b697cecd9d351").legacyPackages.${pkgs.system}.kdePackages.qtwayland
      qt6Packages.qtstyleplugin-kvantum
      # libsForQt5.qt5.qtwayland
      # Ensure support for both qt5ct and qt6ct via kvantume
      # libsForQt5.qt5ct

      # Notifications
      libnotify

      # Clipboard Utilities
      wl-clipboard
      wl-clipboard-x11
      cliphist

      # Screen Capture Utilities
      wl-gammactl
      wf-recorder
      hyprpicker
      slurp
      grim

      # System Control Utilities
      pavucontrol
      brightnessctl
      wayshot
      swappy
      swww

      # Gnome stuff
      loupe
      # adwaita-icon-theme
      nautilus
      baobab
      # gnome-calendar
      # gnome-boxes
      # gnome-system-monitor
      # gnome-control-center
      # gnome-weather
      # gnome-calculator
      # gnome-clocks
      # gnome-software # for flatpak
      gnome-tweaks
      # cage
    ];

    services = {
      displayManager = {
        defaultSession = "hyprland";
      };

      # AGS and Gnome services
      gvfs.enable = true;
      upower.enable = false;
      accounts-daemon.enable = true;
      gnome = {
        glib-networking.enable = true;
        gnome-keyring.enable = true;
      };
    };

    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        enable = true;

        description = "polkit-gnome-authentication-agent-1";
        wantedBy = ["graphical-session.target"];
        wants = ["graphical-session.target"];
        after = ["graphical-session.target"];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };
  };
}
