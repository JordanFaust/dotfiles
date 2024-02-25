{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.hyprland;
    hyprland = inputs.hyprland.packages.${pkgs.system}.hyprland;
    configDir = config.dotfiles.configDir;
in {
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # Enable Hyprland
    programs.hyprland = {
      enable = true;
      package = hyprland;
      xwayland.enable = true;
    };

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
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
    };

    # Install required packages for this window manager
    environment.systemPackages = with pkgs; [
      # Required for XDG Portal Hyprland
      qt6.qtwayland
      libsForQt5.qt5.qtwayland
      # Ensure support for both qt5ct and qt6ct via kvantume
      libsForQt5.qt5ct
      qt6Packages.qtstyleplugin-kvantum

      # Greeter
      stable.libsForQt5.sddm
      stable.libsForQt5.qt5.qtgraphicaleffects
      # Greeter Themes
      catppuccin-sddm-corners
      inputs.sddm-catppuccin.packages.${pkgs.hostPlatform.system}.sddm-catppuccin
      sddm-chili-theme
      where-is-my-sddm-theme

      # Notifications
      unstable.dunst
      libnotify

      # Clipboard Utilities
      wl-clipboard
      wl-clipboard-x11
      cliphist

      # Screen Capture Utilities
      wl-gammactl
      wf-recorder
      hyprpicker
      imagemagick
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
      gnome.adwaita-icon-theme
      gnome.nautilus
      gnome.baobab
      gnome.gnome-calendar
      gnome.gnome-boxes
      gnome.gnome-system-monitor
      gnome.gnome-control-center
      gnome.gnome-weather
      gnome.gnome-calculator
      gnome.gnome-clocks
      gnome.gnome-software # for flatpak
      gnome.gnome-tweaks
    ];

    services = {
      picom.enable = true;
      redshift.enable = true;
      # Configure greetd as the greeter
      # greetd = {
      #   enable = true;
      #   settings = {
      #     default_session = {
      #       command = "${pkgs.cage}/bin/cage -s -- regreet";
      #     };
      #   };
      # };

      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "hyprland";
          sddm.enable = true;
          sddm.theme = "catppuccin";
          sddm.wayland.enable = true;
          # lightdm.enable = true;
          # lightdm.greeters.pantheon.enable = true;
        };
        # windowManager.bspwm.enable = true;
      };

      # AGS and Gnome services
      gvfs.enable = true;
      # devmon.enable = true;
      # udisks2.enable = true;
      upower.enable = true;
      # power-profiles-daemon.enable = true;
      accounts-daemon.enable = true;
      gnome = {
        # evolution-data-server.enable = true;
        glib-networking.enable = true;
        gnome-keyring.enable = true;
        # gnome-online-accounts.enable = true;
      };
    };

    # modules.theme.onReload.hyprland = ''
    #   ${pkgs.hyprland}/bin/hyprctl reload
    # '';

    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        enable = true;

        description = "polkit-gnome-authentication-agent-1";
        wantedBy = [ "graphical-session.target" ];
        wants = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };

    # # Use the Regreet greetd theme
    # programs.regreet.enable = true;
  };
}
