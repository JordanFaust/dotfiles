{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.hyprland;
    configDir = config.dotfiles.configDir;
in {
  options.modules.desktop.hyprland = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # Enable Hyprland
    programs.hyprland = {
      enable = true;
      package = pkgs.hyprland;
      xwayland.enable = true;

      # Optional, hint electron apps to use wayland:
      enableNvidiaPatches = true;
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
      KITTY_ENABLE_WAYLAND = "1";
    };

    # Install required packages for this window manager
    environment.systemPackages = with pkgs; [
      # Greeter
      libsForQt5.sddm
      libsForQt5.qt5.qtgraphicaleffects
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

      # System Control Utilities
      pavucontrol
      brightnessctl
      wayshot
      swappy
      swww

      # Gnome stuff
      gnome.gnome-calendar
      gnome.gnome-boxes
      gnome.gnome-system-monitor
      gnome.gnome-control-center
      gnome.gnome-weather
      gnome.gnome-calculator
      gnome.gnome-clocks
      gnome.gnome-software # for flatpak
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
    };

    modules.theme.onReload.hyprland = ''
      ${pkgs.hyprland}/bin/hyprctl reload
    '';

    # This service won't be restarted as part of a nixos-rebuild switch,
    # the process must be killed to allow the systemd unit to restart it
    # with any changes added as part of a theme.
    modules.theme.onReload.dunst = ''
      ${pkgs.procps}/bin/pkill -u "$USER" dunst
    '';


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

      user.services.dunst = {
        enable = true;

        description = "Dunst notification daemon";
        documentation = [ "man:dunst(1)" ];
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];

        serviceConfig = {
          Type = "dbus";
          # Expand the path of the unit to include system and user packages
          # * System packages can be found within /run/current-system/sw/bin
          # * User (home-manager) packages can be found within /etc/profiles/per-user/$USER/bin
          Environment = "PATH=/run/current-system/sw/bin:/etc/profiles/per-user/${config.user.name}/bin";
          BusName = "org.freedesktop.Notifications";
          ExecStart = "${pkgs.unstable.dunst}/bin/dunst";
          Restart = "always";
          RestartSec = 2;
        };
      };
    };

    # # Use the Regreet greetd theme
    # programs.regreet.enable = true;

    # link recursively so other modules can link files in their folders
    home.configFile = {
      # "sxhkd".source = "${configDir}/sxhkd";
      "hypr" = {
        source = "${configDir}/hypr";
        recursive = true;
      };
    };
  };
}
