{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.hyprland;
    configdir = config.dotfiles.configdir;
in {
  options.desktop.hyprland = {
    enable = mkboolopt false;
  };

  config = mkif cfg.enable {
    # enable hyprland
    programs.hyprland = {
      enable = true;
      package = pkgs.hyprland;
      xwayland.enable = true;
    };

    xdg.portal = {
      enable = true;
      extraportals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
    };

    security = {
      polkit.enable = true;
      pam.services.ags = {};
    };

    environment.sessionvariables = {
      nixos_ozone_wl = "1";
      wlr_no_hardware_cursors = "1";
      kitty_enable_wayland = "1";
    };

    # install required packages for this window manager
    environment.systempackages = with pkgs; [
      # greeter
      libsforqt5.sddm
      libsforqt5.qt5.qtgraphicaleffects
      # greeter themes
      catppuccin-sddm-corners
      inputs.sddm-catppuccin.packages.${pkgs.hostplatform.system}.sddm-catppuccin
      sddm-chili-theme
      where-is-my-sddm-theme

      # notifications
      unstable.dunst
      libnotify

      # clipboard utilities
      wl-clipboard
      wl-clipboard-x11
      cliphist

      # screen capture utilities
      wl-gammactl
      wf-recorder
      hyprpicker
      imagemagick
      slurp

      # system control utilities
      pavucontrol
      brightnessctl
      wayshot
      swappy
      swww

      # gnome stuff
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
      # configure greetd as the greeter
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
        displaymanager = {
          defaultsession = "hyprland";
          sddm.enable = true;
          sddm.theme = "catppuccin";
          sddm.wayland.enable = true;
          # lightdm.enable = true;
          # lightdm.greeters.pantheon.enable = true;
        };
        # windowmanager.bspwm.enable = true;
      };
    };

    modules.theme.onreload.hyprland = ''
      ${pkgs.hyprland}/bin/hyprctl reload
    '';

    # this service won't be restarted as part of a nixos-rebuild switch,
    # the process must be killed to allow the systemd unit to restart it
    # with any changes added as part of a theme.
    modules.theme.onreload.dunst = ''
      ${pkgs.procps}/bin/pkill -u "$user" dunst
    '';


    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        enable = true;

        description = "polkit-gnome-authentication-agent-1";
        wantedby = [ "graphical-session.target" ];
        wants = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceconfig = {
          type = "simple";
          execstart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          restart = "on-failure";
          restartsec = 1;
          timeoutstopsec = 10;
        };
      };

      user.services.dunst = {
        enable = true;

        description = "dunst notification daemon";
        documentation = [ "man:dunst(1)" ];
        wantedby = [ "graphical-session.target" ];
        partof = [ "graphical-session.target" ];

        serviceconfig = {
          type = "dbus";
          # expand the path of the unit to include system and user packages
          # * system packages can be found within /run/current-system/sw/bin
          # * user (home-manager) packages can be found within /etc/profiles/per-user/$user/bin
          environment = "path=/run/current-system/sw/bin:/etc/profiles/per-user/${config.user.name}/bin";
          busname = "org.freedesktop.notifications";
          execstart = "${pkgs.unstable.dunst}/bin/dunst";
          restart = "always";
          restartsec = 2;
        };
      };
    };

    # # use the regreet greetd theme
    # programs.regreet.enable = true;

    # link recursively so other modules can link files in their folders
    home.configfile = {
      # "sxhkd".source = "${configdir}/sxhkd";
      "hypr" = {
        source = "${configdir}/hypr";
        recursive = true;
      };
    };
  };
}

