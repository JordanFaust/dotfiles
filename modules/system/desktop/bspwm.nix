{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.bspwm;
    configDir = config.dotfiles.configDir;
in {
  options.modules.desktop.bspwm = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.theme.onReload.bspwm = ''
      ${pkgs.bspwm}/bin/bspc wm -r
      source $XDG_CONFIG_HOME/bspwm/bspwmrc
      ${pkgs.bsp-layout}/bin/bsp-layout set even
    '';

    environment.systemPackages = with pkgs; [

      # bspwm
      (makeDesktopItem {
        name = "none+bspwm";
        desktopName = "bspwm";
        icon = "display";
        exec = "${bspwm}/bin/bspwm";
        categories = [ "System" ];
      })
      bc
      bsp-layout
      # Greeter
      unstable.lightdm
      libsForQt5.sddm
      # qtgraphicaleffects
      catppuccin-sddm-corners
      inputs.sddm-catppuccin.packages.${pkgs.hostPlatform.system}.sddm-catppuccin
      sddm-chili-theme
      where-is-my-sddm-theme
      # Notifications
      unstable.dunst
      libnotify
      # Status bar
      # (polybar.override {
      #   pulseSupport = true;
      #   nlSupport = true;
      # })
      # Desktop Menu
      jgmenu
      # # General Dependencies
      # XFCE GTK Setting Applications
      # Pull in a few GUI settings managers for general settings,
      # display, and power management.
      xfce.xfce4-settings
      (makeDesktopItem {
        name = "xfce Display Settings";
        desktopName = "xfce Display Settings";
        icon = "display";
        exec = "${xfce.xfce4-settings}/bin/xfce4-display-settings";
        categories = [ "Settings" "Accessibility" "XFCE" ];
      })
      (makeDesktopItem {
        name = "xfce Accessibility Settings";
        desktopName = "xfce Accessibility Settings";
        icon = "settings";
        exec = "${xfce.xfce4-settings}/bin/xfce4-accessibility-settings";
        categories = [ "Settings" "Accessibility" "XFCE" ];
      })
      xfce.xfce4-power-manager
      (makeDesktopItem {
        name = "xfce Power Manager";
        desktopName = "xfce Power Manager";
        icon = "settings";
        exec = "${xfce.xfce4-settings}/bin/xfce4-power-manager-settings";
        categories = [ "Settings" "Accessibility" "XFCE" ];
      })
    ];

    services = {
      picom.enable = true;
      redshift.enable = true;
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+bspwm";
          # sddm.enable = true;
          # sddm.theme = "catppuccin";
          lightdm.enable = true;
          lightdm.greeters.pantheon.enable = true;
        };
        windowManager.bspwm.enable = true;
      };
    };

    # This service won't be restarted as part of a nixos-rebuild switch,
    # the process must be killed to allow the systemd unit to restart it
    # with any changes added as part of a theme.
    modules.theme.onReload.dunst = ''
      ${pkgs.procps}/bin/pkill -u "$USER" dunst
    '';

    systemd.user.services."dunst" = {
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

    # link recursively so other modules can link files in their folders
    home.configFile = {
      "sxhkd".source = "${configDir}/sxhkd";
      "bspwm" = {
        source = "${configDir}/bspwm";
        recursive = true;
      };
    };
  };
}
