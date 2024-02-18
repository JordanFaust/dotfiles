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
    # (makeDesktopItem {
    #   name = "hyprland";
    #   desktopName = "hyprland";
    #   icon = "display";
    #   exec = "${bspwm}/bin/bspwm";
    #   categories = [ "System" ];
    # })
    # Enable Hyprland
    programs.hyprland.enable = true;
    programs.hyprland.enableNvidiaPatches = true;
    # Optional, hint electron apps to use wayland:
    environment.sessionVariables.NIXOS_OZONE_WL = "1";
    environment.sessionVariables.WLR_NO_HARDWARE_CURSORS = "1";

    # Install required packages for this window manager
    environment.systemPackages = with pkgs; [
      # Greeter
      libsForQt5.sddm
      libsForQt5.qt5.qtgraphicaleffects

      # qtgraphicaleffects
      catppuccin-sddm-corners
      inputs.sddm-catppuccin.packages.${pkgs.hostPlatform.system}.sddm-catppuccin
      sddm-chili-theme
      where-is-my-sddm-theme
      # cage
      # greetd.greetd
      # greetd.regreet
      # Notifications
      unstable.dunst
      libnotify
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
