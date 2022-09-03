# modules/themes/vilebloom/default.nix --- a pokemon and keyboard inspired theme

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;

    # Script used to lock the computer
    lockscreen = pkgs.writeScriptBin "lockscreen" ''
      fg=152733
      wrong=ffba95
      highlight=ffba95
      date=24455b
      verify=ffba95

      # Force the displays to go to sleep
      ${pkgs.xorg.xset}/bin/xset dpms force off
      # Pause notifications until the screen is unlocked
      ${pkgs.dunst}/bin/dunstctl set-paused true
      # Lock the screen and block until unlocked
      ${pkgs.i3lock-color}/bin/i3lock-color --nofork --force-clock -i $HOME/.wallpapers/lock.jpg -e --indicator --radius=20 --ring-width=40 \
        --inside-color=$fg --ring-color=$fg --insidever-color=$verify --ringver-color=$verify \
        --insidewrong-color=$wrong --ringwrong-color=$wrong --keyhl-color=$verify --separator-color=$verify \
        --bshl-color=$verify  --date-color=$date --time-color=$date --greeter-color=$fg --wrong-color=$wrong --verif-color=$verify\
        --verif-text="Verifying Password..." --wrong-text="Wrong Password!" --noinput-text="" --greeter-text="Type the password to Unlock" \
        --time-font="JetBrainsMono Nerd Font:style=Bold" --date-font="JetBrainsMono Nerd Font" --verif-font="JetBrainsMono Nerd Font" \
        --greeter-font="JetBrainsMono Nerd Font" --wrong-font="JetBrainsMono Nerd Font" --time-str="%H:%M" --time-size=140 \
        --date-str="%a, %d %b" --date-size=45 --verif-size=23 --greeter-size=23 --wrong-size=23 \
        --ind-pos="2690:2120" --time-pos="2690:1900" --date-pos="2690:1690" --greeter-pos="2690:2290" --wrong-pos="2690:2320" --verif-pos="2690:2320" \
        --pointer=default --refresh-rate=0 --pass-media-keys --pass-volume-keys --line-uses-inside --fill
      # Turn notifactions back on
      ${pkgs.dunst}/bin/dunstctl set-paused false
    '';
in {
  config = mkIf (cfg.active == "vilebloom") (mkMerge [
    # Desktop-agnostic configuration
    {
      modules = {
        theme = {
          wallpaper = mkDefault ./config/wallpaper.jpg;
          gtk = {
            theme = "Dracula";
            iconTheme = "Papirus";
            cursorTheme = "Dracula";
          };
          fonts = {
            sans.name = "Fira Sans";
            mono.name = "JetBrains Mono";
            mono.size = 16;
          };
          colors = {
            black         = "#24455b";
            red           = "#ff777a"; #ff777a?
            green         = "#d97a9b";
            yellow        = "#ffba95";
            blue          = "#4684ae";
            magenta       = "#325f7d";
            cyan          = "#c7bcba";
            silver        = "#dfd9d8";
            grey          = "#24455b";
            brightred     = "#ff777a";
            brightgreen   = "#d97a9b";
            brightyellow  = "#ffba95";
            brightblue    = "#4684ae";
            brightmagenta = "#325f7d";
            brightcyan    = "#c7bcba";
            white         = "#ded8d7";

            types.fg      = "#ded8d7";
            types.bg      = "#152733";
            types.panelbg = "#152733";
            types.border  = "#ffba95";
          };
        };

        shell.zsh.rcFiles  = [ ./config/zsh/prompt.zsh ];
        shell.tmux.rcFiles = [ ./config/tmux.conf ];
        # desktop.browsers = {
        #   firefox.userChrome = concatMapStringsSep "\n" readFile [
        #     ./config/firefox/userChrome.css
        #   ];
        # };
      };
    }

    # Desktop (X11) theming
    (mkIf config.services.xserver.enable {
      user.packages = with pkgs; [
        unstable.dracula-theme
        # TODO replace this with papirus
        dracula-theme
        paper-icon-theme # for rofi
        papirus-icon-theme # dunst
        gnome.adwaita-icon-theme
        # Polybar Dependencies
        lsof
        zscroll
        # TODO replace pamixer with amixer
        pamixer
        alsa-utils
        playerctl
        # misc
        gpick
        neofetch
        # Dunst + EWW
        recode
        # Added utilities used in rice scripts
        moreutils
        # Lock Screen
        i3lock-color
        # User provided lockscreen script
        lockscreen
      ];
      fonts = {
        fonts = with pkgs; [
          # General Coding Fonts
          jetbrains-mono
          fira-code
          fira-code-symbols
          # General Sans Fonts
          open-sans
          siji
          # Icon Fonts
          font-awesome
          material-design-icons
        ];
      };

      # Compositor
      services.picom = {
        # Fade
        fade = true;
        fadeDelta = 4;
        fadeSteps = [ 0.03 0.03 ];
        # Opacity
        inactiveOpacity = 1.0;
        # Shadow
        shadow = true;
        shadowOffsets = [ (-25) (-25) ];
        shadowOpacity = 0.50;
        shadowExclude = [
          "! name~='(rofi|scratch|Dunst)$'"
          "window_type = 'menu'"
          "class_g = 'Plank'"
          "class_g = 'Cairo-dock'"
          "class_g = 'activate-linux'"
          "class_g = 'firefox'"
          "class_g = 'eww-background-closer'"
          "class_g = 'GLava'"
          "class_g = 'eww-visualizer'"
          "class_g = 'eww-lyrics'"
          "class_g = 'eww-volume-indicator'"
          "class_g = 'eww-brightness-indicator'"
        ];
        wintypes = {
          tooltip = { fade = true; shadow = false; opacity = 1.0; focus = true; full-shadow = false; };
          dock = { shadow = true; };
          dnd = { shadow = true; };
          popup_menu = { opacity = 1.0; };
          dropdown_menu = { opacity = 1.0; };
        };
        settings = {
          shadow-radius = 25;
          frame-opacity = 1.0;
          corner-radius = 12;

          # General Settings
          mark-wmwin-focused = true;
          mark-ovredir-focused = true;
          detect-rounded-corners = true;
          detect-client-opacity = true;
          detect-transient = true;
          use-damage = true;
          # blur-kern = "7x7box";
          # blur-strength = 320;
        };
      };

      # Login screen theme
      services.xserver.displayManager.lightdm.greeters.mini.extraConfig = ''
        text-color = "${cfg.colors.magenta}"
        password-background-color = "${cfg.colors.black}"
        window-color = "${cfg.colors.types.border}"
        border-color = "${cfg.colors.types.border}"
      '';

      # Autorandr Hooks
      #
      # The hooks here will be ran after autorandr detects a change in display configuration.
      # This can be used to ensure other systems are aware of the changes and make changes
      # or reload as needed.

      ## Global Hooks
      ##
      ## The Global hooks will be ran before/after every profile change. This is needed
      ## to make sure that the polybar instances ran on the none primary monitor are reaped
      ## when switch from the "workstation" and "travel" profiles.
      services.autorandr.hooks.preswitch = {
        "kill-polybar" = ''
          ${pkgs.procps}/bin/pkill -u $UID -x polybar
          while ${pkgs.procps}/bin/pgrep -u $UID -x polybar >/dev/null; do ${pkgs.coreutils}/bin/sleep 1; ${pkgs.procps}/bin/pkill -u $UID -x polybar; done
        '';
      };

      ## Autorander Workstation Hooks
      # services.autorandr.profiles.workstation.hooks.postswitch = builtins.readFile ./config/autorandr/workstation/postswitch.sh;
      services.autorandr.profiles.workstation.hooks.postswitch = {
        # This needs to better handle moving windows over to the workspaces on the new displays
        "notify-bspwm" = ''
          if [[ ! -z $DISPLAY ]]; then
            echo "[autorandr] profile=workstation type=postswitch hook=polybar Notifying BSPWM"
            ${pkgs.bspwm}/bin/bspc wm -r
            source $XDG_CONFIG_HOME/bspwm/bspwmrc
          fi
        '';
      };

      ##
      ## Autolock
      ##

      # Enable autolocking
      services.xserver.xautolock.enable = true;
      # Enable notifications warning about the computer being locked
      services.xserver.xautolock.enableNotifier = true;
      # Idle time (in minutes) to wait until autolock locks the computer
      services.xserver.xautolock.time = 15;
      # Time in seconds before the actual lock when the notification about the lock will be sent
      services.xserver.xautolock.notify = 30;
      # The notification sent before locking the computer
      services.xserver.xautolock.notifier = "${pkgs.dunst}/bin/dunstify -i \"gnome-lockscreen\" \"System\" \"Locking in 30 seconds\"";
      # The script to run to lock the computer
      services.xserver.xautolock.locker = "${lockscreen}/bin/lockscreen";

      # This service won't be restarted as part of a nixos-rebuild switch,
      # the process must be killed to allow the systemd unit to restart it
      # with any changes added as part of a theme.
      modules.theme.onReload.polybar = ''
        while ${pkgs.procps}/bin/pgrep -u $UID -x polybar >/dev/null; do ${pkgs.coreutils}/bin/sleep 1; ${pkgs.procps}/bin/pkill -u $UID -x polybar; done
      '';

      # Polybar systemd service
      systemd.user.services."polybar" = {
        enable = true;

        description = "Status Bar";
        documentation = [ "man:polybar(1)" ];
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];

        serviceConfig = {
          Type="forking";
          # Expand the path of the unit to include system and user packages
          # * System packages can be found within /run/current-system/sw/bin
          # * User (home-manager) packages can be found within /etc/profiles/per-user/$USER/bin
          Environment = "PATH=/run/current-system/sw/bin:/etc/profiles/per-user/${config.user.name}/bin";
          ExecStart = let scriptPkg = pkgs.writeShellScriptBin "polybar-start" ''
            echo "Starting primary monitor bar"
            polybar bar -c $XDG_CONFIG_HOME/polybar/config.ini -r &
            echo "Primary monitor bar started"
          ''; in "${scriptPkg}/bin/polybar-start";

          Restart = "always";
          RestartSec = 2;
        };
      };

      # This service won't be restarted as part of a nixos-rebuild switch,
      # the process must be killed to allow the systemd unit to restart it
      # with any changes added as part of a theme.
      modules.theme.onReload.eww = ''
        while ${pkgs.procps}/bin/pgrep -u $UID -x eww >/dev/null; do sleep 1; ${pkgs.procps}/bin/pkill -u $UID -x eww; done
      '';

      # EWW systemd  service
      systemd.user.services."eww" = {
        enable = true;

        description = "Elkowar Wacky Widgets";
        documentation = [ "man:eww(1)" ];
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];

        serviceConfig = {
          Type="forking";
          # Expand the path of the unit to include system and user packages
          # * System packages can be found within /run/current-system/sw/bin
          # * User (home-manager) packages can be found within /etc/profiles/per-user/$USER/bin
          Environment = "PATH=/run/current-system/sw/bin:/etc/profiles/per-user/${config.user.name}/bin";
          ExecStart = let scriptPkg = pkgs.writeShellScriptBin "eww-start" ''
            echo "Starting eww daemon"
            eww daemon &
            echo "Started eww daemon"
          ''; in "${scriptPkg}/bin/eww-start";

          Restart = "always";
          RestartSec = 5;
        };
      };

      home.file = {
        # Firefox configuration
        ".mozilla/firefox/jordan.default" = { source = ./config/firefox; recursive = true; };
        # General Scripts
        ".scripts" = { source = ./config/scripts; recursive = true; };
        # Wallpaper (used by i3lock)
        ".wallpapers/lock.jpg".source = ./config/vilebloom.jpg;
      };

      # Other dotfiles
      home.configFile = with config.modules; mkMerge [
        {
          # Sourced from sessionCommands in modules/themes/default.nix
          "xtheme/90-theme".source = ./config/Xresources;
        }
        (mkIf desktop.bspwm.enable {
          "bspwm/rc.d/00-theme".source = ./config/bspwmrc;
          # "bspwm/rc.d/95-polybar".source = ./config/polybar/run.sh;
        })
        (mkIf desktop.apps.rofi.enable {
          "rofi/theme" = { source = ./config/rofi; recursive = true; };
        })
        (mkIf desktop.term.alacritty.enable {
          "alacritty/alacritty.yml".text = import ./config/alacritty/alacritty.yml cfg;
        })
        (mkIf desktop.bspwm.enable {
          # Status Bar
          "polybar/config.ini".text = import ./config/polybar/config.ini { theme = cfg; pkgs = pkgs; };
          "polybar/glyphs.ini".source = ./config/polybar/glyphs.ini;
          "polybar/modules.ini".source = ./config/polybar/modules.ini;
          "polybar/scripts" = { source = ./config/polybar/scripts; recursive = true; };
          # Notifications
          "dunst/dunstrc".text = import ./config/dunstrc { theme = cfg; pkgs = pkgs; };
          # Widgets
          "eww/eww.scss".text = import ./config/eww/eww.scss cfg;
          "eww/eww.yuck".source = ./config/eww/eww.yuck;
          "eww/Main" = { source = ./config/eww/Main; recursive = true;  };
          "eww/Misc" = { source = ./config/eww/Misc; recursive = true;  };
          # "eww/Player" = { source = ./config/eww/Player; recursive = true;  };
          "eww/System-Menu" = { source = ./config/eww/System-Menu; recursive = true;  };
          # X11 Menu
          "jgmenu" = { source = ./config/jgmenu; recursive = true; };
          # GTK Theme
          "Dracula-purple-solid-kvantum" = {
            recursive = true;
            source = "${pkgs.unstable.dracula-theme}/share/themes/Dracula/kde/kvantum/Dracula-purple-solid";
            target = "Kvantum/Dracula-purple-solid";
          };
          "kvantum.kvconfig" = {
            text = "theme=Dracula-purple-solid";
            target = "Kvantum/kvantum.kvconfig";
          };
        })
        (mkIf desktop.media.graphics.vector.enable {
          "inkscape/templates/default.svg".source = ./config/inkscape/default-template.svg;
        })
      ];
    })
  ]);
}
