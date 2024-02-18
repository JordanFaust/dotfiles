# modules/themes/catppuccin/default.nix --- a pokemon and keyboard inspired theme

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

      # Pause notifications until the screen is unlocked
      ${pkgs.dunst}/bin/dunstctl set-paused true
      # Force the displays to go to sleep
      ${pkgs.xorg.xset}/bin/xset dpms force off
      # Lock the screen and block until unlocked
      ${pkgs.i3lock-color}/bin/i3lock-color --nofork --force-clock -i $XDG_DATA_HOME/wallpaper -e --indicator --radius=20 --ring-width=40 \
        --inside-color=$fg --ring-color=$fg --insidever-color=$verify --ringver-color=$verify \
        --insidewrong-color=$wrong --ringwrong-color=$wrong --keyhl-color=$verify --separator-color=$verify \
        --bshl-color=$verify  --date-color=$date --time-color=$date --greeter-color=$fg --wrong-color=$wrong --verif-color=$verify\
        --verif-text="Verifying Password..." --wrong-text="Wrong Password!" --noinput-text="" --greeter-text="Type the password to Unlock" \
        --time-font="JetBrainsMono Nerd Font:style=Bold" --date-font="JetBrainsMono Nerd Font" --verif-font="JetBrainsMono Nerd Font" \
        --greeter-font="JetBrainsMono Nerd Font" --wrong-font="JetBrainsMono Nerd Font" --time-str="%H:%M" --time-size=140 \
        --date-str="%a, %d %b" --date-size=45 --verif-size=23 --greeter-size=23 --wrong-size=23 \
        --ind-pos="2690:1060" --time-pos="2690:800" --date-pos="2690:845" --greeter-pos="2690:1145" --wrong-pos="2690:1160" --verif-pos="2690:1160" \
        --pointer=default --refresh-rate=0 --pass-media-keys --pass-volume-keys --line-uses-inside --fill
      # Turn notifactions back on
      ${pkgs.dunst}/bin/dunstctl set-paused false
    '';
in {
  config = mkIf (cfg.active == "catppuccin") (mkMerge [
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
            # mono.name = "JetBrains Mono";
            mono.name = "Cascadia Code Normal";
            mono.size = 16;
          };
          colors = {
            black         = "#24273A";
            red           = "#ED8796";
            green         = "#A6DA95";
            yellow        = "#EED49F";
            blue          = "#8AADF4";
            magenta       = "#B7BDF8";
            cyan          = "#91D7E3";
            silver        = "#dfd9d8";
            grey          = "#24455b";
            brightred     = "#ED8796";
            brightgreen   = "#A6DA95";
            brightyellow  = "#EED49F";
            brightblue    = "#8AADF4";
            brightmagenta = "#B7BDF8";
            brightcyan    = "#91D7E3";
            white         = "#ded8d7";

            types.fg      = "#ded8d7";
            types.bg      = "#24273A";
            types.panelbg = "#1E2030";
            types.border  = "#F5A97F";
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
        # Fix broken nerd fonts
        nerdfix
      ];
      fonts.packages = with pkgs; [
        # General Coding Fonts
        jetbrains-mono
        fira-code
        fira-code-symbols
        cascadia-code
        victor-mono
        # General Sans Fonts
        open-sans
        siji
        # Icon Fonts
        # font-awesome
        # material-icons
        # material-design-icons
        (nerdfonts.override { fonts = [ "CascadiaCode" ]; })
        my.nonicons
      ];

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

          rounded-corners-exclude = [
            "window_type = 'dock'"
          ];
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
            echo "[autorandr] profile=office_plus_laptop type=postswitch hook=polybar Notifying BSPWM"
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

      # # This service won't be restarted as part of a nixos-rebuild switch,
      # # the process must be killed to allow the systemd unit to restart it
      # # with any changes added as part of a theme.
      # modules.theme.onReload.polybar = ''
      #   while ${pkgs.procps}/bin/pgrep -u $UID -x polybar >/dev/null; do ${pkgs.coreutils}/bin/sleep 1; ${pkgs.procps}/bin/pkill -u $UID -x polybar; done
      # '';
      #
      # # Polybar systemd service
      # systemd.user.services."polybar" = {
      #   enable = true;
      #
      #   description = "Status Bar";
      #   documentation = [ "man:polybar(1)" ];
      #   wantedBy = [ "graphical-session.target" ];
      #   partOf = [ "graphical-session.target" ];
      #
      #   serviceConfig = {
      #     Type="forking";
      #     # Expand the path of the unit to include system and user packages
      #     # * System packages can be found within /run/current-system/sw/bin
      #     # * User (home-manager) packages can be found within /etc/profiles/per-user/$USER/bin
      #     Environment = "PATH=/run/current-system/sw/bin:/etc/profiles/per-user/${config.user.name}/bin";
      #     ExecStart = let scriptPkg = pkgs.writeShellScriptBin "polybar-start" ''
      #       echo "Starting primary monitor bar"
      #       polybar bar -c $XDG_CONFIG_HOME/polybar/config.ini -r &
      #       echo "Primary monitor bar started"
      #     ''; in "${scriptPkg}/bin/polybar-start";
      #
      #     Restart = "always";
      #     RestartSec = 2;
      #   };
      # };

      # This service won't be restarted as part of a nixos-rebuild switch,
      # the process must be killed to allow the systemd unit to restart it
      # with any changes added as part of a theme.
      # modules.theme.onReload.eww = ''
      #   while ${pkgs.procps}/bin/pgrep -u $UID -x eww >/dev/null; do sleep 1; ${pkgs.procps}/bin/pkill -u $UID -x eww; done
      #   sleep 1
      #   systemctl restart --user eww.service
      # '';

      # EWW systemd  service
      systemd.user.services."eww" = {
        enable = true;

        description = "Elkowar Wacky Widgets";
        documentation = [ "man:eww(1)" ];
        wantedBy = [ "default.target" ];
        partOf = [ "home-manager-jordan.service" ];
        # wantedBy = [ "graphical-session.target" ];
        # partOf = [ "graphical-session.target" ];

        serviceConfig = {
          Type="forking";
          # Expand the path of the unit to include system and user packages
          # * System packages can be found within /run/current-system/sw/bin
          # * User (home-manager) packages can be found within /etc/profiles/per-user/$USER/bin
          Environment = "PATH=/run/current-system/sw/bin:/etc/profiles/per-user/${config.user.name}/bin";
          ExecStart = let scriptPkg = pkgs.writeShellScriptBin "eww-start" ''
            echo "Starting eww daemon"
            eww --debug daemon --logs &
            echo "Started eww daemon"
            sleep 1
            eww open bar
          ''; in "${scriptPkg}/bin/eww-start";
          ExecStop = "${pkgs.procps}/bin/pkill eww";

          Restart = "always";
          RestartSec = 5;
        };
      };

      # systemd.user.paths."eww".pathConfig = {
      #   PathChanges = "$HOME/config/eww";
      #   Unit = "eww.service";
      # };
      #
      # # This service won't be restarted as part of a nixos-rebuild switch,
      # # the process must be killed to allow the systemd unit to restart it
      # # with any changes added as part of a theme.
      # modules.theme.onReload.spotify_cover_art = ''
      #   systemctl restart --user spotify_cover_art.service
      # '';

      # EWW Spotify Cover Art
      systemd.user.services."spotify_cover_art" = {
        enable = true;

        description = "Spotify Cover Art Daemon";
        documentation = [ "man:eww(1)" ];
        partOf = [ "home-manager-jordan.service" ];

        serviceConfig = {
          Type="forking";
          # Expand the path of the unit to include system and user packages
          # * System packages can be found within /run/current-system/sw/bin
          # * User (home-manager) packages can be found within /etc/profiles/per-user/$USER/bin
          Environment = "PATH=/run/current-system/sw/bin:/etc/profiles/per-user/${config.user.name}/bin";
          ExecStart = let scriptPkg = pkgs.writeShellScriptBin "spotify_cover_art_daemon" ''
            TMP_DIR="/tmp/eww/playerctl"
            PLAYERS="spotify,%any,firefox,chromium,brave,mpd"
            daemon_start() {
              local song_art
              local album
              local artist
              local cover_path
              local temp_path
              local gradient_direction
              local gradient

              gradient_direction="gradient:direction=east"
              gradient="gradient:rgba(16,20,21,1.0)-rgba(16,20,21,0.5)"
              temp_path="/tmp/eww/playerctl/temp.png"

              mkdir -p $TMP_DIR

              # Follow changes to the art url of the player. Process and download cover art that has not already been
              # downloaded.
              while read -r line
              do

                if [[ "$line" == "No players found" ]]; then
                  continue
                fi

                song_art=$line
                album=$(playerctl -p $PLAYERS metadata --format '{{ album }}')
                artist=$(playerctl -p $PLAYERS metadata --format '{{ artist }}')
                echo "processing album art, album: $album, artist: $artist"
                cover_path="$TMP_DIR/$artist - $album.png"


                if [[ -f "$cover_path" ]]; then
                  echo "cover art already exists: $cover_path"
                  continue
                fi

                echo "downloading art to $temp_path"
                curl -s "$song_art" --output "$temp_path"

                echo "resizing image"
                convert "$temp_path" -resize 390x390\> "$temp_path"

                echo "applying gradient"
                convert "$temp_path" -size 390x390 -define "$gradient_direction" "$gradient" -compose multiply -composite "$cover_path"

                echo "done!"
              done < <(playerctl -p "$PLAYERS" metadata mpris:artUrl -F)
            }

            daemon_start &
          ''; in "${scriptPkg}/bin/spotify_cover_art_daemon";

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
        (mkIf desktop.term.kitty.enable {
          "kitty" = { source = ./config/kitty; recursive = true; };
          "kitty/themes/monokai-pro.conf".source = ./config/kitty/themes/monokai-pro.conf;
          "kitty/themes/catppuccin-macchiato.conf".source = ./config/kitty/themes/catppuccin-macchiato.conf;
        })
        (mkIf (desktop.bspwm.enable || desktop.hyprland.enable) {
          # Status Bar
          # "polybar/config.ini".text = import ./config/polybar/config.ini { theme = cfg; pkgs = pkgs; };
          # "polybar/glyphs.ini".source = ./config/polybar/glyphs.ini;
          # "polybar/modules.ini".source = ./config/polybar/modules.ini;
          # "polybar/scripts" = { source = ./config/polybar/scripts; recursive = true; };
          # Notifications
          "dunst/dunstrc".text = import ./config/dunstrc { theme = cfg; pkgs = pkgs; };
          # Widgets
          "eww/eww.scss".source = ./config/eww/eww.scss;
          "eww/eww.yuck".source = ./config/eww/eww.yuck;
          "eww/assets" = { source = ./config/eww/assets; recursive = true;  };
          "eww/scripts" = { source = ./config/eww/scripts; recursive = true;  };
          "eww/src" = { source = ./config/eww/src; recursive = true;  };
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

