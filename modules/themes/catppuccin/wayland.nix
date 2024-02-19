# modules/themes/catppuccin/wayland.nix --- a soothing pastel theme

{ options, config, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;

    # catppuccin-ags = pkgs.callPackage ./ags/ags.nix {};
in {
  config = mkIf (cfg.active == "catppuccin") (mkMerge [
    # Desktop (Wayland) theming
    (mkIf cfg.wayland.enable {

      environment.sessionVariables = {
        XCURSOR_THEME = cfg.gtk.cursorTheme;
        XCURSOR_SIZE = "24";
      };

      # home-manager.users."jordan" = {
      #   gtk = {
      #     enable = true;
      #     font.name = "Cascadia Code Regular";
      #     theme = {
      #       name = cfg.gtk.theme;
      #       package = pkgs.catppuccin-gtk.override {
      #         accents = [ "pink" ];
      #         size = "compact";
      #         tweaks = [ "rimless" "black" ];
      #         variant = "macchiato";
      #       };
      #     };
      #   };
      # };

      user.packages = with pkgs; [
        (catppuccin-gtk.override {
          accents = [ "pink" ]; # You can specify multiple accents here to output multiple themes
          size = "compact";
          tweaks = [ "rimless" "black" ]; # You can also specify multiple tweaks here
          variant = "macchiato";
        })
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
        # Fix broken nerd fonts
        nerdfix

        inputs.ags.packages.${pkgs.hostPlatform.system}.ags
        my.ags.desktop.script
        my.ags.greeter.script
        dart-sass
        gtk3
        accountsservice
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

      # TODO

      # This service won't be restarted as part of a nixos-rebuild switch,
      # the process must be killed to allow the systemd unit to restart it
      # with any changes added as part of a theme.
      # modules.theme.onReload.eww = ''
      #   while ${pkgs.procps}/bin/pgrep -u $UID -x eww >/dev/null; do sleep 1; ${pkgs.procps}/bin/pkill -u $UID -x eww; done
      #   sleep 1
      #   systemctl restart --user eww.service
      # '';

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
          "ags/config.js" = {
            source = "${pkgs.my.ags.desktop.config}/config.js";
          };
        })
        (mkIf desktop.media.graphics.vector.enable {
          "inkscape/templates/default.svg".source = ./config/inkscape/default-template.svg;
        })
      ];
    })
  ]);
}

