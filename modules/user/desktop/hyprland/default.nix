{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.hyprland;

  yt = pkgs.writeShellScript "yt" ''
    notify-send "Opening video" "$(wl-paste)"
    mpv "$(wl-paste)"
  '';

  playerctl = "${pkgs.playerctl}/bin/playerctl";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  pactl = "${pkgs.pulseaudio}/bin/pactl";

  gtkTheme = config.modules.desktop.gtk.name;

  cursor = {
    name = config.modules.desktop.gtk.cursor.name;
    size = config.modules.desktop.gtk.cursor.size;
  };
in {
  options.modules.desktop.hyprland = mkOption {
    description = ''
      Enable the Hyprland Window Manager
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "hyprland";
              hyprlock = {
                enable = mkEnableOption "hyprlock";
              };
            };
          }
        ];
      });
    default = {
      # This is gated behind hyprland, default to enabling if hyprland is enabled
      hyprlock.enable = true;
    };
  };

  config = lib.mkIf (cfg.enable) {
    # Disabled, along with most gnome dependencies to reduce install size and CVEs
    # xdg.desktopEntries."org.gnome.Settings" = {
    #   name = "Settings";
    #   comment = "Gnome Control Center";
    #   icon = "org.gnome.Settings";
    #   exec = "env XDG_CURRENT_DESKTOP=gnome ${pkgs.gnome-control-center}/bin/gnome-control-center";
    #   categories = ["X-Preferences"];
    #   terminal = false;
    # };

    home.packages = with pkgs; [
      hyprpanel
    ];

    xdg.configFile = {
      "xdg-desktop-portal/hyprland-portals.conf".source = (pkgs.formats.ini {}).generate "hyprland-portals.conf" {
        preferred = {
          default = "hyprland;gtk";
          "org.freedesktop.impl.portal.FileChooser" = "kde";
        };
      };

      # Set relevant environment variables outside of the hyprland.conf directory for
      # theming, xcursor, nvidia and general toolkit variables.
      # See https://wiki.hyprland.org/Configuring/Environment-variables/
      "uwsm/env" = {
        text = ''
          #!/usr/bin/env bash
          export UWSM_FINALIZE_VARNAMES="''${UWSM_FINALIZE_VARNAMES} WAYLAND_DISPLAY"
          export GTK_THEME=${gtkTheme}
          export XCURSOR_THEME=${builtins.toString cursor.name}
          export XCURSOR_SIZE=${builtins.toString cursor.size}
        '';
      };

      # Set relevant environment variables for hyprland and aquamarine
      # See https://wiki.hyprland.org/Configuring/Environment-variables/
      "uwsm/env-hyprland" = {
        text = ''
          #!/usr/bin/env bash
          export HYPRCURSOR_THEME=${builtins.toString cursor.name}
          export HYPRCURSOR_SIZE=${builtins.toString cursor.size}
        '';
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = false;
      xwayland.enable = false;

      settings = {
        exec-once = [
          "uwsm app -- ${pkgs.swww}/bin/swww-daemon"
          "uwsm app -- ${pkgs.hyprpanel}/bin/hyprpanel"
          "uwsm app -- wl-paste --type text --watch cliphist store"
          "uwsm app -- wl-paste --type image --watch cliphist store"
          "uwsm app -- hyprctl setcursor ${cursor.name} ${builtins.toString cursor.size}"
        ];

        monitor = [
          "HDMI-A-1,3840x1600@60,0x0,1"
          ",preferred,auto,1"
        ];

        general = {
          layout = "dwindle";
          resize_on_border = true;
          gaps_in = 14;
          gaps_out = 28;
          border_size = 3;
          "col.active_border" = "rgb(EED49F) rgb(EED49F)";
          "col.inactive_border" = "0xff24455b";
        };

        misc = {
          disable_splash_rendering = true;
          force_default_wallpaper = 1;
        };

        input = {
          follow_mouse = 1;
          touchpad = {
            natural_scroll = "yes";
            disable_while_typing = true;
            drag_lock = true;
          };
          sensitivity = 0;
          float_switch_override_focus = 2;
        };

        binds = {
          allow_workspace_cycles = true;
        };

        dwindle = {
          pseudotile = "yes";
          preserve_split = "yes";
          # no_gaps_when_only = 0;
        };

        gestures = {
          workspace_swipe = true;
          workspace_swipe_forever = true;
          # workspace_swipe_numbered = true;
        };

        workspace = [
          "1, monitor:HDMI-A-1, default:true"
          "2, monitor:HDMI-A-1"
          "3, monitor:HDMI-A-1"
          "4, monitor:HDMI-A-1"
          "5, monitor:HDMI-A-1"
        ];

        windowrule = let
          fregex = regex: "float, ^(${regex})$";
        in [
          # (fregex "org.gnome.Settings")
          # (fregex "org.gnome.design.Palette")
          # (fregex "Color Picker")
          # (fregex "xdg-desktop-portal")
          # (fregex "xdg-desktop-portal-gnome")
          # (fregex "com.github.Aylur.ags")
          "workspace stayfocused, title:MainPicker"
        ];

        windowrulev2 = let
          f = title: class: "float, title:^(${title})$, class:^(${class})$";
          pin = title: class: "pin, title:^(${title})$ class:^(${class})$";
          inhibitfocus = regex: "idleinhibit focus,title:^(${regex})$";
        in [
          # Make sure that the zoom toolbar and video window are floating
          (f "zoom_linux_float_video_window" "zoom")
          (f "zoom_linux_float_message_reminder" "zoom")
          (f "as_toolbar" "zoom")
          (f "app.zoom.us is sharing your screen." "")
          (f "GlobalProtect Login" "gpauth")
          # Pin the floating video window and make it follow the current workspace
          (pin "zoom_linux_float_video_window" "zoom")
          # Pin the floating message reminder and make it follow the current workspace
          # Zoom should be smart enough to make sure they don't stack
          (pin "zoom_linux_float_message_reminder" "zoom")
          # First move the toolbar so it doesn't disappear behind AGS bar
          # Pin the floating toolbar and make it follow the current workspace
          "move 1498 58, title:^(as_toolbar)$, class:^(zoom)$"
          (pin "as_toolbar" "zoom")
          # Inhibit Screen Locking/Sleeping during video calls/watching videos
          (inhibitfocus "Zoom Meeting")

          # Autostart workspace placement
          "workspace 2 silent, class:neovim"
          "workspace 2 silent, class:firefox"
          "workspace 3 silent, class:Slack"
          "workspace 3 silent, class:spotify"
          "workspace 4 silent, class:chromium-browser"
        ];

        bind = let
          binding = mod: cmd: key: arg: "${mod}, ${key}, ${cmd}, ${arg}";
          mvfocus = binding "SUPER" "movefocus";
          ws = binding "SUPER" "workspace";
          resizeactive = binding "SUPER ALT" "resizeactive";
          mvactive = binding "SUPER SHIFT" "movewindow";
          mvtows = binding "SUPER CONTROL" "movetoworkspace";
          arr = [1 2 3 4 5];
        in
          [
            ",Home, exec, uwsm app -- grim -g \"$(slurp)\""
            "Super, Home, exec, uwsm app -- grim -g \"$(slurp -d)\" - | wl-copy"

            # Launch Applicaitons Shortcuts
            "SUPER, Return, exec, uwsm app -- kitty -e bash -c \"(tmux ls | grep -qEv 'attached|scratch' && tmux at) || tmux\""
            "SUPER, Space, exec, $DOTFILES_BIN/rofi/appmenu"

            # Copy History Menu
            "SUPER, V, exec, $DOTFILES_BIN/rofi/copyhistmenu"

            # youtube
            ", XF86Launch1,  exec, ${yt}"

            "ALT, Tab, focuscurrentorlast"
            "SUPER, Q, killactive"
            "SUPERSHIFT, F, fullscreen, 0"
            "SUPER, F, fullscreen, 1"
            "SUPER, P, togglesplit"

            (mvfocus "k" "u")
            (mvfocus "j" "d")
            (mvfocus "l" "r")
            (mvfocus "h" "l")
            (ws "left" "e-1")
            (ws "right" "e+1")
            (mvtows "left" "e-1")
            (mvtows "right" "e+1")
            (resizeactive "k" "0 -20")
            (resizeactive "j" "0 20")
            (resizeactive "l" "20 0")
            (resizeactive "h" "-20 0")
            (mvactive "k" "u")
            (mvactive "j" "d")
            (mvactive "l" "r")
            (mvactive "h" "l")
          ]
          ++ (map (i: ws (toString i) (toString i)) arr)
          ++ (map (i: mvtows (toString i) (toString i)) arr);

        bindle = [
          ",XF86MonBrightnessUp,   exec, ${brightnessctl} set +5%"
          ",XF86MonBrightnessDown, exec, ${brightnessctl} set  5%-"
          ",XF86KbdBrightnessUp,   exec, ${brightnessctl} -d asus::kbd_backlight set +1"
          ",XF86KbdBrightnessDown, exec, ${brightnessctl} -d asus::kbd_backlight set  1-"
          ",XF86AudioRaiseVolume,  exec, ${pactl} set-sink-volume @DEFAULT_SINK@ +5%"
          ",XF86AudioLowerVolume,  exec, ${pactl} set-sink-volume @DEFAULT_SINK@ -5%"
        ];

        bindl = [
          ",XF86AudioPlay,    exec, ${playerctl} play-pause"
          ",XF86AudioStop,    exec, ${playerctl} pause"
          ",XF86AudioPause,   exec, ${playerctl} pause"
          ",XF86AudioPrev,    exec, ${playerctl} previous"
          ",XF86AudioNext,    exec, ${playerctl} next"
          ",XF86AudioMicMute, exec, ${pactl} set-source-mute @DEFAULT_SOURCE@ toggle"
        ];

        bindm = [
          "CTRL, mouse:273, resizewindow"
          "CTRL, mouse:272, movewindow"
        ];

        decoration = {
          shadow = {
            enabled = true;
            range = 100;
            render_power = 5;
            color = "0x33000000";
            color_inactive = "0x22000000";
          };
          rounding = 5;
          blur = {
            enabled = true;
            size = 9;
            passes = 2;
            new_optimizations = "on";
            contrast = 1;
            brightness = 0.59;
          };
        };

        animations = {
          enabled = "yes";
          bezier = "overshot,0.13,0.99,0.29,1.1";
          animation = [
            "windows,1,4,overshot,slide"
            "windows,1,4,overshot,slide"
            "border,1,10,default"
            "fade,1,10,default"
            "workspaces,1,6,overshot,slide"
          ];
        };
      };
    };
  };
}
