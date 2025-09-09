{
  config,
  lib,
  ...
}:
let
  cfg = config.modules.desktop.hyprland;
in {
  options = {};

  config = lib.mkIf (cfg.enable) {
    # We are manually apply this theme
    catppuccin.hyprlock.enable = false;
    programs.hyprlock = {
      enable = true;
      settings = {
        # General Configuraiton
        general = {
          # grace = 1;
          ignore_empty_input = true;
          text_trim = false;
          # disable_loading_bar = true;
        };

        # Background Configuration
        background = [
          {
            # Target a specific monitor, not needed for single monitor setups.
            # monitor = "";
            path = "${config.xdg.configHome}/lockscreen";
            color = "rgb(ED8796)";

            # Blur Configurations (https://wiki.hyprland.org/Configuring/Variables/#blur)
            blur_size = 4;
            blur_passes = 3; # 0 disables blurring
            noise = 0.0117;
            contrast = 1.3000; # Vibrant!!!
            brightness = 0.8000;
            vibrancy = 0.2100;
            vibrancy_darkness = 0.0;
            zindex = -1;
          }
        ];

        # Input Field(s) Configuration
        input-field = [
          {
            # monitor = "";
            size = "350, 70";
            outline_thickness = 4;
            dots_size = 0.2; # Scale of input-field height, 0.2 - 0.8
            dots_spacing = 0.64; # Scale of dots' absolute size, 0.0 - 1.0
            dots_center = true;
            outer_color = "rgb(ED8796)";
            inner_color = "rgb(24273A)";
            font_color = "rgb(ded8d7)";
            fade_on_empty = false;
            font_family = "MonoLisa Variable Regular";
            placeholder_text = "<i><span foreground='##ded8d7'>Input Password...</span></i>";
            hide_input = false;
            position = "0, -600";
            halign = "center";
            valign = "center";
            zindex = 10;
          }
        ];

        shape = [
          # Big Rectangle
          {
            size = "100%, 80";
            color = "rgb(24273A)";
            halign = "center";
            valign = "bottom";
            zindex = 0;
          }

          # Small Rectangle for Session
          {
            size = "250, 52";
            rounding = "10";
            color = "rgb(ED8796)";
            halign = "center";
            valign = "bottom";
            position = "0, 14";
            zindex = 1;
          }
        ];

        image = [
          # PFP Image
          {
              path = "$HOME/.face";
              size = "140";
              rounding = "-1";
              border_size = "3";
              border_color = "rgba(ded8d7FF)";
              position = "10, 10";
              halign = "left";
              valign = "bottom";
              zindex = "3";
          }
        ];

        label = [

         # Lock Icon
          {
              text = " ";
              shadow_passes = "1";
              shadow_boost = "0.5";
              color = "rgba(ded8d7FF)";
              shadow_color = "rgba(ded8d7AA)";
              font_size = "20";
              font_family = "Font Awesome 6 Free Solid";
              position = "0, -140";
              halign = "center";
              valign = "top";
              zindex = "2";
          }
          # Current Time
          {
            # monitor = "";
            text = "$TIME";
            color = "rgb(ded8d7)";
            font_size = 120;
            shadow_passes = 3;
            shadow_boost = 0.5;
            font_family = "MonoLisa Variable Bold";
            position = "0, -200";
            halign = "center";
            valign = "top";
            zindex = 3;
          }
          # Current Session Status
          {
            text = "cmd[update:24000000] echo \"Session: \$XDG_SESSION_DESKTOP\"";
            color = "rgba(24273AFF)";
            shadow_color = "rgba(24273AAA)";
            font_size = 16;
            font_family = "MonoLisa Variable Medium";
            position = "0, -4";
            halign = "center";
            valign = "bottom";
            zindex = 2;
          }

          # Username
          {
            # monitor = "";
            # text = "Hey <span text_transform=\"capitalize\" size=\"larger\">$USER</span>";
            text = "$USER";
            shadow_passes = 1;
            shadow_boost = 0.5;
            color = "rgba(ED8796BB)";
            shadow_color = "rgba(ED8796AA)";
            font_size = 20;
            font_family = "MonoLisa Variable Italic Medium";
            position = "160, 40";
            halign = "left";
            valign = "bottom";
            zindex = 2;
          }
          # Hostname
          {
            text = "cmd[update:24000000] echo \"@\$(uname -n)\"";
            shadow_passes = 1;
            shadow_boost = 0.5;
            color = "rgba(ded8d7BB)";
            shadow_color = "rgba(ded8d7AA)";
            font_size = 18;
            font_family = "MonoLisa Variable Italic";
            position = "160, -22";
            halign = "left";
            valign = "bottom";
            zindex = 2;
          }
        ];
      };
    };
  };
}


