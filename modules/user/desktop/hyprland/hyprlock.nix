{
  config,
  inputs,
  pkgs,
  lib,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.hyprland;
in {
  imports = [inputs.hyprlock.homeManagerModules.default];
  options = {};

  config = lib.mkIf (cfg.enable) {
    programs.hyprlock = {
      enable = true;
      # General Configuraiton
      general = {
        grace = 5;
      };

      # Background Configuration
      backgrounds = [
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
        }
      ];

      # Input Field(s) Configuration
      input-fields = [
        {
          # monitor = "";
          size = {width = 250; height = 50;};
          outline_thickness = 3;
          dots_size = 0.2; # Scale of input-field height, 0.2 - 0.8
          dots_spacing = 0.64; # Scale of dots' absolute size, 0.0 - 1.0
          dots_center = true;
          outer_color = "rgb(ED8796)";
          inner_color = "rgb(24273A)";
          font_color = "rgb(ded8d7)";
          fade_on_empty = true;
          placeholder_text = "<i>Password...</i>";
          hide_input = false;
          position = {x = 0; y = 50;};
          halign = "center";
          valign = "bottom";
        }
      ];

      labels = [
        # Current Time
        {
          # monitor = "";
          text = "<b><big>$TIME</big></b>";
          color = "rgb(ded8d7)";
          font_size = 84;
          font_family = "Cascadia Code Normal";
          position = {x = 0; y = 100;};
          halign = "center";
          valign = "center";
        }
        # User
        {
          # monitor = "";
          text = "Hey <span text_transform=\"capitalize\" size=\"larger\">$USER</span>";
          color = "rgb(ded8d7)";
          font_size = 20;
          font_family = "Cascadia Code Normal";
          position = {x = 0; y = 0;};
          halign = "center";
          valign = "center";
        }
        # Password Field
        {
          # monitor = "";
          text = "Type to unlock!";
          color = "rgb(ded8d7)";
          font_size = 16;
          font_family = "Cascadia Code Normal";
          position = {x = 0; y = 30;};
          halign = "center";
          valign = "bottom";
        }
      ];
    };
  };
}


