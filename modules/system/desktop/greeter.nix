{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop.greeter;
  # cursor = config.user-options.desktop.gtk.cursor.name;
  # conf = pkgs.writeText "config" ''
  #   exec-once = ${lib.getExe greeter}; hyprctl dispatch exit
  #   misc {
  #     disable_splash_rendering = true
  #     force_default_wallpaper = 1
  #   }
  # '';
  greetdUser = config.services.greetd.settings.default_session.user.group;
in {
  options.modules.desktop.greeter = mkOption {
    description = ''
      The greeter for users logging into the system.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "greeter";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  config = mkIf (cfg.enable) {
    user.packages = with pkgs; [
      # Script that executes the configures greetd configuration for testing
      (pkgs.writeScriptBin "greetd-test" ''
        ${config.services.greetd.settings.default_session.command}
      '')
    ];
    # # Use the Regreet greetd theme
    programs.regreet = {
      enable = true;

      settings = {
        background = {
          path = "${config.dotfiles.configDir}/themes/catppuccin/doggocat.png";
          fit = "Cover";
        };

        gtk = {
          application_prefer_dark_theme = true;
          cursor_theme_name = config.user-options.gtk.cursorTheme.name;
          font_name = config.user-options.gtk.font.name;
          icon_theme_name = config.user-options.gtk.iconTheme.name;
          theme_name = config.user-options.gtk.theme.name;
        };
      };
    };
  };
}
