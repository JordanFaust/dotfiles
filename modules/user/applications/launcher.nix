{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.launcher;
  minimal = config.modules.minimal;
in {
  options.modules.applications.launcher = mkOption {
    description = ''
      Configurations for application launchers, such as rofi.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              #
              # Launcher
              #
              enable = mkEnableOption "launcher";

              #
              # Rofi
              #
              rofi = {
                enable = mkEnableOption "rofi";
              };
            };
          }
        ];
      });
    default = {
      enable = true;
      rofi.enable = true;
    };
  };

  # Add configured launchers if this isn't a minimal install and launchers are enabled.
  config = mkIf (!minimal && cfg.enable) {
    home = {
      packages = with pkgs; [
        (writeScriptBin "rofi" ''
          #!${stdenv.shell}
          exec ${pkgs.rofi-wayland}/bin/rofi -terminal xst -m -1 "$@"
        '')

        # Fake rofi dmenu entries
        (makeDesktopItem {
          name = "rofi-browsermenu";
          desktopName = "Open Bookmark in Browser";
          icon = "bookmark-new-symbolic";
          exec = "${osConfig.dotfiles.binDir}/rofi/browsermenu";
        })
        (makeDesktopItem {
          name = "rofi-browsermenu-history";
          desktopName = "Open Browser History";
          icon = "accessories-clock";
          exec = "${osConfig.dotfiles.binDir}/rofi/browsermenu history";
        })
        (makeDesktopItem {
          name = "rofi-filemenu";
          desktopName = "Open Directory in Terminal";
          icon = "folder";
          exec = "${osConfig.dotfiles.binDir}/rofi/filemenu";
        })
        (makeDesktopItem {
          name = "rofi-filemenu-scratch";
          desktopName = "Open Directory in Scratch Terminal";
          icon = "folder";
          exec = "${osConfig.dotfiles.binDir}/rofi/filemenu -x";
        })

        (makeDesktopItem {
          name = "lock-display";
          desktopName = "Lock screen";
          icon = "system-lock-screen";
          exec = "${osConfig.dotfiles.binDir}/zzz";
        })
      ];
    };
  };
}
