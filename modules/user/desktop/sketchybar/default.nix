# modules/user/desktop/sketchybar/default.nix
#
# Sketchybar status bar — macOS only. Replaces the native menu bar.
# Managed via home-manager's programs.sketchybar module which installs the
# binary, writes config to ~/.config/sketchybar/, and registers a LaunchAgent.
#
# Requires: native menu bar hidden via system.defaults.NSGlobalDomain._HIHideMenuBar
# Workspaces are native macOS desktops (sketchybar `space` items); see
# modules/user/desktop/spaces.nix for the desktop hotkeys/app pinning.
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.sketchybar;
in {
  options.modules.desktop.sketchybar = mkOption {
    description = "Sketchybar status bar (macOS only)";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "sketchybar";
          }
        ];
      });
    default = {};
  };

  config = mkIf (cfg.enable && pkgs.stdenv.hostPlatform.isDarwin) {
    programs.sketchybar = {
      enable = true;

      # jq: used in future plugin expansions (media, etc.)
      # networksetup and pmset are system binaries — included via includeSystemPath
      extraPackages = with pkgs; [jq];

      # includeSystemPath = true (default) adds /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/homebrew/bin
      # This ensures networksetup, pmset, osascript are all reachable from plugin scripts.

      config = {
        source = ./config;
        recursive = true;
      };

      service = {
        enable = true;
        # Logs land in ~/Library/Logs/sketchybar/ — check here when debugging startup
      };
    };
  };
}
