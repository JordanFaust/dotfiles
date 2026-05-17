# modules/user/applications/ghostty.nix
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.ghostty;
  desktop = pkgs.makeDesktopItem {
    name = "Ghostty";
    desktopName = "Ghostty";
    genericName = "Terminal emulator";
    icon = "utilities-terminal";
    exec = "uwsm app -- ${pkgs.ghostty}/bin/ghostty";
    categories = ["Development" "System" "Utility"];
  };
in {
  options.modules.applications.ghostty = mkOption {
    description = ''
      Configuration for the Ghostty terminal.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "ghostty";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  config = mkIf cfg.enable {
    home = {
      packages =
        # Linux: custom .desktop wrapper + package.
        # macOS: binary is installed via homebrew.casks = ["ghostty"] in
        # modules/darwin/homebrew.nix — pkgs.ghostty is Linux-only in nixpkgs.
        lib.optionals pkgs.stdenv.isLinux [desktop pkgs.ghostty];

      sessionVariables = {
        # TERMINAL is intentionally omitted here. During the kitty→ghostty
        # transition, both modules are enabled simultaneously. kitty.nix owns
        # TERMINAL = "kitty" (via lib.mkDefault). Once kitty is fully removed,
        # uncomment the line below and remove it from kitty.nix.
        #
        # TERMINAL = "ghostty";
        #
        # TERM is not set globally — Ghostty sets it when it spawns a shell.
        # Ghostty bundles its own terminfo (xterm-ghostty); no TERMINFO_DIRS
        # export needed unlike kitty.
      };
    };

    # Desktop item autostart — Linux/XDG only
    xdg.configFile = lib.mkIf pkgs.stdenv.isLinux {
      "autostart/ghostty.desktop".source = "${desktop}/share/applications/Ghostty.desktop";
    };

    programs.ghostty = {
      enable = true;
      settings =
        {
          font-family = "MonoLisa Variable";
          font-size = 18;
          # kitty adjust_line_height 150% → Ghostty counts the delta above 100%
          adjust-cell-height = "50%";
          cursor-style = "block";
          shell-integration = "detect";
          background-opacity = 1.0;
          copy-on-select = false;
          term = "xterm-ghostty";
        }
        // lib.optionalAttrs pkgs.stdenv.isDarwin {
          # macOS-specific settings
          macos-option-as-alt = true;
          macos-titlebar-style = "hidden";
        };
    };
  };
}
