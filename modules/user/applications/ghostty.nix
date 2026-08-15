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

  config = mkIf cfg.enable (
    let
      # Only constructed on Linux — pkgs.ghostty is Linux-only in nixpkgs.
      # On macOS the binary arrives via homebrew.casks = ["ghostty"].
      desktop =
        if pkgs.stdenv.hostPlatform.isLinux
        then
          pkgs.makeDesktopItem {
            name = "Ghostty";
            desktopName = "Ghostty";
            genericName = "Terminal emulator";
            icon = "utilities-terminal";
            exec = "uwsm app -- ${pkgs.ghostty}/bin/ghostty";
            categories = ["Development" "System" "Utility"];
          }
        else null;
    in {
      home = {
        packages =
          # Linux: custom .desktop wrapper + package.
          # macOS: managed by homebrew — see modules/darwin/homebrew.nix.
          lib.optionals pkgs.stdenv.hostPlatform.isLinux [desktop pkgs.ghostty];

        sessionVariables = {
          # TERMINAL is intentionally omitted during the kitty→ghostty transition.
          # Both modules are active simultaneously; kitty.nix owns TERMINAL = "kitty".
          # Once kitty.nix is removed, add: TERMINAL = "ghostty";
          #
          # TERM is not set globally — Ghostty sets it when spawning a shell.
          # Ghostty bundles its own terminfo (xterm-ghostty); no TERMINFO_DIRS needed.
        };
      };

      # Desktop item autostart — Linux/XDG only
      xdg.configFile = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        "autostart/ghostty.desktop".source = "${desktop}/share/applications/Ghostty.desktop";
      };

      programs.ghostty = {
        enable = true;
        enableZshIntegration = false;
        # pkgs.ghostty is Linux-only in nixpkgs. On macOS the binary is installed
        # via homebrew.casks = ["ghostty"] in modules/darwin/homebrew.nix.
        # Setting package = null suppresses HM's internal pkgs.ghostty references
        # (package install, onChange validator, bat/vim syntax helpers).
        package =
          if pkgs.stdenv.hostPlatform.isLinux
          then pkgs.ghostty
          else null;
        settings =
          {
            font-family = "MonoLisa Variable";
            font-style = "SemiBold";
            font-style-italic = "SemiBold Italic";
            font-size = 18;
            # kitty adjust_line_height 150% → Ghostty counts the delta above 100%
            adjust-cell-height = "50%";
            cursor-style = "block";
            shell-integration = "detect";
            background-opacity = 1.0;
            copy-on-select = false;
            term = "xterm-ghostty";
          }
          // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
            # macOS-specific settings
            macos-option-as-alt = true;
            macos-titlebar-style = "hidden";
          };
      };
    }
  );
}
