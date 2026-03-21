{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.applications.neovim;
  desktop = pkgs.makeDesktopItem {
    name = "Neovim";
    desktopName = "Neovim";
    genericName = "Text Editor";
    icon = "nvim";
    exec = "uwsm app -- ${pkgs.kitty}/bin/kitty --title Neovim --class neovim --hold -e bash -c \"(tmux ls | grep -qEv 'attached|scratch' && tmux at) || tmux\"";
    categories = [
      "Utility"
      "TextEditor"
    ];
  };
in
{
  options.modules.applications.neovim = mkOption {
    description = ''
      The one and only text editor.
    '';
    type =
      with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              #
              # Neovim
              #
              enable = mkEnableOption "neovim";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  # Always enable neovim, no matter the installation
  config = mkIf cfg.enable {
    home = {
      sessionVariables = {
        EDITOR = "nvim";
        MANPAGER = "nvim -c Man!";
      };

      packages =
        with pkgs;
        [
          editorconfig-core-c
          neovim

          # Neovim Addon Dependencies
          ranger
          tree-sitter
          markdownlint-cli2
          # marksman

          # Image preview
          chafa

          # AI Integrations
          claude-code
        ]
        ++ lib.optionals pkgs.stdenv.isLinux [
          # Desktop item and code snapshot tool are Linux-only
          desktop
          codesnap
        ];
    };

    xdg.configFile = {
      # Autostart is Linux/XDG only
      "autostart/neovim.desktop" = lib.mkIf pkgs.stdenv.isLinux {
        source = "${desktop}/share/applications/Neovim.desktop";
      };

      "neovide/config.toml".source = (pkgs.formats.toml { }).generate "config.toml" {
        #
        # MonoLisa
        #
        font = {
          normal = [
            {
              family = "MonoLisa Variable";
              style = "Medium";
            }
            {
              family = "CaskaydiaCove Nerd Font";
              style = "Medium";
            }
          ];
          bold = {
            family = "MonoLisa Variable";
            style = "ExtraBold";
          };
          italic = {
            family = "MonoLisa Variable";
            style = "Italic Medium";
          };
          bold_italic = {
            family = "MonoLisa Variable";
            style = "Italic ExtraBold";
          };
          size = 16;
        };
      };
    };
  };
}
