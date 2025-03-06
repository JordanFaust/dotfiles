{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.neovim;
  desktop = pkgs.makeDesktopItem {
    name = "Neovim";
    desktopName = "Neovim";
    genericName = "Text Editor";
    icon = "nvim";
    exec = "uwsm app -- ${pkgs.kitty}/bin/kitty --title Neovim --class neovim --hold zsh -c \"nvim %F\"";
    categories = ["Utility" "TextEditor"];
  };
in {
  options.modules.applications.neovim = mkOption {
    description = ''
      The one and only text editor.
    '';
    type = with lib.types;
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

      packages = with pkgs; [
        editorconfig-core-c
        deno
        # deno-webkit
        # neovim-nightly
        neovim
        # Disabled until https://github.com/neovide/neovide/issues/2491
        # neovide

        # Neovim Addon Dependencies
        ranger

        # Image preview
        chafa

        # Peek.nvim pre-built
        # my.peek-nvim

        # Code Snippet Image Generator
        codesnap

        # desktop
        desktop
      ];
    };

    xdg.configFile = {
      # Add Neovim as a startup application
      "autostart/neovim.desktop".source = "${desktop}/share/applications/Neovim.desktop";

      "neovide/config.toml".source = (pkgs.formats.toml {}).generate "config.toml" {
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
