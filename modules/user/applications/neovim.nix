{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.neovim;
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
      };

      packages = with pkgs; [
        editorconfig-core-c
        # unstable.neovim
        deno
        # deno-webkit
        # neovim-nightly
        neovim
        neovide

        # Neovim Addon Dependencies
        ranger

        # Image preview
        chafa

        # Peek.nvim pre-built
        # my.peek-nvim

        # (makeDesktopItem {
        #   name = "Neovim";
        #   desktopName = "Neovim";
        #   genericName = "Text Editor";
        #   icon = "nvim";
        #   # KITTY_ENABLE_WAYLAND must be set here or integrations with wayland, such as copy/paste, won't work
        #   exec = "bash -c \"KITTY_ENABLE_WAYLAND=1; ${kitty}/bin/kitty --title Neovim --class neovim -e nvim %F\"";
        #   categories = ["Utility" "TextEditor"];
        # })
      ];
    };

    xdg.configFile = {
      "neovide/config.toml".source = (pkgs.formats.toml {}).generate "config.toml" {
        font = {
          normal = [
            {
              family = "MonoLisa Variable";
              style = "Medium";
            }
            {
              family = "Cascadia Code";
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
          size = 14;
        };
        # font.features = {
        #   MonoLisa = [ "+ss01" ];
        # };
        # font = {
        #   normal = {
        #     family = "Cascadia Code";
        #     style = "Medium";
        #   };
        #   bold = {
        #     family = "Cascadia Code";
        #     style = "Bold";
        #   };
        #   italic = {
        #     family = "Victor Mono";
        #     style = "Italic SemiBold";
        #   };
        #   bold_italic = {
        #     family = "Victor Mono";
        #     style = "Bold Italic";
        #   };
        #   size = 14;
        # };
      };
    };
  };
}
