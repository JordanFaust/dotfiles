{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.vscode;
in {
  # imports = [
  #   ./vscode/settings.nix
  #   ./vscode/keybindings.nix
  # ];
  options.modules.applications.vscode = mkOption {
    description = ''
      The (forced) one and only text editor.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              #
              # Neovim
              #
              enable = mkEnableOption "vscode";
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
      packages = with pkgs; [
        fzf
        ripgrep
        bat
        alejandra
      ];
    };

    catppuccin = {
      enable = true;
      vscode = {
        profiles = {
          default = {
            accent = "pink";
            settings = {
              boldKeywords = true;
              italicComments = true;
              italicKeywords = true;
              colorOverrides = {};
              customUIColors = {};
              workbenchMode = "default";
              bracketMode = "rainbow";
              extraBordersEnabled = false;
            };
          };
        };
      };
    };

    # Extensions are managed manually via Cursor's marketplace.
    # Nix-managed extensions cause registration issues with Cursor's
    # headless scanner (it skips symlinks without version suffixes).
    # See: modules/user/applications/editors/vscode/EXTENSIONS.md
    programs.vscode = {
      enable = true;
      package = pkgs.code-cursor;
      mutableExtensionsDir = true;

      profiles = {
        default = {
          enableUpdateCheck = false;
          enableExtensionUpdateCheck = false;
        };
      };
    };
  };
}
