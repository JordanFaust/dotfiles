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
  imports = [
    ./vscode/settings.nix
    ./vscode/keybindings.nix
  ];
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

  # Always enable neovim, no matter the installation
  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        # fzf-picker dependnecies
        fzf
        ripgrep
        bat
      ];

    };

    catppuccin = {
      enable = true;
      # optionally configure the extension settings, defaults are shown below:
      vscode = {
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

    programs.vscode = {
      enable = true;
      # Set the package to the FHS-wrapped version of VS Code
      package = pkgs.vscode;
      mutableExtensionsDir = true;

      profiles = {
        default = {
          # enableUpdateCheck = false;
          # enableExtensionUpdateCheck = false;

          extensions = with pkgs.vscode-extensions;
            [
              # Theme
              catppuccin.catppuccin-vsc
              catppuccin.catppuccin-vsc-icons
              # Neovim
              asvetliakov.vscode-neovim
              # Search Extensions
              # Extensions
              vspacecode.whichkey
              # Development
              ms-azuretools.vscode-docker
            ]
            ++ (with pkgs.vscode-marketplace; [
              # jellydn.fzf-picker
            ]);
        };
      };
    };
  };
}
