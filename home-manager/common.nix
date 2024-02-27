{ pkgs, inputs, config, lib, username, ... }:
with lib;
with lib.my;
let
  homeDirectory = "/home/${username}";
in
{
  imports =
    # Space to include configuraton that must run first
    # TODO: might remove
    [(import ./options.nix { inherit lib types; } )]
    # All my personal modules
    ++ (mapModulesRec' (toString ./modules) import);

  news.display = "show";

  targets.genericLinux.enable = true;

  home = {
    inherit username homeDirectory;

    sessionVariables = {
      QT_XCB_GL_INTEGRATION = "none"; # kde-connect
      NIXPKGS_ALLOW_UNFREE = "1";
      NIXPKGS_ALLOW_INSECURE = "1";
      BAT_THEME = "base16";
      GOPATH = "${homeDirectory}/.local/share/go";
      GOMODCACHE="${homeDirectory}/.cache/go/pkg/mod";
    };

    sessionPath = [
      "$HOME/.local/bin"
    ];
  };

  programs.home-manager.enable = true;
}

