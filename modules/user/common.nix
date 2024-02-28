{ pkgs, inputs, config, lib, username, ... }:
with lib;
with lib.my;
let
  homeDirectory = "/home/${username}";
in
{
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

