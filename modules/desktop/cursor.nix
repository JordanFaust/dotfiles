# Module to set a default cursor theme

{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.cursor;

  indexThemeText = theme: generators.toINI {} {"icon theme" = { Inherits = "${theme}"; }; };

  mkDefaultCursorFile = theme: pkgs.writeTextDir
    "share/icons/default/index.theme"
    "${indexThemeText theme}";

  defaultCursorPkg = mkDefaultCursorFile cfg.theme;
in
{
  options.modules.desktop.cursor = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to set a default cursor theme for graphical environments.
      '';
    };

    theme = mkOption {
      type = types.str;
      default = "Adwaita";
      example = "Adwaita";
      description = "The name of the defualt cursor theme.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      defaultCursorPkg
    ];
  };
}
