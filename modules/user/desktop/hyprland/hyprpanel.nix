{
  config,
  inputs,
  pkgs,
  lib,
  osConfig,
  system,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.hyprland;
in {
  options = {};

  config = lib.mkIf (cfg.enable) {
  };
}
