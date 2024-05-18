{
  config,
  lib,
  pkgs,
  inputs,
  osConfig,
  ...
}: let
  cursor = config.modules.desktop.gtk.cursor.name;
in {
  imports = [inputs.ags.homeManagerModules.default];

  home.packages = with pkgs; [
    # libdbusmenu-gtk3
    bun
    dart-sass
    # allow triggering greeter from cli
    (pkgs.writeScriptBin "greeter" ''
      export XCURSOR_THEME=${cursor}
      ${pkgs.my.ags}/bin/greeter
    '')
    fd
    brightnessctl
    swww
    # inputs.matugen.packages.${system}.default
    slurp
    wf-recorder
    wl-clipboard
    wayshot
    swappy
    hyprpicker
    pavucontrol
    networkmanager
    gtk3
  ];

  programs.ags = {
    enable = true;
    configDir = pkgs.my.ags;
    extraPackages = with pkgs; [
      accountsservice
    ];
  };
}
