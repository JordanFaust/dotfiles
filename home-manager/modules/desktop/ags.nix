{  config, lib, pkgs, inputs, ... }:
{
  imports = [ inputs.ags.homeManagerModules.default ];

  home.packages = with pkgs; [
    libdbusmenu-gtk3
    dart-sass
    gtk3 # gtk-launch
  ];

  programs.ags = {
    enable = true;
    configDir = pkgs.my.ags.desktop.config;
    extraPackages = with pkgs; [
      accountsservice
    ];
  };
}
