{  config, lib, pkgs, inputs, ... }:
{
  imports = [ inputs.ags.homeManagerModules.default ];

  home.packages = with pkgs; [
    dart-sass
    gtk3 # gtk-launch
  ];

  # home.configFile = with config; {
  #   "ags/config.js" = {
  #     source = "${pkgs.my.ags.desktop.config}/config.js";
  #   };
  # }

  programs.ags = {
    enable = true;
    configDir = pkgs.my.ags.desktop.config;
    extraPackages = with pkgs; [
      accountsservice
    ];
  };
}
