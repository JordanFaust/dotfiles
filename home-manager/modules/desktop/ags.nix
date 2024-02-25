{  config, lib, pkgs, inputs, ... }:
let greeter = pkgs.my.ags.greeter { cursor = config.desktop.gtk.cursor.name; };
# let greeter = pkgs.my.ags.desktop;
in {
  imports = [ inputs.ags.homeManagerModules.default ];

  home.packages = with pkgs; [
    libdbusmenu-gtk3
    dart-sass
    gtk3 # gtk-launch
    # allow triggering greeter from cli
    greeter
  ];

  programs.ags = {
    enable = true;
    configDir = pkgs.my.ags.config;
    extraPackages = with pkgs; [
      accountsservice
    ];
  };
}
