{
  config,
  options,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop;
in {
  config = mkIf config.services.xserver.enable {
    assertions = [
      {
        assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
        message = "Can't have more than one desktop environment enabled at a time";
      }
      {
        assertion = let
          srv = config.services;
        in
          srv.xserver.enable
          || srv.sway.enable
          || srv.wayland.enable
          || !(anyAttrs
            (n: v:
              isAttrs v
              && anyAttrs (n: v: isAttrs v && v.enable))
            cfg);
        message = "Can't enable a desktop app without a desktop environment";
      }
    ];

    user.packages = with pkgs; [
      libsForQt5.qtstyleplugin-kvantum # SVG-based Qt5 theme engine plus a config tool and extra theme

      # Performance Monitoring
      bmon
      btop
      htop

      ##
      ## Ricing Packages
      ##

      # NixOS Config Testing
      # vagrant
      virt-manager

      # Misc
      pdftk
    ];

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs; [
        nerd-fonts.jetbrains-mono
        nerd-fonts.fira-code
        nerd-fonts.droid-sans-mono
      ];
    };
  };
}
