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
      feh # image viewer
      xclip
      xdotool
      xdo
      xorg.xwininfo
      libqalculate # calculator cli w/ currency conversion
      (makeDesktopItem {
        name = "scratch-calc";
        desktopName = "Calculator";
        icon = "calc";
        exec = ''scratch "${tmux}/bin/tmux new-session -s calc -n calc qalc"'';
        categories = ["Development"];
      })
      qgnomeplatform # QPlatformTheme for a better Qt application inclusion in GNOME
      libsForQt5.qtstyleplugin-kvantum # SVG-based Qt5 theme engine plus a config tool and extra theme

      # Patch favorite fonts with NerdFonts
      (nerdfonts.override {fonts = ["JetBrainsMono" "FiraCode"];})

      # Performance Monitoring
      bmon
      btop
      htop

      ##
      ## Ricing Packages
      ##

      # Widgets
      # unstable.eww
      # (builtins.getFlake "github:NixOS/nixpkgs/61f87a8dc31587ea7738c9e14f46f8a3199874e5").legacyPackages.${pkgs.system}.eww
      (builtins.getFlake "github:NixOS/nixpkgs/61f87a8dc31587ea7738c9e14f46f8a3199874e5").legacyPackages.${pkgs.system}.eww-wayland
      # eww
      socat
      # Audio
      pavucontrol
      # Battery
      acpi # battery
      # Bluetooth
      bluez # bluetoothctl
      # Brightness
      brightnessctl
      # Wireless
      wirelesstools
      # Networking
      # TODO should I stick with dmenu or switch to the gnome applet
      dmenu
      networkmanager_dmenu

      # NixOS Config Testing
      vagrant
      virt-manager

      # Misc
      pdftk
    ];

    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      packages = with pkgs; [
        ubuntu_font_family
        dejavu_fonts
        symbola
        (nerdfonts.override {fonts = ["JetBrainsMono" "FiraCode" "DroidSansMono"];})
      ];
    };
  };
}
