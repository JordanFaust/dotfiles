# modules/themes/catppuccin/default.nix --- a pokemon and keyboard inspired theme

{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;

    # catppuccin-ags = pkgs.callPackage ./ags/ags.nix {};
in {
  config = mkIf (cfg.active == "catppuccin") (mkMerge [
    # Desktop-agnostic configuration
    {
      modules = {
        theme = {
          gtk = {
            theme = "Catppuccin-Macchiato-Compact-Pink-Dark";
            iconTheme = "MoreWaita";
            cursorTheme = "Qogir";
          };
          fonts = {
            sans.name = "Fira Sans";
            # mono.name = "JetBrains Mono";
            mono.name = "Cascadia Code Normal";
            mono.size = 16;
          };
          colors = {
            black         = "#24273A";
            red           = "#ED8796";
            green         = "#A6DA95";
            yellow        = "#EED49F";
            blue          = "#8AADF4";
            magenta       = "#B7BDF8";
            cyan          = "#91D7E3";
            silver        = "#dfd9d8";
            grey          = "#24455b";
            brightred     = "#ED8796";
            brightgreen   = "#A6DA95";
            brightyellow  = "#EED49F";
            brightblue    = "#8AADF4";
            brightmagenta = "#B7BDF8";
            brightcyan    = "#91D7E3";
            white         = "#ded8d7";

            types.fg      = "#ded8d7";
            types.bg      = "#24273A";
            types.panelbg = "#1E2030";
            types.border  = "#F5A97F";
          };
        };

        shell.zsh.rcFiles  = [ ./config/zsh/prompt.zsh ];
        shell.tmux.rcFiles = [ ./config/tmux.conf ];
        # desktop.browsers = {
        #   firefox.userChrome = concatMapStringsSep "\n" readFile [
        #     ./config/firefox/userChrome.css
        #   ];
        # };
      };
    }

    # Desktop (X11) theming
    (mkIf (config.services.xserver.enable && !cfg.wayland.enable) {
      user.packages = with pkgs; [
        unstable.dracula-theme
        # TODO replace this with papirus
        dracula-theme
        paper-icon-theme # for rofi
        papirus-icon-theme # dunst
        gnome.adwaita-icon-theme
        # Polybar Dependencies
        lsof
        zscroll
        # TODO replace pamixer with amixer
        pamixer
        alsa-utils
        playerctl
        # misc
        gpick
        neofetch
        # Dunst + EWW
        recode
        # Added utilities used in rice scripts
        moreutils
        # Fix broken nerd fonts
        nerdfix
      ];
      fonts.packages = with pkgs; [
        # General Coding Fonts
        jetbrains-mono
        fira-code
        fira-code-symbols
        cascadia-code
        victor-mono
        # General Sans Fonts
        open-sans
        siji
        # Icon Fonts
        # font-awesome
        # material-icons
        # material-design-icons
        (nerdfonts.override { fonts = [ "CascadiaCode" ]; })
        my.nonicons
      ];

      home.file = {
        # Firefox configuration
        ".mozilla/firefox/jordan.default" = { source = ./config/firefox; recursive = true; };
      };

      # Other dotfiles
      home.configFile = with config.modules; mkMerge [
        (mkIf desktop.apps.rofi.enable {
          "rofi/theme" = { source = ./config/rofi; recursive = true; };
        })
        (mkIf desktop.term.kitty.enable {
          "kitty" = { source = ./config/kitty; recursive = true; };
          "kitty/themes/monokai-pro.conf".source = ./config/kitty/themes/monokai-pro.conf;
          "kitty/themes/catppuccin-macchiato.conf".source = ./config/kitty/themes/catppuccin-macchiato.conf;
        })
      ];
    })
  ]);
}

