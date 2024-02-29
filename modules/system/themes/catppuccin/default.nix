# modules/themes/catppuccin/default.nix --- a pokemon and keyboard inspired theme
{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.theme;
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
            black = "#24273A";
            red = "#ED8796";
            green = "#A6DA95";
            yellow = "#EED49F";
            blue = "#8AADF4";
            magenta = "#B7BDF8";
            cyan = "#91D7E3";
            silver = "#dfd9d8";
            grey = "#24455b";
            brightred = "#ED8796";
            brightgreen = "#A6DA95";
            brightyellow = "#EED49F";
            brightblue = "#8AADF4";
            brightmagenta = "#B7BDF8";
            brightcyan = "#91D7E3";
            white = "#ded8d7";

            types.fg = "#ded8d7";
            types.bg = "#24273A";
            types.panelbg = "#1E2030";
            types.border = "#F5A97F";
          };
        };

        shell.zsh.rcFiles = [./config/zsh/prompt.zsh];
        shell.tmux.rcFiles = [./config/tmux.conf];
        # desktop.browsers = {
        #   firefox.userChrome = concatMapStringsSep "\n" readFile [
        #     ./config/firefox/userChrome.css
        #   ];
        # };
      };
    }
  ]);
}
