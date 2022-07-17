# modules/desktop/term/alacritty.nix

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.alacritty;
in {
  options.modules.desktop.term.alacritty = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # We want better tmux integration since that is my primary driver
    modules.shell.zsh.rcInit = ''
      [ "$TERM" = xterm-256color ] && export TERM=screen-256color
    '';

    user.packages = with pkgs; [
      alacritty
      (makeDesktopItem {
        name = "alacritty";
        desktopName = "Alacritty";
        genericName = "Alacritty terminal";
        icon = "utilities-terminal";
        exec = "${alacritty}/bin/alacritty";
        categories = [ "Development" "System" "Utility" ];
      })
    ];
  };
}
