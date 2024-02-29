# modules/desktop/term/kitty.nix
{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.term.kitty;
in {
  options.modules.desktop.term.kitty = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # We want better tmux integration since that is my primary driver
    modules.shell.zsh.rcInit = ''
      [ "$TERM" = xterm-256color ] && export TERM=screen-256color
    '';

    user.packages = with pkgs; [
      kitty
      (makeDesktopItem {
        name = "kitty";
        desktopName = "Kitty";
        genericName = "Kitty terminal";
        icon = "utilities-terminal";
        exec = "${kitty}/bin/kitty";
        categories = ["Development" "System" "Utility"];
      })
    ];
  };
}
