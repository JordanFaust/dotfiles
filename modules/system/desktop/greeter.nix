{
  config,
  lib,
  pkgs,
  ...
}: let
  # cursor = config.user-options.desktop.gtk.cursor.name;
  # conf = pkgs.writeText "config" ''
  #   exec-once = ${lib.getExe greeter}; hyprctl dispatch exit
  #   misc {
  #     disable_splash_rendering = true
  #     force_default_wallpaper = 1
  #   }
  # '';
  greetdUser = config.services.greetd.settings.default_session.user.group;
in {
  # services.greetd = {
  #   enable = true;
  #   # settings.default_session = {
  #   #   command = "${pkgs.dbus}/bin/dbus-run-session ${lib.getExe greeter}";
  #   # };
  #   # settings.default_session = {
  #   #   command = "Hyprland --config ${conf}";
  #   # };
  #   settings.default_session.command = pkgs.writeShellScript "greeter" ''
  #     export XCURSOR_THEME=${cursor}
  #     ${pkgs.my.ags}/bin/greeter
  #   '';
  # };
  #
  # user.extraGroups = [ "greeter" ];
  #
  # systemd.tmpfiles.settings."10-greeter" = let
  #   defaultConfig = {
  #     user = "greeter";
  #     group = "greeter";
  #     mode = "0755";
  #   };
  # in {
  #   "/var/log/greeter".d = defaultConfig;
  #   "/var/cache/greeter".d = defaultConfig;
  # };
  # #
  # # systemd.tmpfiles.rules = [
  # #   "d '/var/cache/greeter' - greeter greeter - -"
  # # ];
  # #
  # system.activationScripts.wallpaper = ''
  #   PATH=$PATH:${pkgs.coreutils}/bin:${pkgs.gawk}/bin:${pkgs.jq}/bin
  #   CACHE="/var/cache/greeter"
  #   OPTS="$CACHE/options.json"
  #
  #   cp /home/${config.user.name}/.cache/ags/options.json $OPTS
  #   chown greeter:greeter $OPTS
  #
  #   BG=$(cat $OPTS | jq -r '.wallpaper // "/home/${config.user.name}/.config/background"')
  #
  #   cp $BG $CACHE/background
  #   chown greeter:greeter $CACHE/background
  # '';
  #
  # environment.systemPackages = with pkgs; [
  #   morewaita-icon-theme
  #   gnome.adwaita-icon-theme
  # ];
}
