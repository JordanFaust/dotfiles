{ pkgs, config, ... }:
let
  greeter = pkgs.my.ags.greeter {
    cursor = config.user-options.desktop.gtk.cursor.name;
  };
in {
  # services.greetd = {
  #   enable = true;
  #   settings.default_session = {
  #     command = "${greeter}/bin/greeter";
  #   };
  # };

  systemd.tmpfiles.rules = [
    "d '/var/cache/greeter' - greeter greeter - -"
  ];

  system.activationScripts.wallpaper = ''
    PATH=$PATH:${pkgs.coreutils}/bin:${pkgs.gawk}/bin:${pkgs.jq}/bin
    CACHE="/var/cache/greeter"
    OPTS="$CACHE/options.json"

    cp /home/${config.user.name}/.cache/ags/options.json $OPTS
    chown greeter:greeter $OPTS

    BG=$(cat $OPTS | jq -r '.wallpaper // "/home/${config.user.name}/.config/background"')

    cp $BG $CACHE/background
    chown greeter:greeter $CACHE/background
  '';

  environment.systemPackages = with pkgs; [
    morewaita-icon-theme
    gnome.adwaita-icon-theme
  ];
}
