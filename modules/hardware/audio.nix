{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    sound.enable = true;
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      wireplumber.enable = true;
    };

    environment.etc = {
      "pipewire/pipewire.conf.d/92-low-latency.conf".text = ''
        context.properties = {
          default.clock.rate = 44100
          default.clock.quantum = 512
          default.clock.min-quantum = 512
          default.clock.max-quantum = 512
        }
      '';
    };

    services.udev.extraRules = ''
      KERNEL=="rtc0", GROUP="audio"
      KERNEL=="hpet", GROUP="audio"
    '';

    security.pam.loginLimits = [
      { domain = "@audio"; item = "memlock"; type = "-"   ; value = "unlimited"; }
      { domain = "@audio"; item = "rtprio" ; type = "-"   ; value = "99"       ; }
      { domain = "@audio"; item = "nofile" ; type = "soft"; value = "99999"    ; }
      { domain = "@audio"; item = "nofile" ; type = "hard"; value = "524288"    ; }
    ];

    # services.pipewire = {
    #   enable = true;
    #   alsa.enable = true;
    #   alsa.support32Bit = true;
    #   pulse.enable = true;
    # };
    #
    # security.rtkit.enable = true;
    #
    # environment.systemPackages = with pkgs; [
    #   easyeffects
    #   # pulseaudio
    #   # pacmd
    # ];
    #
    # # HACK Prevents ~/.esd_auth files by disabling the esound protocol module
    # #      for pulseaudio, which I likely don't need. Is there a better way?
    # hardware.pulseaudio.configFile =
    #   let inherit (pkgs) runCommand pulseaudio;
    #       paConfigFile =
    #         runCommand "disablePulseaudioEsoundModule"
    #           { buildInputs = [ pulseaudio ]; } ''
    #             mkdir "$out"
    #             cp ${pulseaudio}/etc/pulse/default.pa "$out/default.pa"
    #             sed -i -e 's|load-module module-esound-protocol-unix|# ...|' "$out/default.pa"
    #           '';
    #   in mkIf config.hardware.pulseaudio.enable
    #     "${paConfigFile}/default.pa";
    #
    # user.extraGroups = [ "audio" ];
  };
}
