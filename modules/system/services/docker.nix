{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.docker;
in {
  options.modules.services.docker = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      docker-buildx
      docker-compose
      # Inspect Docker Images
      dive
    ];

    # env.DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    # env.MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker/machine";

    user.extraGroups = ["docker"];

    # Docker zsh aliases are in modules/user/services/docker.nix (sourced when zsh enabled)

    virtualisation = {
      docker = {
        enable = true;
        autoPrune.enable = true;
        enableOnBoot = mkDefault false;
        # listenOptions = [];
      };
    };
  };
}
