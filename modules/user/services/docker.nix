# Docker user module — shell aliases (all platforms) and socket env var (Darwin).
# Service lifecycle and package installation are system concerns handled by:
#   NixOS  → modules/system/services/docker.nix
#   Darwin → modules/darwin/services/docker.nix
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
  options.modules.services.docker = mkOption {
    description = "Docker shell integration.";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "docker";
          }
        ];
      });
    default = {};
  };

  config = mkMerge [
    (mkIf cfg.enable {
      programs.zsh.initContent = mkOrder 1500 ''
        ## Docker aliases
        alias dk=docker
        alias dkc=docker-compose
        alias dkl='dk logs'
        alias dkcl='dkc logs'

        dkclr() {
          dk stop $(docker ps -a -q)
          dk rm $(docker ps -a -q)
        }

        dke() {
          dk exec -it "$1" "''${@:2}"
        }
      '';
    })

    # On Darwin, Colima puts its socket at ~/.colima/default/docker.sock rather
    # than /var/run/docker.sock. Export DOCKER_HOST so every tool finds it without
    # needing a manual `docker context use colima`.
    (mkIf (cfg.enable && pkgs.stdenv.hostPlatform.isDarwin) {
      home.sessionVariables.DOCKER_HOST = "unix://${config.home.homeDirectory}/.colima/default/docker.sock";
    })
  ];
}
