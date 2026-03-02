# Docker user module — shell aliases (both platforms) + colima config (Darwin only).
# System packages come from:
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
    description = "Docker shell integration and (on Darwin) Colima VM configuration.";
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
    # Cross-platform aliases — active on both NixOS and Darwin when enabled.
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

    # Darwin only — deploy colima config file and auto-start launchd agent.
    (mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
      # cpu: 0 and memory: 0 = use all available resources (colima convention).
      # vmType: vz uses Apple's Virtualization Framework (Apple Silicon) for
      # better performance over QEMU. virtiofs gives native-speed bind mounts.
      home.file.".colima/default/colima.yaml".text = ''
        cpu: 0
        memory: 0
        disk: 60
        vmType: vz
        rosetta: false
        mountType: virtiofs
      '';

      launchd.agents.colima = {
        enable = true;
        config = {
          ProgramArguments = [
            "${pkgs.colima}/bin/colima"
            "start"
            "--foreground"
          ];
          RunAtLoad = true;
          KeepAlive = {SuccessfulExit = true;};
          StandardOutPath = "/tmp/colima-jordan.log";
          StandardErrorPath = "/tmp/colima-jordan.log";
        };
      };
    })
  ];
}
