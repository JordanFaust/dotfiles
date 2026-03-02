# Docker zsh aliases — injected when system docker module is enabled.
# System packages (docker, docker-compose, etc.) come from modules/system/services/docker.nix.
{
  config,
  lib,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  dockerEnabled = osConfig.modules.services.docker.enable or false;
  zshEnabled = config.modules.shell.zsh != null && config.modules.shell.zsh.enable;
in {
  config = mkIf (dockerEnabled && zshEnabled) {
    programs.zsh.initContent = mkOrder 1500 ''
      ## Docker aliases (from modules/user/services/docker.nix)
      alias dk=docker
      alias dkc=docker-compose
      alias dkm=docker-machine
      alias dkl='dk logs'
      alias dkcl='dkc logs'

      dkclr() {
        dk stop $(docker ps -a -q)
        dk rm $(docker ps -a -q)
      }

      dke() {
        dk exec -it "$1" "''${@:1}"
      }
    '';
  };
}
