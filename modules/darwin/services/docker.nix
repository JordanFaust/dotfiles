# Darwin system module for Docker via Colima.
# Installs all tooling and manages the Colima VM as a user-session launchd agent.
# Shell aliases and DOCKER_HOST are handled by the user module (modules/user/services/docker.nix).
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

    cpu = mkOption {
      type = types.int;
      description = "Number of CPUs to allocate to the Colima VM.";
    };

    memory = mkOption {
      type = types.int;
      description = "Memory in GiB to allocate to the Colima VM.";
    };

    disk = mkOption {
      type = types.int;
      description = "Disk size in GiB for the Colima VM.";
    };

    vmType = mkOption {
      type = types.enum ["vz" "qemu"];
      default = "vz";
      description = "VM type — vz uses Apple Virtualization Framework (Apple Silicon only), qemu for broader compatibility.";
    };

    mountType = mkOption {
      type = types.enum ["virtiofs" "sshfs" "9p"];
      default = "virtiofs";
      description = "Mount type for host volume sharing.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      colima
      docker-client
      docker-compose
      docker-buildx
      dive
    ];

    # User-session launchd agent — runs as the logged-in user, starts at login.
    # EnvironmentVariables sets PATH so colima's dependency checks find docker and
    # lima without a wrapper script (which would show a Nix store path as the
    # process name in the macOS UI).
    # SuccessfulExit = false: only restart on crash; `colima stop` exits 0 and stays down.
    launchd.user.agents.colima = {
      serviceConfig = {
        Label = "com.github.colima";
        ProgramArguments = [
          "${pkgs.colima}/bin/colima"
          "start"
          "--foreground"
          "--cpu"
          (toString cfg.cpu)
          "--memory"
          (toString cfg.memory)
          "--disk"
          (toString cfg.disk)
          "--vm-type"
          cfg.vmType
          "--mount-type"
          cfg.mountType
        ];
        EnvironmentVariables = {
          PATH = "${pkgs.docker-client}/bin:${pkgs.lima}/bin:/usr/bin:/bin:/usr/sbin:/sbin";
        };
        RunAtLoad = true;
        KeepAlive = {SuccessfulExit = false;};
        StandardOutPath = "/tmp/colima.log";
        StandardErrorPath = "/tmp/colima.log";
      };
    };
  };
}
