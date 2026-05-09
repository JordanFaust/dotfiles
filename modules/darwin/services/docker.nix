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

  # Generate colima config as JSON (a valid YAML superset) so the provision
  # scripts and all VM parameters are captured declaratively.
  colimaConfig = pkgs.writeText "colima-config.yaml" (builtins.toJSON ({
      inherit (cfg) cpu memory disk vmType mountType mountInotify;
      runtime = "docker";
    }
    // optionalAttrs (cfg.mounts != []) {
      mounts = map (m: {inherit (m) location writable;}) cfg.mounts;
    }
    // optionalAttrs (cfg.provision != []) {
      provision = map (p: {inherit (p) mode script;}) cfg.provision;
    }));

  # Wrapper that installs the Nix-generated config before starting colima.
  # Colima's provision section has no CLI flag — it must live in the YAML file.
  # Remove stale Lima vz.pid before starting — after an unclean shutdown macOS
  # recycles PIDs, causing Lima to think the VZ driver is still running and
  # refusing to start with "vz driver is running but host agent is not".
  colimaStartScript = pkgs.writeShellScript "colima-start" ''
    CONFIG_DIR="$HOME/.colima/default"
    mkdir -p "$CONFIG_DIR"
    install -m 644 "${colimaConfig}" "$CONFIG_DIR/colima.yaml"
    rm -f "$HOME/.colima/_lima/colima/vz.pid"
    exec "${pkgs.colima}/bin/colima" start --foreground
  '';
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

    mountInotify = mkOption {
      type = types.bool;
      default = false;
      description = "Propagate inotify file events to the VM. Experimental.";
    };

    mounts = mkOption {
      type = types.listOf (types.submodule {
        options = {
          location = mkOption {
            type = types.str;
            description = "Host path to mount into the VM. Supports ~ expansion.";
          };
          writable = mkOption {
            type = types.bool;
            default = true;
            description = "Whether the mount is writable.";
          };
        };
      });
      default = [];
      description = "Additional host paths to mount into the Colima VM.";
    };

    provision = mkOption {
      type = types.listOf (types.submodule {
        options = {
          mode = mkOption {
            type = types.enum ["system" "user"];
            default = "system";
            description = "Whether to run the provision script as root (system) or the default user.";
          };
          script = mkOption {
            type = types.lines;
            description = "Shell script to run inside the Colima VM on start.";
          };
        };
      });
      default = [];
      description = "Provisioning scripts to run inside the Colima VM on every start.";
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
    # A wrapper script installs the Nix-generated colima.yaml (which includes
    # provision scripts) then execs colima in foreground mode.
    # SuccessfulExit = false: only restart on crash; `colima stop` exits 0 and stays down.
    launchd.user.agents.colima = {
      serviceConfig = {
        Label = "com.github.colima";
        ProgramArguments = ["${colimaStartScript}"];
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
