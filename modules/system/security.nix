{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit
    (lib)
    concatStringsSep
    mapAttrsToList
    mkIf
    mkMerge
    mkDefault
    mkOverride
    types
    ;
  inherit
    (lib.my)
    mkOpt
    ;
  cfg = config.modules.security;
in {
  options.modules.security = {
    copySensitive = mkOpt (types.attrsOf types.lines) {};
  };

  config = mkMerge [
    {
      ## System security tweaks
      # sets hidepid=2 on /proc (make process info visible only to owning user)
      # NOTE Was removed on nixpkgs-unstable because it doesn't do anything
      # security.hideProcessInformation = true;
      # Prevent replacing the running kernel w/o reboot
      security.protectKernelImage = true;

      # tmpfs = /tmp is mounted in ram. Doing so makes temp file management speedy
      # on ssd systems, and volatile! Because it's wiped on reboot.
      boot.tmp.useTmpfs = mkDefault true;
      # If not using tmpfs, which is naturally purged on reboot, we must clean it
      # /tmp ourselves. /tmp should be volatile storage!
      boot.tmp.cleanOnBoot = mkDefault (!config.boot.tmp.useTmpfs);

      # Fix a security hole in place for backwards compatibility. See desc in
      # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
      boot.loader.systemd-boot.editor = false;

      boot.kernel.sysctl = {
        # The Magic SysRq key is a key combo that allows users connected to the
        # system console of a Linux kernel to perform some low-level commands.
        # Disable it, since we don't need it, and is a potential security concern.
        "kernel.sysrq" = 0;

        # Disable ftrace debugging
        "kernel.ftrace_enabled" = mkDefault false;
        "kernel.unprivileged_bpf_disabled" = 1;

        ## TCP hardening

        # Prevent bogus ICMP errors from filling up logs.
        "net.ipv4.icmp_ignore_bogus_error_responses" = 1;

        # Do not accept IP source route packets (we're not a router)
        "net.ipv4.conf.all.accept_source_route" = 0;
        "net.ipv6.conf.all.accept_source_route" = 0;

        # Protects against SYN flood attacks
        "net.ipv4.tcp_syncookies" = 1;
        # Incomplete protection again TIME-WAIT assassination
        "net.ipv4.tcp_rfc1337" = 1;

        ## TCP optimization
        # TCP Fast Open is a TCP extension that reduces network latency by packing
        # data in the sender’s initial TCP SYN. Setting 3 = enable TCP Fast Open for
        # both incoming and outgoing connections:
        "net.ipv4.tcp_fastopen" = 3;
        # Bufferbloat mitigations + slight improvement in throughput & latency
        "net.ipv4.tcp_congestion_control" = "bbr";
        "net.core.default_qdisc" = "cake";

        # Hide kptrs even for processes with CAP_SYSLOG
        "kernel.kptr_restrict" = mkOverride 500 2;

        # Disable bpf() JIT (to eliminate spray attacks)
        "net.core.bpf_jit_enable" = mkDefault false;

        # Enable strict reverse path filtering (that is, do not attempt to route
        # packets that "obviously" do not belong to the iface's network; dropped
        # packets are logged as martians).
        "net.ipv4.conf.all.log_martians" = mkDefault true;
        "net.ipv4.conf.all.rp_filter" = mkDefault "1";
        "net.ipv4.conf.default.log_martians" = mkDefault true;
        "net.ipv4.conf.default.rp_filter" = mkDefault "1";

        # Ignore broadcast ICMP (mitigate SMURF)
        "net.ipv4.icmp_echo_ignore_broadcasts" = mkDefault true;

        # Ignore incoming ICMP redirects (note: default is needed to ensure that the
        # setting is applied to interfaces added after the sysctls are set)
        "net.ipv4.conf.all.accept_redirects" = mkDefault false;
        "net.ipv4.conf.all.secure_redirects" = mkDefault false;
        "net.ipv4.conf.default.accept_redirects" = mkDefault false;
        "net.ipv4.conf.default.secure_redirects" = mkDefault false;
        "net.ipv6.conf.all.accept_redirects" = mkDefault false;
        "net.ipv6.conf.default.accept_redirects" = mkDefault false;

        # Ignore outgoing ICMP redirects (this is ipv4 only)
        "net.ipv4.conf.all.send_redirects" = mkDefault false;
        "net.ipv4.conf.default.send_redirects" = mkDefault false;

        "net.ipv4.conf.all.forwarding" = mkDefault "0";
      };

      boot.kernelModules = ["tcp_bbr"];

      # Change me later!
      user.initialPassword = "nixos";
      users.users.root.initialPassword = "nixos";

      # So we don't have to do this later...
      security.acme.acceptTerms = true;

      # Setup the Gnome Keyring
      services.gnome.gnome-keyring.enable = true;
      programs.seahorse.enable = true;

      # Setup basic log rotation
      services.logrotate.enable = true;
      services.journald.extraConfig = "SystemMaxUse=1000M";

      # Setup Password Quaility Control
      security.pam.services.passwd.rules.password.pwquality = {
        control = "required";
        modulePath = "${pkgs.libpwquality.lib}/lib/security/pam_pwquality.so";
        # order BEFORE pam_unix.so
        order = config.security.pam.services.passwd.rules.password.unix.order - 10;
        settings = {
          shadowretry = 3;
          minlen = 12;
          difok = 6;
          dcredit = -1;
          ucredit = -1;
          ocredit = -1;
          lcredit = -1;
          enforce_for_root = true;
        };
      };

      user.packages = with pkgs; [
        # Setup Keybase for use of storing sensitive credentials
        # kbfs
        openssl
        # Security scanning tools
        # Disable until grype can actually build in nixos
        vulnix
        sbomnix
        grype
        syft
        lynis
        (pkgs.writeShellScriptBin "nixos-scan" ''
          mkdir -p /etc/dotfiles/reports

          # echo "Generating meta information.."
          # nix-env -qa --meta --json '.*' > /etc/dotfiles/reports/meta.json

          echo "Generating SBOM..."
          ${sbomnix}/bin/sbomnix /run/current-system/sw/ \
            --csv /etc/dotfiles/reports/sbom.csv \
            --cdx /etc/dotfiles/reports/sbox.cdx.json \
            --spdx /etc/dotfiles/reports/sbom.spdx.json

          echo "Evaluating vulnerabilities..."
          ${grype}/bin/grype sbom:/etc/dotfiles/reports/sbom.spdx.json --add-cpes-if-none
        '')
        nix-tree
      ];

      # Enable Keybase
      # services.keybase.enable = true;
      # security.pam.services.keybase.enableGnomeKeyring = true;

      #
      # Systemd Hardening
      #

    #   systemd.services.systemd-rfkill = {
    #     serviceConfig = {
    #       ProtectSystem = "strict";
    #       ProtectHome = true;
    #       ProtectKernelTunables = true;
    #       ProtectKernelModules = true;
    #       ProtectControlGroups = true;
    #       ProtectClock = true;
    #       ProtectProc = "invisible";
    #       ProcSubset = "pid";
    #       PrivateTmp = true;
    #       MemoryDenyWriteExecute = true; #
    #       NoNewPrivileges = true;
    #       LockPersonality = true; #
    #       RestrictRealtime = true; #
    #       SystemCallFilter = ["@system-service"];
    #       SystemCallArchitectures = "native";
    #       UMask = "0077";
    #       IPAddressDeny = "any";
    #     };
    #   };
    #
    #   systemd.services.systemd-journald = {
    #     serviceConfig = {
    #       UMask = 0077;
    #       PrivateNetwork = true;
    #       ProtectHostname = true;
    #       ProtectKernelModules = true;
    #     };
    #   };
    #
    #   # Network Manager
    #   systemd.services.NetworkManager-dispatcher = {
    #     serviceConfig = {
    #       ProtectHome = true;
    #       ProtectKernelTunables = true;
    #       ProtectKernelModules = true;
    #       ProtectControlGroups = true;
    #       ProtectKernelLogs = true;
    #       ProtectHostname = true;
    #       ProtectClock = true;
    #       ProtectProc = "invisible";
    #       ProcSubset = "pid";
    #       PrivateUsers = true;
    #       PrivateDevices = true;
    #       MemoryDenyWriteExecute = true;
    #       NoNewPrivileges = true;
    #       LockPersonality = true;
    #       RestrictRealtime = true;
    #       RestrictSUIDSGID = true;
    #       RestrictAddressFamilies = ["AF_INET" "AF_UNIX"];
    #       RestrictNamespaces = true;
    #       SystemCallFilter = ["@system-service"];
    #       SystemCallArchitectures = "native";
    #       UMask = "0077";
    #       IPAddressDeny = "any";
    #     };
    #   };
    #   systemd.services.NetworkManager = {
    #     serviceConfig = {
    #       NoNewPrivileges = true;
    #       ProtectClock = true;
    #       ProtectKernelLogs = true;
    #       ProtectControlGroups = true;
    #       ProtectKernelModules = true;
    #       SystemCallArchitectures = "native";
    #       MemoryDenyWriteExecute = true;
    #       ProtectProc = "invisible";
    #       ProcSubset = "pid";
    #       RestrictNamespaces = true;
    #       ProtectKernelTunables = true;
    #       ProtectHome = true;
    #       PrivateTmp = true;
    #       UMask = "0077";
    #       SystemCallFilter = ["@system-service"];
    #     };
    #   };
    #
    #   systemd.services."dbus" = {
    #     serviceConfig = {
    #       PrivateTmp = true;
    #       PrivateNetwork = true;
    #       ProtectSystem = "full";
    #       ProtectHome = true;
    #       # SystemCallFilter = "@system-service @clock @cpu-emulation @module @mount @obsolete @raw-io @reboot @swap";
    #       ProtectKernelTunables = true;
    #       NoNewPrivileges = true;
    #       # CapabilityBoundingSet = ["~CAP_SYS_TIME" "~CAP_SYS_PACCT" "~CAP_KILL" "~CAP_WAKE_ALARM" "~CAP_SYS_BOOT" "~CAP_SYS_CHROOT" "~CAP_LEASE" "~CAP_MKNOD" "~CAP_NET_ADMIN" "~CAP_SYS_ADMIN" "~CAP_SYSLOG" "~CAP_NET_BIND_SERVICE" "~CAP_NET_BROADCAST" "~CAP_AUDIT_WRITE" "~CAP_AUDIT_CONTROL" "~CAP_SYS_RAWIO" "~CAP_SYS_NICE" "~CAP_SYS_RESOURCE" "~CAP_SYS_TTY_CONFIG" "~CAP_SYS_MODULE" "~CAP_IPC_LOCK" "~CAP_LINUX_IMMUTABLE" "~CAP_BLOCK_SUSPEND" "~CAP_MAC_*" "~CAP_DAC_*" "~CAP_FOWNER" "~CAP_IPC_OWNER" "~CAP_SYS_PTRACE" "~CAP_SETUID" "~CAP_SETGID" "~CAP_SETPCAP" "~CAP_FSETID" "~CAP_SETFCAP" "~CAP_CHOWN"];
    #       ProtectKernelModules = true;
    #       ProtectKernelLogs = true;
    #       ProtectClock = true;
    #       ProtectControlGroups = true;
    #       RestrictNamespaces = true;
    #       MemoryDenyWriteExecute = true;
    #       # RestrictAddressFamilies = ["~AF_PACKET" "~AF_NETLINK"];
    #       ProtectHostname = true;
    #       LockPersonality = true;
    #       RestrictRealtime = true;
    #       # PrivateUsers = true;
    #     };
    #   };
    #
    #   systemd.services.reload-systemd-vconsole-setup = {
    #     serviceConfig = {
    #       ProtectSystem = "strict";
    #       ProtectHome = true;
    #       ProtectKernelTunables = true;
    #       ProtectKernelModules = true;
    #       ProtectControlGroups = true;
    #       ProtectKernelLogs = true;
    #       ProtectClock = true;
    #       PrivateUsers = true;
    #       PrivateDevices = true;
    #       MemoryDenyWriteExecute = true;
    #       NoNewPrivileges = true;
    #       LockPersonality = true;
    #       RestrictRealtime = true;
    #       RestrictNamespaces = true;
    #       UMask = "0077";
    #       IPAddressDeny = "any";
    #     };
    #   };
    #
    #   systemd.services.rescue = {
    #     serviceConfig = {
    #       ProtectSystem = "strict";
    #       ProtectHome = true;
    #       ProtectKernelTunables = true;
    #       ProtectKernelModules = true;
    #       ProtectControlGroups = true;
    #       ProtectKernelLogs = true;
    #       ProtectClock = true;
    #       ProtectProc = "invisible";
    #       ProcSubset = "pid";
    #       PrivateTmp = true;
    #       PrivateUsers = true;
    #       # PrivateDevices = true; # Might need adjustment for rescue operations
    #       PrivateIPC = true;
    #       MemoryDenyWriteExecute = true;
    #       NoNewPrivileges = true;
    #       LockPersonality = true;
    #       RestrictRealtime = true;
    #       RestrictSUIDSGID = true;
    #       RestrictAddressFamilies = "AF_INET AF_INET6"; # Networking might be necessary in rescue mode
    #       RestrictNamespaces = true;
    #       # SystemCallFilter = ["@system-service" "@clock" "@setuid" "@mount" "@process" "@basic-io" "@privileged"];
    #       SystemCallArchitectures = "native";
    #       UMask = "0077";
    #       # IPAddressDeny = "any"; # May need to be relaxed for network troubleshooting in rescue mode
    #     };
    #   };
    #
    #   # systemd.services.nix-daemon = {
    #   #   serviceConfig = {
    #   #     ReadWritePaths = [ "/root/" ];
    #   #     ProtectHome = true;
    #   #     PrivateUsers = false;
    #   #   };
    #   # };
    #
    #   systemd.services."systemd-ask-password-console" = {
    #     serviceConfig = {
    #       ProtectSystem = "strict";
    #       ProtectHome = true;
    #       ProtectKernelTunables = true;
    #       ProtectKernelModules = true;
    #       ProtectControlGroups = true;
    #       ProtectKernelLogs = true;
    #       ProtectClock = true;
    #       ProtectProc = "invisible";
    #       ProcSubset = "pid";
    #       PrivateTmp = true;
    #       PrivateUsers = true;
    #       # PrivateDevices = true; # May need adjustment for console access
    #       PrivateIPC = true;
    #       MemoryDenyWriteExecute = true;
    #       NoNewPrivileges = true;
    #       LockPersonality = true;
    #       RestrictRealtime = true;
    #       RestrictSUIDSGID = true;
    #       RestrictAddressFamilies = "AF_INET AF_INET6";
    #       RestrictNamespaces = true;
    #       SystemCallFilter = ["@system-service"]; # A more permissive filter
    #       SystemCallArchitectures = "native";
    #       UMask = "0077";
    #       IPAddressDeny = "any";
    #     };
    #   };
    #
    #   systemd.services."systemd-ask-password-wall" = {
    #     serviceConfig = {
    #       ProtectSystem = "strict";
    #       ProtectHome = true;
    #       ProtectKernelTunables = true;
    #       ProtectKernelModules = true;
    #       ProtectControlGroups = true;
    #       ProtectKernelLogs = true;
    #       ProtectClock = true;
    #       ProtectProc = "invisible";
    #       ProcSubset = "pid";
    #       PrivateTmp = true;
    #       PrivateUsers = true;
    #       PrivateDevices = true;
    #       PrivateIPC = true;
    #       MemoryDenyWriteExecute = true;
    #       NoNewPrivileges = true;
    #       LockPersonality = true;
    #       RestrictRealtime = true;
    #       RestrictSUIDSGID = true;
    #       RestrictAddressFamilies = "AF_INET AF_INET6";
    #       RestrictNamespaces = true;
    #       SystemCallFilter = ["@system-service"]; # A more permissive filter
    #       SystemCallArchitectures = "native";
    #       UMask = "0077";
    #       IPAddressDeny = "any";
    #     };
    #   };
    }

    # Keybase Sensitive Secrets
    #
    # This requires a login in keybase before the user activation script
    # will complete successfully so it will not successfully setup all credentials
    # on the first run
    #
    # This produces a package called "sensitive" that can be ran at anytime to sync
    # credentials from the sensitive repo

    # (mkIf (cfg.copySensitive != {})
    #   (let
    #     sensitive = pkgs.writeScriptBin "sensitive" ''
    #       #!/usr/bin/env bash
    #       echo "Checking if sensitive repo is setup"
    #       if [[ ! -d /etc/sensitive ]]; then
    #         echo "[sensitive] cloning sensitive config from keybase"
    #         ${pkgs.git}/bin/git clone keybase://private/jordanfaust/sensitive /etc/sensitive
    #         if [[ $? -eq 0 ]]; then
    #           echo "[sensitive] clone complete"
    #         else
    #           echo "[sensitive] clone failed, login to keybase required"
    #         fi
    #       fi
    #       if [[ -d /etc/sensitive ]]; then
    #         echo "[sensitive] Copying sensitive credentials"
    #         ${concatStringsSep "\n"
    #         (mapAttrsToList (name: script: ''
    #             echo "[${name}]"
    #             ${script}
    #           '')
    #           cfg.copySensitive)}
    #       fi
    #     '';
    #   in {
    #     user.packages = [sensitive];
    #     system.userActivationScripts.sensitive = ''
    #       [ -z "$NORELOAD" ] && ${sensitive}/bin/sensitive
    #     '';
    #   }))
  ];
}
