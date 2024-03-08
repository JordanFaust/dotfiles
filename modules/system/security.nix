{
  config,
  options,
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

        ## TCP hardening
        # Prevent bogus ICMP errors from filling up logs.
        "net.ipv4.icmp_ignore_bogus_error_responses" = 1;
        # Reverse path filtering causes the kernel to do source validation of
        # packets received from all interfaces. This can mitigate IP spoofing.
        "net.ipv4.conf.default.rp_filter" = 1;
        "net.ipv4.conf.all.rp_filter" = 1;
        # Do not accept IP source route packets (we're not a router)
        "net.ipv4.conf.all.accept_source_route" = 0;
        "net.ipv6.conf.all.accept_source_route" = 0;
        # Don't send ICMP redirects (again, we're on a router)
        "net.ipv4.conf.all.send_redirects" = 0;
        "net.ipv4.conf.default.send_redirects" = 0;
        # Refuse ICMP redirects (MITM mitigations)
        "net.ipv4.conf.all.accept_redirects" = 0;
        "net.ipv4.conf.default.accept_redirects" = 0;
        "net.ipv4.conf.all.secure_redirects" = 0;
        "net.ipv4.conf.default.secure_redirects" = 0;
        "net.ipv6.conf.all.accept_redirects" = 0;
        "net.ipv6.conf.default.accept_redirects" = 0;
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
      security.pam.services.keybase.enableGnomeKeyring = true;

      user.packages = with pkgs; [
        # Setup Keybase for use of storing sensitive credentials
        kbfs
        openssl
        # Security scanning tools
        vulnix
        sbomnix
        grype
        syft
        (pkgs.writeShellScriptBin "nixos-scan" ''
          mkdir -p /etc/dotfiles/reports

          echo "Generating meta information.."
          nix-env -qa --meta --json '.*' > /etc/dotfiles/reports/meta.json

          echo "Generating SBOM..."
          ${sbomnix}/bin/sbomnix /run/current-system/sw/ \
            --csv /etc/dotfiles/reports/sbom.csv \
            --cdx /etc/dotfiles/reports/sbox.cdx.json \
            --spdx /etc/dotfiles/reports/sbom.spdx.json \
            --meta /etc/dotfiles/reports/meta.json

          echo "Evaluating vulnerabilities..."
          ${grype}/bin/grype sbom:/etc/dotfiles/reports/sbom.spdx.json --add-cpes-if-none
        '')
      ];
      services.keybase.enable = true;
    }

    # Keybase Sensitive Secrets
    #
    # This requires a login in keybase before the user activation script
    # will complete successfully so it will not successfully setup all credentials
    # on the first run
    #
    # This produces a package called "sensitive" that can be ran at anytime to sync
    # credentials from the sensitive repo

    (mkIf (cfg.copySensitive != {})
      (let
        sensitive = pkgs.writeScriptBin "sensitive" ''
          #!/usr/bin/env bash
          echo "Checking if sensitive repo is setup"
          if [[ ! -d /etc/sensitive ]]; then
            echo "[sensitive] cloning sensitive config from keybase"
            ${pkgs.git}/bin/git clone keybase://private/jordanfaust/sensitive /etc/sensitive
            if [[ $? -eq 0 ]]; then
              echo "[sensitive] clone complete"
            else
              echo "[sensitive] clone failed, login to keybase required"
            fi
          fi
          if [[ -d /etc/sensitive ]]; then
            echo "[sensitive] Copying sensitive credentials"
            ${concatStringsSep "\n"
            (mapAttrsToList (name: script: ''
                echo "[${name}]"
                ${script}
              '')
              cfg.copySensitive)}
          fi
        '';
      in {
        user.packages = [sensitive];
        system.userActivationScripts.sensitive = ''
          [ -z "$NORELOAD" ] && ${sensitive}/bin/sensitive
        '';
      }))
  ];
}
