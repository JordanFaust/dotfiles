{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
    cfg = config.modules.desktop.apps.globalprotect;
    vpn = pkgs.writeScriptBin "vpn" ''
        #!${pkgs.stdenv.shell}
        prog_name="$(basename $0)"
        pid_file=/tmp/vpn
        vpn_portal=$(${pkgs.coreutils}/bin/cat /etc/sensitive/vpn-portal)
        vpn_gateway=$(${pkgs.coreutils}/bin/cat /etc/sensitive/vpn-gateway)
        log=/tmp/vpn.log

        function help() {
            echo "Usage: $prog_name -h [start|stop|toggle|status]"
            echo
            echo "Options"
            echo "    -h      Show help prompt"
            echo "    start   Launch the SAML login and connect to the VPN"
            echo "    stop    stop the SAML login process and the OpenConnect VPN"
            echo "    toggle  toggle the state of the VPN"
            echo "    status  report the status of the VPN"
        }

        function start() {
            if [[ ! -f $log ]]; then
                touch $log
            fi
            if [[ -f $pid_file ]]; then
                echo "VPN already started" | ${pkgs.coreutils}/bin/tee -a $log >/dev/null
                exit 1
            fi
            echo "Starting the VPN" | ${pkgs.coreutils}/bin/tee -a $log >/dev/null
            echo "${pkgs.gp-saml-gui}/bin/gp-saml-gui --allow-insecure-crypto -S -v --clientos=Linux $vpn_portal -- --usergroup=portal:prelogin-cookie --authgroup=\"$vpn_gateway\" --csd-wrapper=${pkgs.openconnect}/libexec/openconnect/hipreport.sh &> $log &"
            ${pkgs.gp-saml-gui}/bin/gp-saml-gui -vvv --clientos=Linux -S $vpn_portal -- -v --timestamp --usergroup=portal:prelogin-cookie --authgroup="$vpn_gateway" --csd-wrapper=${pkgs.openconnect}/libexec/openconnect/hipreport.sh &> $log &
            vpn_pid=$!
            touch $pid_file
            echo $vpn_pid > $pid_file
        }

        function stop() {
            echo "Stopping the VPN" | ${pkgs.coreutils}/bin/tee -a $log >/dev/null
            if [[ -f $pid_file ]]; then
                echo "Stopping the GUI" | ${pkgs.coreutils}/bin/tee -a $log >/dev/null
                kill -9 $(${pkgs.coreutils}/bin/cat $pid_file)
                rm -f $pid_file
            fi
            if [[ $(ip link list | grep tun0) ]]; then
                echo "Stopping OpenConnect" | ${pkgs.coreutils}/bin/tee -a $log >/dev/null
                sudo -A pkill -SIGINT openconnect
            fi
        }

        function toggle() {
            if [[ -f $pid_file || $(ip link list | grep tun0) ]]; then
                stop
                return 0
            fi
            start
        }

        function status() {
            if [[ $(ip link list | grep tun0) ]]; then
                echo ""
            else
                echo ""
            fi
        }

        subcommand=$1
        case $subcommand in
            "" | "-h" | "--help")
            help
            ;;
            "start")
            start
            ;;
            "stop")
            stop
            ;;
            "toggle")
            toggle
            ;;
            "status")
            status
            ;;
            *)
            echo "Error: '$subcommand' is not a know command." >&2
            echo "       Run '$prog_name --help' for a list of known command." >&2
            exit 1
            ;;
        esac
    '';
in {
  options.modules.desktop.apps.globalprotect = lib.mkOption {
    default = {};
    description = ''
      Enable the Global Protect and the SAML GUI VPN client.
    '';
    type = lib.types.submodule {
      options = {
        enable = mkEnableOption "globalprotect";
      };
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      unstable.openconnect
      my.gp-saml-gui
      vpn
      globalprotect-openconnect
    ];

    services.gnome.at-spi2-core.enable = true;
    services.globalprotect = {
      enable = true;
      # if you need a Host Integrity Protection report
      csdWrapper = "${pkgs.openconnect}/libexec/openconnect/hipreport.sh";
    };
  };
}
