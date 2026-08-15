{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.vpn;
in {
  options.modules.applications.vpn = mkOption {
    description = ''
      Configuration for VPN clients.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "vpn";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  # VPN tooling is Linux-only: openconnect links against webkitgtk (broken on Darwin),
  # and gpauth/gpclient are Linux-specific GlobalProtect wrappers.
  # On macOS, GlobalProtect is installed via the native corporate app or MDM.
  config = mkIf (cfg.enable && pkgs.stdenv.hostPlatform.isLinux) {
    home = {
      packages = with pkgs; [
        openconnect
        gpauth
        gpclient
      ];

      sessionVariables = {
        TERMINAL = "kitty";
        TERM = "kitty";
      };
    };
  };
}
