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

  # Always enable vpn, no matter the installation
  config = mkIf cfg.enable {
    home = {
      packages = with pkgs;
        [openconnect]
        ++ lib.optionals pkgs.stdenv.isLinux [
          # gpauth and gpclient are Linux-only GlobalProtect tools
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
