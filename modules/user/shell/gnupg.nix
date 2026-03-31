{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = mkOption {
    description = "GnuPG user configuration";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "gnupg";
              cacheTTL = mkOption {
                type = types.int;
                default = 3600;
                description = "GPG agent cache TTL in seconds";
              };
            };
          }
        ];
      });
    default = {};
  };

  config = mkIf cfg.enable {
    home.packages = lib.optionals pkgs.stdenv.isLinux [pkgs.tomb];

    programs.gpg = {
      enable = true;
      homedir = "${config.xdg.configHome}/gnupg";
    };

    services.gpg-agent = lib.mkIf pkgs.stdenv.isLinux {
      enable = true;
      defaultCacheTtl = cfg.cacheTTL;
    };
  };
}
