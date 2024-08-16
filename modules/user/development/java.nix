{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.development.java;
  minimal = config.modules.minimal;
in {
  options.modules.development.java = mkOption {
    description = ''
      Configurations for Java development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "java";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  config = lib.mkIf (!minimal && cfg.enable) {
    home = {
      packages = with pkgs; [
        jdk17
        jdt-language-server
      ];
    };
  };
}
