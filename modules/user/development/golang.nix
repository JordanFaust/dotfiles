{
  config,
  lib,
  pkgs,
  inputs,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.development.golang;
  minimal = config.modules.minimal;
in {
  options.modules.development.golang = mkOption {
    description = ''
      Configurations for Golang development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "golang";
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
        # The specific language version
        go
        # LSP
        gopls
        # protoc
        protobuf
        protobufc
        # Buf CLI
        buf
      ];

      sessionVariables = {
        BUF_USER = "jordanfaust";
        GOPRIVATE = "github.com/procore,$GOPRIVATE";
      };
    };
  };
}
