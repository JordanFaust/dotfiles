# modules/dev/golang.nix
{
  config,
  options,
  lib,
  pkgs,
  my,
  ...
}:
with lib;
with lib.my; let
  devCfg = config.modules.dev;
  cfg = devCfg.golang;
in {
  options.modules.dev.golang = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
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
      environment.variables = {
        BUF_USER = "jordanfaust";
        GOPRIVATE = "github.com/procore,$GOPRIVATE";
      };
    })
  ];
}
