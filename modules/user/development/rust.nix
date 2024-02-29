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
  cfg = config.modules.development.rust;
  minimal = config.modules.minimal;
in {
  options.modules.development.rust = mkOption {
    description = ''
      Configurations for Rust development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "rust";
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
      packages = [pkgs.rustup];

      shellAliases = {
        rs = "rustc";
        rsp = "rustup";
        ca = "cargo";
      };

      sessionVariables = {
        RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        # PATH = ["$CARGO_HOME/bin"];
      };
    };
  };
}
