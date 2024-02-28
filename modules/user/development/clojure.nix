{ config, lib, pkgs, inputs, osConfig, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.development.clojure;
  minimal = config.modules.minimal;
in
{
  options.modules.development.clojure = mkOption {
    description = ''
      Configurations for Clojure development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
          options = {
            enable = mkEnableOption "clojure";
          };
        }];
      });
    default = {
      enable = true;
    };
  };

  config = lib.mkIf (!minimal && cfg.enable) {
    home = {
      packages = with pkgs; [
        clojure
        clojure-lsp
        joker
        leiningen
      ];
    };
  };
}
