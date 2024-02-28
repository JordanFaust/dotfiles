{ config, lib, pkgs, inputs, osConfig, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.development.cc;
  minimal = config.modules.minimal;
in
{
  options.modules.development.cc = mkOption {
    description = ''
      Configurations for C development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
          options = {
            enable = mkEnableOption "cc";
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
        # clang
        gcc
        bear
        gdb
        cmake
        llvmPackages.libcxx
        libllvm
      ];
    };
  };
}
