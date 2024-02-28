{ config, lib, pkgs, inputs, osConfig, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.applications.instant-messangers;
  minimal = config.modules.minimal;
in
{
  options.modules.applications.instant-messangers = mkOption {
    description = ''
      Configurations for instant messangers, such as slack.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [{
          options = {
            #
            # Launcher
            #
            enable = mkEnableOption "instant-messangers";

            #
            # Slack
            #
            slack = {
              enable = mkEnableOption "slack";
            };
          };
        }];
      });
    default = {
      enable = true;
      slack.enable = true;
    };
  };

  # Add configured instant messangers if this isn't a minimal install and instant messangers are enabled.
  config = mkIf (!minimal && cfg.enable) {
    home = {
      packages = with pkgs; [
        slack
      ];
    };
  };
}
