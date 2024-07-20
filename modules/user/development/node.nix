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
  cfg = config.modules.development.node;
  minimal = config.modules.minimal;
in {
  options.modules.development.node = mkOption {
    description = ''
      Configurations for Node development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "node";
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
        nodejs_20
        corepack
        # yarn
        # nodePackages.pnpm
        # pkgs.unstable.nodePackages.wrangler
        # pkgs.wrangler
        # (builtins.getFlake "github:NixOS/nixpkgs/a9858885e197f984d92d7fe64e9fff6b2e488d40#nodePackages.wrangler")
        (builtins.getFlake "github:NixOS/nixpkgs/a9858885e197f984d92d7fe64e9fff6b2e488d40").legacyPackages.${pkgs.system}.nodePackages.wrangler
        # stable.nodePackages.wrangler
        nodePackages.typescript
        cypress

        biome
        # pkgs.turbo
        # (builtins.getFlake "github:NixOS/nixpkgs/8dfad603247387df1df4826b8bea58efc5d012d8").legacyPackages.${pkgs.system}.turbo
      ];

      # Run locally installed bin-script, e.g. n coffee file.coffee
      shellAliases = {
        n = "PATH=\"$(${node}/bin/npm bin):$PATH\"";
        ya = "yarn";
      };

      # sessionPath = ["$(${pkgs.yarn}/bin/yarn global bin)"];

      sessionVariables = {
        NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
        NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
        NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
        NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
        NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
      };
    };
  };
}
