{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.development.node;
  inherit (config.modules) minimal;
  # inherit (stdenv)
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
      packages = with pkgs;
        [
          bun
          deno
          nodejs_22
          yarn
          inputs.wrangler.packages.${pkgs.system}.wrangler
          biome
          turbo
        ]
        ++ lib.optionals pkgs.stdenv.isLinux [
          # cypress and playwright browsers are not available on aarch64-darwin
          cypress
          playwright.browsers
        ];

      # Run locally installed bin-script, e.g. n coffee file.coffee
      shellAliases = {
        n = "PATH=\"$(${pkgs.nodejs_22}/bin/npm bin):$PATH\"";
        ya = "yarn";
      };

      # sessionPath = ["$(${pkgs.yarn}/bin/yarn global bin)"];

      sessionVariables =
        {
          NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
          NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
          NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
          NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
        }
        // lib.optionalAttrs pkgs.stdenv.isLinux {
          # XDG_RUNTIME_DIR is set by systemd on Linux only
          NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
          # playwright.browsers is Linux-only; referencing the store path on Darwin
          # causes a "store path without context" warning via builtins.toJSON
          PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright.browsers}";
        };
    };
  };
}
