# modules/dev/node.nix --- https://nodejs.org/en/
#
# JS is one of those "when it's good, it's alright, when it's bad, it's a
# disaster" languages.

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let devCfg = config.modules.dev;
    cfg = devCfg.node;
in {
  options.modules.dev.node = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt devCfg.xdg.enable;
  };

  config = mkMerge [
    # (let node = pkgs.nodejs_18;
    (let node = pkgs.nodejs_20;
     in mkIf cfg.enable {
      user.packages = [
        node
        pkgs.yarn
        pkgs.nodePackages.pnpm
        # pkgs.unstable.nodePackages.wrangler
        # pkgs.wrangler
        # (builtins.getFlake "github:NixOS/nixpkgs/8dfad603247387df1df4826b8bea58efc5d012d8").legacyPackages.${pkgs.system}.nodePackages.wrangler
        pkgs.unstable.nodePackages.wrangler
        pkgs.nodePackages.typescript
        # pkgs.turbo
        # (builtins.getFlake "github:NixOS/nixpkgs/8dfad603247387df1df4826b8bea58efc5d012d8").legacyPackages.${pkgs.system}.turbo
      ];

      # Run locally installed bin-script, e.g. n coffee file.coffee
      environment.shellAliases = {
        n  = "PATH=\"$(${node}/bin/npm bin):$PATH\"";
        ya = "yarn";
      };

      env.PATH = [ "$(${pkgs.yarn}/bin/yarn global bin)" ];
    })

    (mkIf cfg.xdg.enable {
      env.NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
      env.NPM_CONFIG_CACHE      = "$XDG_CACHE_HOME/npm";
      env.NPM_CONFIG_TMP        = "$XDG_RUNTIME_DIR/npm";
      env.NPM_CONFIG_PREFIX     = "$XDG_CACHE_HOME/npm";
      env.NODE_REPL_HISTORY     = "$XDG_CACHE_HOME/node/repl_history";

      # home.configFile."npm/config".text = ''
      #   cache=$XDG_CACHE_HOME/npm
      #   prefix=$XDG_DATA_HOME/npm
      #   //artifacts.procoretech.com/artifactory/api/npm/npm/:_auth=$(${pkgs.coreutils-full}/bin/coreutils --coreutils-prog=cat /etc/sensitive/procore-npm)
      # '';
    })
  ];
}