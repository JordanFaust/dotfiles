# modules/dev/ruby.nix
#
{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
let devCfg = config.modules.dev;
    cfg = devCfg.ruby;
in {
  options.modules.dev.ruby = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
        # The specific language version
        ruby
        # Global gems
        rubyPackages.pry
        rubyPackages.pry-doc
        rubyPackages.solargraph
        rubyPackages.rubocop
      ];
    })
  ];
}
