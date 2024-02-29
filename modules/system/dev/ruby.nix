# modules/dev/ruby.nix
#
{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  devCfg = config.modules.dev;
  cfg = devCfg.ruby;
in {
  options.modules.dev.ruby = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt devCfg.xdg.enable;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
        # The specific language version
        ruby_3_2
        # Global gems
        rubyPackages.pry
        rubyPackages.pry-doc
        rubyPackages_3_2.solargraph
        rubyPackages_3_2.rubocop
        # RE2 regular expression library used for a few Ruby depependnecies
        re2
        # needed for YAML c dependnecies
        libtool
        # Use the following to configure the local bundle config to find the libyaml headers
        #  bundle config build.psych --with-yaml-0.1-dir=$(nix eval nixpkgs#libyaml.outPath --raw)
        libyaml
        bundix
        my.ruby-lsp
      ];
      env.GEM_PATH = ["${pkgs.ruby_3_2}/lib/ruby/gems/3.2.0"];
      env.PATH = ["${pkgs.ruby_3_2}/lib/ruby/gems/3.2.0"];
    })

    (mkIf cfg.xdg.enable {
      })
  ];
}
