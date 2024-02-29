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
  cfg = config.modules.development.ruby;
  minimal = config.modules.minimal;
in {
  options.modules.development.ruby = mkOption {
    description = ''
      Configurations for Ruby development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "ruby";
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

      sessionVariables = {
        GEM_PATH = "${pkgs.ruby_3_2}/lib/ruby/gems/3.2.0";
      };

      sessionPath = ["${pkgs.ruby_3_2}/lib/ruby/gems/3.2.0"];
    };
  };
}
