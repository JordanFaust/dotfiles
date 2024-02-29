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
  cfg = config.modules.development.lua;
  minimal = config.modules.minimal;
in {
  options.modules.development.lua = mkOption {
    description = ''
      Configurations for Lua development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "lua";
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
        lua
        # luajit
        luaformatter
        sumneko-lua-language-server
        stylua
        lua54Packages.luarocks
        # luaPackages.moonscript
        # (mkIf cfg.love2D.enable love2d)
      ];
    };
  };
}
