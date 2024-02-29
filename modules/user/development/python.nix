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
  cfg = config.modules.development.python;
  minimal = config.modules.minimal;
in {
  options.modules.development.python = mkOption {
    description = ''
      Configurations for Python development.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "python";
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
        python3
        python3Packages.pip
        yamllint
        # python37Packages.pylint
      ];

      shellAliases = {
        py = "python";
        py2 = "python2";
        py3 = "python3";
      };
      sessionVariables = {
        PIP_CONFIG_FILE = "$XDG_CONFIG_HOME/pip/pip.conf";
        PIP_LOG_FILE = "$XDG_DATA_HOME/pip/log";
        PYLINTHOME = "$XDG_DATA_HOME/pylint";
        PYLINTRC = "$XDG_CONFIG_HOME/pylint/pylintrc";
        PYTHONSTARTUP = "$XDG_CONFIG_HOME/python/pythonrc";
        PYTHON_EGG_CACHE = "$XDG_CACHE_HOME/python-eggs";
      };
    };
  };
}
