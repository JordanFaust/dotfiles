{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.ai;
  # configDir = config.dotfiles.configDir;
in {
  options.modules.services.ai = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ollama
    ];
    # Enable ollama locally and configure the models that are pulled by default
    services.ollama = {
      enable = true;
      loadModels = [
        "llama3.2"
        "nomic-embed-text"
      ];
    };
  };
}
