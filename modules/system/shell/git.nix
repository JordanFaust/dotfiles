{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.git;
  inherit (config.dotfiles) configDir;
in {
  options.modules.shell.git = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      gh
      git-open
      diff-so-fancy
      (mkIf config.modules.shell.gnupg.enable
        git-crypt)
      act
    ];

    home.configFile = {
      "git/config".source = "${configDir}/git/config";
      "git/ignore".source = "${configDir}/git/ignore";
      "git/attributes".source = "${configDir}/git/attributes";
    };

    modules.shell.zsh.rcFiles = ["${configDir}/git/aliases.zsh"];

    modules.security.copySensitive.git = ''
      if [[ ! -f $HOME/.ssh/github ]]; then
        echo "[sensitive] copying github private key"
        ${pkgs.coreutils}/bin/cp /etc/sensitive/github $HOME/.ssh/github
      fi
      if [[ ! -f $HOME/.ssh/github.pub ]]; then
        echo "[sensitive] copying github public key"
        ${pkgs.coreutils}/bin/cp /etc/sensitive/github.pub $HOME/.ssh/github.pub
      fi
    '';
  };
}
