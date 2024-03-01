{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.git;
  configDir = config.dotfiles.configDir;
in {
  options.modules.shell.git = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      gitAndTools.git-annex
      gitAndTools.gh
      gitAndTools.git-open
      gitAndTools.diff-so-fancy
      (mkIf config.modules.shell.gnupg.enable
        gitAndTools.git-crypt)
      act
    ];

    # home.configFile = {
    #  "git/config".source = "${configDir}/git/config";
    #  "git/ignore".source = "${configDir}/git/ignore";
    #  "git/attributes".source = "${configDir}/git/attributes";
    # };

    # modules.shell.zsh.rcFiles = ["${configDir}/git/aliases.zsh"];

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
