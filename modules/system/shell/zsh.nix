{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.zsh;
in {
  # Keep option declarations so modules that still set rcFiles/rcInit
  # (e.g. services/docker.nix) don't fail evaluation. These options are
  # accepted but not consumed here — Phase 2 will migrate those callers.
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;
    aliases = mkOpt (attrsOf (either str path)) {};
    rcInit = mkOpt' lines "" "Deprecated — use programs.zsh.initExtra in a user module";
    envInit = mkOpt' lines "" "Deprecated — use programs.zsh.envExtra in a user module";
    rcFiles = mkOpt (listOf (either str path)) [];
    envFiles = mkOpt (listOf (either str path)) [];
  };

  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.zsh;

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableGlobalCompInit = false;
      promptInit = "";
    };
  };
}
