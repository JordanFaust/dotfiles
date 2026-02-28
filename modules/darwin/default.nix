# modules/darwin/default.nix
#
# nix-darwin system configuration — imported explicitly by lib/darwin.nix.
# NOT under modules/system/ to avoid being auto-imported into NixOS builds
# by the root default.nix (which does mapModulesRec' ./modules/system).
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./homebrew.nix
    ./defaults.nix
  ];

  # Leave the Nix daemon entirely to Determinate Nix — do not conflict.
  # Do not set nix.package here; Determinate manages it.

  # Touch ID for sudo
  security.pam.enableSudoTouchIdAuth = true;

  environment.systemPackages = with pkgs; [
    git
    curl
    wget
    gnumake
    unzip
    vim
  ];

  programs.zsh.enable = true;

  # Required by nix-darwin — increment when nix-darwin breaks compatibility.
  system.stateVersion = 5;
}
