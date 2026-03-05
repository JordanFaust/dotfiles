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
    ./services/docker.nix
    ./services/aerospace.nix
  ];

  # Determinate Nix manages the daemon — disable nix-darwin's own Nix management
  # entirely to prevent the "Determinate detected, aborting activation" error.
  # nix.package, nix.settings, and nix.enable must not be set here.
  nix.enable = false;

  # Touch ID for sudo (nix-darwin >= 2024 uses pam.services.sudo_local)
  security.pam.services.sudo_local.touchIdAuth = true;

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
