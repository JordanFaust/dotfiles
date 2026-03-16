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
}:
let
  privateFontsPkgs = inputs.private-fonts.packages or {};
  # MonoLisa from private flake — available for aarch64-darwin if the input exists.
  monolisaPkg = lib.optional (privateFontsPkgs ? aarch64-darwin)
    privateFontsPkgs.aarch64-darwin.monolisa-variable;
in {
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
  # Only usable when the lid is open — falls back to password otherwise.
  security.pam.services.sudo_local.touchIdAuth = true;

  # Extend sudo session timeout from the default 5 minutes to 60 minutes.
  # Avoids repeated password prompts during darwin-rebuild or multi-step nix
  # workflows, particularly when using an external keyboard (no Touch ID).
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=60
  '';

  # Install fonts system-wide so macOS CoreText (and every native app,
  # including sketchybar) can find them.  Home Manager's home.packages only
  # puts fonts into ~/.nix-profile/share/fonts/ which fontconfig reads but
  # CoreText does NOT.  fonts.packages is the nix-darwin equivalent of
  # NixOS's fonts.packages — it registers fonts under /Library/Fonts/Nix Fonts/.
  fonts.packages = with pkgs; [
    nerd-fonts.symbols-only   # Nerd Font glyph coverage for sketchybar icons
    nerd-fonts.caskaydia-cove # CaskaydiaCove NF (monospaced fallback)
  ] ++ monolisaPkg;           # MonoLisa Variable (private) if available

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
