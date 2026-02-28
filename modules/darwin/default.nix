# modules/darwin/default.nix
#
# nix-darwin system configuration — imported explicitly by lib/darwin.nix.
# NOT under modules/system/ to avoid being auto-imported into NixOS builds
# by the root default.nix (which does mapModulesRec' ./modules/system).
#
# Populated in full by subsequent tasks (homebrew, defaults, etc.).
{ ... }: { }
