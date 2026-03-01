# hosts/darwin/work/default.nix
#
# nix-darwin system configuration for the work MacBook (Apple Silicon).
# Activate with: darwin-rebuild switch --flake .#work
{
  config,
  lib,
  pkgs,
  ...
}: {
  # User account — name must match the macOS account name exactly.
  users.users.jordan = {
    home = "/Users/jordan";
    shell = pkgs.zsh;
  };

  # Host-specific Homebrew additions (supplements modules/darwin/homebrew.nix).
  homebrew.casks = [
    "raycast"
  ];
}
