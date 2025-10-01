# flake.nix --- the heart of my dotfiles
#
# Author:  Henrik Lissner <contact@henrik.io>
# URL:     https://github.com/hlissner/dotfiles
# License: MIT
#
# Welcome to ground zero. Where the whole flake gets set up and all its modules
# are loaded.
{
  description = "A grossly incandescent nixos config.";

  inputs = {
    # Follow the latest and greatest by default
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprlock.url = "github:hyprwm/hyprlock";
    hypridle.url = "github:hyprwm/hypridle";
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    # Wrangler Latest
    wrangler.url = "github:ryand56/wrangler";

    # VSCode
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Catppuccin
    catppuccin.url = "github:catppuccin/nix";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    # FLake Formatter
    alejandra.url = "github:kamadorueda/alejandra/3.1.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";

    # Private fonts
    private-fonts.url = "git+ssh://git@github.com/JordanFaust/private-fonts.git?ref=main";
    private-fonts.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-stable,
    nixpkgs-unstable,
    home-manager,
    alejandra,
    nix-vscode-extensions,
    ...
  }: let
    inherit (lib.my) mapModules mapModulesRec mapHosts;

    system = "x86_64-linux";

    mkPkgs = pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };

    pkgs = mkPkgs nixpkgs [
      self.overlay
      inputs.nix-vscode-extensions.overlays.default
    ];
    pkgs' = mkPkgs nixpkgs-unstable [];
    pkgs-stable' = mkPkgs nixpkgs-stable [];

    lib =
      nixpkgs.lib.extend
      (self: super: {
        my = import ./lib {
          inherit pkgs inputs home-manager;
          lib = self;
        };
      });
  in rec {
    # Cachix Substituters
    nix.settings = {
      # substituters = [
      #   "https://wrangler.cachix.org"
      #   "https://hyprland.cachix.org"
      # ];
      # trusted-public-keys = [
      #   "wrangler.cachix.org-1:N/FIcG2qBQcolSpklb2IMDbsfjZKWg+ctxx0mSMXdSs="
      #   "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      # ];
    };

    lib = lib.my;

    overlay = final: prev: {
      # Tracking unstable everywhere by default. Adding this for legacy support and to allow
      # swapping pack to stable
      unstable = pkgs';
      stable = pkgs-stable';
      my = self.packages."${system}";
    };

    overlays =
      mapModules ./overlays import;

    #
    # Custom Packages
    #
    packages."${system}" =
      mapModules ./packages (p: pkgs.callPackage p {inherit inputs;});

    #
    # Custom Modules
    #
    nixosModules =
      {dotfiles = import ./.;} // mapModulesRec ./modules/system import;

    #
    # Host Configuration
    #
    nixosConfigurations =
      mapHosts ./hosts {};

    devShell."${system}" =
      import ./shell.nix {inherit pkgs;};

    defaultApp."${system}" = {
      type = "app";
      program = ./bin/hey;
    };
  };
}
