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
    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
    nix-darwin,
    alejandra,
    nix-vscode-extensions,
    ...
  }: let
    inherit (lib.my) mapModules mapModulesRec mapHosts mapDarwinHosts;

    linuxSystem  = "x86_64-linux";
    darwinSystem = "aarch64-darwin";

    mkPkgs = system: pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };

    pkgs        = mkPkgs linuxSystem  nixpkgs [ self.overlay inputs.nix-vscode-extensions.overlays.default ];
    pkgs'       = mkPkgs linuxSystem  nixpkgs-unstable [];
    pkgs-stable' = mkPkgs linuxSystem nixpkgs-stable [];
    pkgs-darwin  = mkPkgs darwinSystem nixpkgs [ self.overlay inputs.nix-vscode-extensions.overlays.default ];

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
      my = self.packages."${linuxSystem}";
    };

    overlays =
      mapModules ./overlays import;

    #
    # Custom Packages
    #
    packages."${linuxSystem}" =
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
      mapHosts ./hosts/nixos {};

    darwinConfigurations = {};

    devShell."${linuxSystem}" =
      import ./shell.nix {inherit pkgs;};

    defaultApp."${linuxSystem}" = {
      type = "app";
      program = ./bin/hey;
    };
  };
}
