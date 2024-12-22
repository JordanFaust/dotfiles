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

    # Hyperland Home Manager
    hyprland.url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    hyprland-plugins.url = "github:hyprwm/hyprland-plugins";
    hyprland-plugins.inputs.hyprland.follows = "hyprland";
    hyprlock.url = "github:hyprwm/hyprlock";
    hypridle.url = "github:hyprwm/hypridle";
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel/955eed6c60a3ea5d6b0b1b8b7086cffbae984277";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

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
    ...
  }: let
    inherit (lib.my) mapModules mapModulesRec mapHosts;

    system = "x86_64-linux";
    username = "jordan";

    mkPkgs = pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };

    pkgs = mkPkgs nixpkgs [
      self.overlay
      inputs.hyprpanel.overlay
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

    # templates =
    #   {
    #     full = {
    #       path = ./.;
    #       description = "A grossly incandescent nixos config";
    #     };
    #   }
    #   // import ./templates;
    # defaultTemplate = self.templates.full;

    defaultApp."${system}" = {
      type = "app";
      program = ./bin/hey;
    };
  };
}
