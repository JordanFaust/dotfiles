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

  inputs =
    {
      # Default to using packages within the stable 23.11 release
      # nixpkgs.url = "nixpkgs/nixos-23.11";
      # # Provide a means to install cutting edge packages where necessary
      # nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable";
      #
      # home-manager.url = "github:rycee/home-manager/release-23.11";
      # home-manager.inputs.nixpkgs.follows = "nixpkgs";

      nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";
      nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

      # Follow the latest and greatest by default
      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
      home-manager = {
        url = "github:nix-community/home-manager";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      # Hyperland Home Manager
      hyprland.url = "github:hyprwm/Hyprland";
      hyprland-plugins = {
        url = "github:hyprwm/hyprland-plugins";
        inputs.nixpkgs.follows = "hyprland";
      };

      matugen.url = "github:InioX/matugen";

      agenix.url = "github:ryantm/agenix";
      agenix.inputs.nixpkgs.follows = "nixpkgs";

      # Extras
      emacs-overlay.url  = "github:nix-community/emacs-overlay";
      # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
      nixos-hardware.url = "github:nixos/nixos-hardware";
      # SDDM + Theme
      sddm-catppuccin.url = "github:khaneliman/sddm-catppuccin";
      sddm-catppuccin.inputs.nixpkgs.follows = "nixpkgs";
      # AGS
      ags.url = "github:Aylur/ags";
      ags.inputs.nixpkgs.follows = "nixpkgs";
      # Support building deno applications
      deno2nix.url = "github:SnO2WMaN/deno2nix";
    };

outputs = inputs @ { self, nixpkgs, nixpkgs-stable, nixpkgs-unstable, home-manager, ... }:
  let
    inherit (lib.my) mapModules mapModulesRec mapHosts;

    system = "x86_64-linux";
    username = "jordan";

    mkPkgs = pkgs: extraOverlays: import pkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = extraOverlays ++ (lib.attrValues self.overlays);
    };
    pkgs  = mkPkgs nixpkgs [ self.overlay inputs.deno2nix.overlays.default ];
    pkgs' = mkPkgs nixpkgs-unstable [];
    pkgs-stable' = mkPkgs nixpkgs-stable [];

    lib = nixpkgs.lib.extend
      (self: super: { my = import ./lib { inherit pkgs inputs home-manager; lib = self; }; });
  in {
    lib = lib.my;

    overlay =
      final: prev: {
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
      mapModules ./packages (p: pkgs.callPackage p { inherit inputs; });
      # // { desktop = (pkgs.callPackage ./packages/ags.nix { inherit inputs; }); };

    #
    # Custom Modules
    #
    nixosModules =
      { dotfiles = import ./.; } // mapModulesRec ./modules import;

    #
    # Host Configuration
    #
    nixosConfigurations =
      mapHosts ./hosts {};

    #
    # Home Configuration
    #
    # homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
    #   inherit pkgs;
    #   extraSpecialArgs = { inherit inputs username lib; };
    #   modules = [ ./home-manager/home.nix ];
    # };

    devShell."${system}" =
      import ./shell.nix { inherit pkgs; };

    templates = {
      full = {
        path = ./.;
        description = "A grossly incandescent nixos config";
      };
    } // import ./templates;
    defaultTemplate = self.templates.full;

    defaultApp."${system}" = {
      type = "app";
      program = ./bin/hey;
    };
  };
}
