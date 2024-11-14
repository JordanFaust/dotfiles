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
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.11";
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
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";
    # hyprlock.inputs.hyprland.follows = "hyprland";
    xdg-desktop-portal-hyprland.url = "github:hyprwm/xdg-desktop-portal-hyprland";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    # Support building deno applications
    deno2nix.url = "github:SnO2WMaN/deno2nix";

    # FLake Formatter
    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
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
      inputs.deno2nix.overlays.default
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
      # TODO: remove this once mutagen resolves the sha collision
      matugen = final.rustPlatform.buildRustPackage rec {
        pname = "matugen";
        version = "2.4.0";

        src = final.fetchFromGitHub {
          owner = "InioX";
          repo = "matugen";
          rev = "refs/tags/v${version}";
          hash = "sha256-l623fIVhVCU/ylbBmohAtQNbK0YrWlEny0sC/vBJ+dU=";
        };

        cargoHash = "sha256-FwQhhwlldDskDzmIOxhwRuUv8NxXCxd3ZmOwqcuWz64=";

        meta = {
          description = "Material you color generation tool";
          homepage = "https://github.com/InioX/matugen";
          changelog = "https://github.com/InioX/matugen/blob/${src.rev}/CHANGELOG.md";
          license = final.lib.licenses.gpl2Only;
          maintainers = with final.lib.maintainers; [lampros];
          mainProgram = "matugen";
        };
      };
    };

    overlays =
      mapModules ./overlays import;

    #
    # Custom Packages
    #
    packages."${system}" =
      mapModules ./packages (p: pkgs.callPackage p {inherit inputs;});
    # // { desktop = (pkgs.callPackage ./packages/ags.nix { inherit inputs; }); };

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

    #
    # Home Configuration
    #
    # homeConfigurations.${username} = home-manager.lib.homeManagerConfiguration {
    #   inherit pkgs;
    #   extraSpecialArgs = { inherit inputs username lib; };
    #   modules = [ ./home-manager/home.nix ];
    # };

    devShell."${system}" =
      import ./shell.nix {inherit pkgs;};

    templates =
      {
        full = {
          path = ./.;
          description = "A grossly incandescent nixos config";
        };
      }
      // import ./templates;
    defaultTemplate = self.templates.full;

    defaultApp."${system}" = {
      type = "app";
      program = ./bin/hey;
    };
  };
}
