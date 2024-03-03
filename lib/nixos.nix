{
  inputs,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  sys = "x86_64-linux";
in {
  mkHost = path: attrs @ {system ? sys, ...}:
    let
      specialArgs = {lib = lib; inputs = inputs; system = system; home-manager = home-manager;};
    in
    nixosSystem {
      inherit system;
      specialArgs = {inherit (specialArgs) lib inputs system home-manager;};
      modules = [
        # Setup nixpkgs and establish hostname
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        # Filter system from the attributes as it has already been set?
        (filterAttrs (n: v: !elem n ["system"]) attrs)
        # Add home-manager as a system module, allow using it within user modules and
        # granting access to the capabilities of home-manager within system modules.
        # Load the default.nix configuration at the root of the repo. This includes setup
        # configuration common for all systems and establishes basic configuration of
        # used within both system and user modules via home-manager.
        ../. # /default.nix
        # Load the configuration for home-manager for the target user. The import for the
        # target user will land home package configurations.
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.jordan = import "${path}/home.nix";
          # home-manager.users.modules = modules;

          home-manager.extraSpecialArgs = {inherit (specialArgs) inputs system;};
        }
        # import the configuration within the target host directory. This will start by
        # evaluating the ./host/path/default.nix which should include the appropriate imports
        # of the other configurations, such as hardware-configuration.nix.
        (import path)
      ];
    };

  mapHosts = dir: attrs @ {system ? system, ...}:
    mapModules dir
    (hostPath: mkHost hostPath attrs);
}
