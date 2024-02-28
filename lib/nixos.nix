{ inputs, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;
let sys = "x86_64-linux";
in {
  mkHost = path: attrs @ { system ? sys, ... }:
    nixosSystem {
      inherit system;
      specialArgs = { inherit lib inputs system home-manager; };
      modules = [
        # Setup nixpkgs and establish hostname
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        # Filter system from the attributes as it has already been set?
        (filterAttrs (n: v: !elem n [ "system" ]) attrs)
        # Load the configuration in the defualt.nix file in the directory of the system
        ../.   # /default.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.jordan = import "${path}/home.nix";

          home-manager.extraSpecialArgs = { inherit inputs; };
        }
        # ../home.nix
        # ?
        (import path)
      ];
    };

  mapHosts = dir: attrs @ { system ? system, ... }:
    mapModules dir
      (hostPath: mkHost hostPath attrs);
}
