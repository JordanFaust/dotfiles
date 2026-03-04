{
  inputs,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  sys = "aarch64-darwin";
in {
  mkDarwinHost = path: attrs @ {
    system ? sys,
    pkgs ? (throw "pkgs-darwin must be passed to mapDarwinHosts"),
    user ? "jordan",
    ...
  }: let
    specialArgs = {inherit lib inputs system home-manager;};
  in
    inputs.nix-darwin.lib.darwinSystem {
      inherit system;
      specialArgs = specialArgs;
      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path));
          system.primaryUser = user;
        }
        (filterAttrs (n: v: !elem n ["system" "pkgs" "user"]) attrs)
        ../modules/darwin
        inputs.home-manager.darwinModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.${user} = import "${path}/home.nix";
            extraSpecialArgs = {inherit (specialArgs) inputs system;};
          };
        }
        (import path)
      ];
    };

  mapDarwinHosts = dir: attrs @ {system ? sys, ...}:
    mapModules dir (hostPath: mkDarwinHost hostPath attrs);
}
