{
  config,
  options,
  lib,
  home-manager,
  ...
}:
with lib;
with lib.my; {
  options = with types; {
    user = mkOpt attrs {};

    dotfiles = {
      dir =
        mkOpt path
        # We need to drop /mnt in the evaluated directories as the configuration is fully built.
        # On reboot, the mounted volume will exist as the root directory.
        (removePrefix "/mnt"
          # Find the first path that can contain the root config directory.
          # ../../. - This transits manually from this current file up to the root of the repo
          (findFirst pathExists (toString ../../.) [
            # This may be the moved location of the config files after an install but before a reboot
            "/mnt/etc/dotfiles"
            # This is the expected location of the root of this flake
            "/etc/dotfiles"
          ]));
      binDir = mkOpt path "${config.dotfiles.dir}/bin";
      configDir = mkOpt path "${config.dotfiles.dir}/config";
      modulesDir = mkOpt path "${config.dotfiles.dir}/modules";
      themesDir = mkOpt path "${config.dotfiles.modulesDir}/system/themes";
    };

    home = {
      file = mkOpt' attrs {} "Files to place directly in $HOME";
      configFile = mkOpt' attrs {} "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOpt' attrs {} "Files to place in $XDG_DATA_HOME";
    };

    user-options = mkOpt' attrs {} "Primary user options";

    env = mkOption {
      type = attrsOf (oneOf [str path (listOf (either str path))]);
      apply =
        mapAttrs
        (n: v:
          if isList v
          then concatMapStringsSep ":" (x: toString x) v
          else (toString v));
      default = {};
      description = "TODO";
    };
  };

  config = {
    user = let
      user = builtins.getEnv "USER";
      name =
        if elem user ["" "root"]
        then "jordan"
        else user;
    in {
      inherit name;
      description = "Jordan Faust";
      extraGroups = [
        "qemu-libvirtd"
        "libvirtd"
        "wheel"
        "video"
        "audio"
        "disk"
        "networkmanager"
        "greeter"
      ];
      isNormalUser = true;
      home = "/home/${name}";
      group = "users";
      uid = 1000;
    };

    # Install user packages to /etc/profiles instead. Necessary for
    # nixos-rebuild build-vm to work.
    home-manager = {
      useUserPackages = true;

      # I only need a subset of home-manager's capabilities. That is, access to
      # its home.file, home.xdg.configFile and home.xdg.dataFile so I can deploy
      # files easily to my $HOME, but 'home-manager.users.jordan.home.file.*'
      # is much too long and harder to maintain, so I've made aliases in:
      #
      #   home.file        ->  home-manager.users.jordan.home.file
      #   home.configFile  ->  home-manager.users.jordan.home.xdg.configFile
      #   home.dataFile    ->  home-manager.users.jordan.home.xdg.dataFile
      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          # Necessary for home-manager to work with flakes, otherwise it will
          # look for a nixpkgs channel.
          stateVersion = config.system.stateVersion;
        };
        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    user-options = config.home-manager.users.${config.user.name};

    nix.settings = let
      users = ["root" config.user.name];
    in {
      trusted-users = users;
      allowed-users = users;
      auto-optimise-store = true;
    };

    # must already begin with pre-existing PATH. Also, can't use binDir here,
    # because it contains a nix store path.
    env.PATH = ["$DOTFILES_BIN" "$XDG_BIN_HOME" "$PATH"];

    environment.extraInit =
      concatStringsSep "\n"
      (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
  };
}
