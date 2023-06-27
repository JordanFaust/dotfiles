{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.emacs;
    configDir = config.dotfiles.configDir;
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    doom = rec {
      enable = mkBoolOpt false;
      repoUrl = mkOpt types.str "https://github.com/doomemacs/doomemacs";
      configRepoUrl = mkOpt types.str "https://github.com/JordanFaust/doom-config";
    };
  };

  config = mkIf cfg.enable {
    # Configure the overlay for emacs
    # nixpkgs.overlays = [
    #   inputs.emacs-overlay.overlay
    #   (self: super: {
    #     emacsPgtkNativeComp = super.emacsPgtkNativeComp.override {
    #       # Enable Xwidget support
    #       withXwidgets = true;
    #       # Enable GTK3 support for webkit
    #       withGTK3 = true;
    #     };
    #     emacs-unstable = super.emacs-unstable.override {
    #       # Enable Xwidget support
    #       withXwidgets = true;
    #       # Enable GTK3 support for webkit
    #       withGTK3 = true;
    #       # Enable SQLite3 integration
    #       withSQLite3 = true;
    #       # Enable native-comp
    #       withNativeCompilation = true;
    #     };
    #   })
    # ];

    user.packages = with pkgs; [
      ## Emacs itself
      binutils       # native-comp needs 'as', provided by this
      # 29 + pgtk + native-comp
      # ((emacsPackagesFor emacsPgtkNativeComp).emacsWithPackages (epkgs: [
      #   epkgs.vterm
      # ]))
      # webkitgtk
      # (makeDesktopItem {
      #   name = "Doom Emacs";
      #   desktopName = "Doom Emacs";
      #   icon = "emacs";
      #   exec = "${emacsPgtkNativeComp}/bin/emacs --name doom";
      #   categories = [ "Development" "TextEditor" ];
      # })
      # 28.1 + sql + gtk3 + webkit + webp + native-comp
      xdg-desktop-portal
      # ((emacsPackagesFor emacsUnstable).emacsWithPackages (epkgs: [
      #   epkgs.vterm
      # ]))
      # emacsUnstable
      webkitgtk
      # (makeDesktopItem {
      #   name = "Doom Emacs";
      #   desktopName = "Doom Emacs";
      #   icon = "emacs";
      #   exec = "${emacs-unstable}/bin/emacs --name doom";
      #   categories = [ "Development" "TextEditor" ];
      # })
      emacs-all-the-icons-fonts

      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})
      gnutls              # for TLS connectivity

      ## Optional dependencies
      fd                  # faster projectile indexing
      imagemagick         # for image-dired
      (mkIf (config.programs.gnupg.agent.enable)
        pinentry_emacs)   # in-emacs gnupg prompts
      zstd                # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [
        en en-computers en-science
      ]))

      # :tools editorconfig
      editorconfig-core-c # per-project style config
      # :tools terraform
      terraform
      # :tools lookup & :lang org +roam
      sqlite
      # needed to compile sqlite for org roam
      gcc

      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      # :lang markdown
      pandoc
      # TODO
      # :lang org
      maim
    ];

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    modules.shell.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # Leaving this for historical reference. Allowing Nix to own my emacs
    # configuration allows it to be rolled into the rebuild process. It does this
    # at the cost of easy iteration of my configuration. Until my emacs configuration
    # stabilizes this will live outside of nix within it's own repo
    # home.configFile = {
    #   # "doom" = { source = "${configDir}/doom"; recursive = true; };
    # };

    # Clones Doom Emacs and my personal configuration. This does not include the
    # steps to install doom and sync/build the packages from my personal config
    system.userActivationScripts = mkIf cfg.doom.enable {
      installDoomEmacs = ''
        #!${pkgs.stdenv.shell}
        echo "Checking if Doom Emacs is installed"
        if [ ! -f "$XDG_CONFIG_HOME/emacs/bin/doom" ]; then
          echo "Cloning Doom Emacs"
          ${pkgs.git}/bin/git clone --depth=1 --single-branch "${cfg.doom.repoUrl}" "$XDG_CONFIG_HOME/emacs"
          echo "Cloning Doom Config"
          ${pkgs.git}/bin/git clone "${cfg.doom.configRepoUrl}" "$XDG_CONFIG_HOME/doom"
        fi
      '';
    };
  };
}
