# When I'm stuck in the terminal or don't have access to Emacs, (neo)vim is my
# go-to. I am a vimmer at heart, after all.

{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.vim;
    denoLibPath = super: self:
      self.glibc + (":" + super.lib.makeLibraryPath [self.webkitgtk]);
    denoOverlay = (self: super: {
      deno-webkit = super.deno.overrideAttrs (old: {
        propagatedBuildInputs = [
          self.webkitgtk
          self.gtk3
        ];
        # postFixup = ''
        #   patchelf --set-rpath ${(denoLibPath super self)} "$out/bin/deno" || true
        # '';
        # postInstall = ''
        #   wrapProgram "$out/bin/deno" --set WEBKIT_DISABLE_COMPOSITING_MODE 1
        # '';
      });
    });
in {
  options.modules.editors.vim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # nixpkgs.overlays = [
    #   inputs.neovim-nightly-overlay.overlay
    #   # (self: super: {
    #   #   neovim-nightly = super.neovim-nightly.overrideAttrs {
    #   #
    #   #   };
    #   # })
    # ];

    environment.systemPackages = with pkgs; [
      webkitgtk
    ];

    user.packages = with pkgs; [
      editorconfig-core-c
      # unstable.neovim
      deno
      # deno-webkit
      # neovim-nightly
      neovim

      # Neovim Addon Dependencies
      ranger 

      # Image preview
      chafa

      (makeDesktopItem {
        name = "Neovim";
        desktopName = "Neovim";
        genericName = "Text Editor";
        icon = "nvim";
        exec = "${kitty}/bin/kitty --title Neovim --class neovim -e nvim %F";
        categories = [ "Utility" "TextEditor" ];
      })
    ];

    # This is for non-neovim, so it loads my nvim config
    # env.VIMINIT = "let \\$MYVIMRC='\\$XDG_CONFIG_HOME/nvim/init.vim' | source \\$MYVIMRC";

    environment.shellAliases = {
      vim = "nvim";
      v   = "nvim";
    };
  };
}
