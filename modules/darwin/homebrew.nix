# modules/darwin/homebrew.nix
#
# Declarative Homebrew management via nix-darwin.
# Homebrew itself must be pre-installed: https://brew.sh
# All casks and brews listed here are installed on `darwin-rebuild switch`.
# Apps NOT in this list are removed when cleanup = "zap".
_: {
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      upgrade = true;
      # "zap" removes unlisted casks on activation.
      # Set to "none" temporarily if you want to test without removing apps.
      cleanup = "zap";
      # newer brew bundle requires --force when --cleanup is passed
      extraFlags = ["--force"];
    };
    casks = [
      "ghostty"
      "claude-code@latest"
      # Add work-required GUI apps here, e.g.:
      # "zoom"
      # "slack"
      # "1password"
    ];
    brews = [
      # CLI tools not available in nixpkgs, e.g.:
      "opencode"
    ];
  };
}
