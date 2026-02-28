# modules/darwin/defaults.nix
#
# macOS system preferences set declaratively via nix-darwin.
# Run `darwin-rebuild switch` to apply. Some changes require logout/reboot.
# Full option list: https://daiderd.com/nix-darwin/manual/index.html#opt-system.defaults
{...}: {
  system.defaults = {
    NSGlobalDomain = {
      # Fastest key repeat (helps with vim-style navigation)
      KeyRepeat = 1;
      InitialKeyRepeatDelay = 10;
      # Expand save and print panels by default
      NSNavPanelExpandedStateForSaveMode = true;
      PMPrintingExpandedStateForPrint = true;
    };

    dock = {
      autohide = true;
      show-recents = false;
      # Do not rearrange spaces based on most recent use
      mru-spaces = false;
    };

    finder = {
      AppleShowAllExtensions = true;
      # Column view
      FXPreferredViewStyle = "clmv";
      ShowPathbar = true;
    };

    # Disable quarantine dialog for downloaded apps
    LaunchServices.LSQuarantine = false;
  };
}
