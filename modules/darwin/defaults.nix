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
      InitialKeyRepeat = 10;
      # Expand save and print panels by default
      NSNavPanelExpandedStateForSaveMode = true;
      PMPrintingExpandedStateForPrint = true;
      # Dark mode — matches Catppuccin Macchiato dark theme used on NixOS/Hyprland
      AppleInterfaceStyle = "Dark";
    };

    # universalaccess.reduceTransparency is intentionally omitted —
    # com.apple.universalaccess is SIP-protected on macOS 13+; writing to it
    # without Accessibility entitlements fails even as root.
    # Set manually: System Settings → Accessibility → Display → Reduce Transparency.

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

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };
}
