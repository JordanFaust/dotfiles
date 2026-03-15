# modules/darwin/defaults.nix
#
# macOS system preferences set declaratively via nix-darwin.
# Run `darwin-rebuild switch` to apply. Some changes require logout/reboot.
# Full option list: https://nix-darwin.github.io/nix-darwin/manual/
{...}: {
  system.defaults = {
    NSGlobalDomain = {
      # Key repeat — dialed back from max (1) to avoid overshooting.
      # Scale: 1 (fastest) → 15 (slowest). macOS default is 6/25.
      KeyRepeat = 3;
      InitialKeyRepeat = 15;
      # Disable "natural" scroll direction — matches Linux trackpad/wheel behavior
      # (scroll up = content moves up). macOS default is true (reversed).
      "com.apple.swipescrolldirection" = false;
      # Expand save and print panels by default
      NSNavPanelExpandedStateForSaveMode = true;
      PMPrintingExpandedStateForPrint = true;
      # Dark mode — matches Catppuccin Macchiato dark theme used on NixOS/Hyprland
      AppleInterfaceStyle = "Dark";
      # Hide native menu bar so sketchybar can own the top of the screen
      _HIHideMenuBar = true;
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

    screensaver = {
      # Require password immediately after screensaver starts
      askForPassword = true;
      askForPasswordDelay = 0;
    };

    CustomUserPreferences = {
      # Screensaver idle timeout — 900s = 15 minutes (matches hypridle lock timeout)
      "com.apple.screensaver".idleTime = 900;
    };
  };

  # Display sleep must be >= screensaver timeout or the screen turns off before
  # the screensaver activates. Set to 15 min to match.
  system.activationScripts.postActivation.text = ''
    echo "Configuring display sleep timeout..."
    /usr/bin/pmset -a displaysleep 15
  '';

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };
}
