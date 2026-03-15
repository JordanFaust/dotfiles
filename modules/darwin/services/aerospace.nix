# modules/darwin/services/aerospace.nix
#
# AeroSpace tiling window manager — managed as a nix-darwin system service so
# that a proper launch agent is registered. Putting `pkgs.aerospace` in
# home.packages only installs the CLI; the server (AeroSpace.app) never starts
# and CLI commands fail to connect.
{ ... }:
{
  # Enable SKHD for coverage of keybinds not supported by AeroSpace
  services.skhd.enable = true;
  services.aerospace = {
    enable = true;

    settings = {
      # start-at-login must NOT be set here — nix-darwin manages AeroSpace
      # startup via launchd and asserts that this option is absent.
      automatically-unhide-macos-hidden-apps = true;

      enable-normalization-flatten-containers = true;
      enable-normalization-opposite-orientation-for-nested-containers = true;

      # Mouse follows focus when the active monitor changes
      on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];

      exec.on-workspace-change = [
        "sketchybar"
        "--trigger"
        "aerospace_workspace_change"
        "FOCUSED_WORKSPACE=%s"
        "PREV_WORKSPACE=%u"
      ];

      # Gap configuration — mirrors Hyprland's gaps_in = 14 / gaps_out = 28
      gaps = {
        inner.horizontal = 14;
        inner.vertical = 14;
        outer = {
          left = 28;
          right = 28;
          top = 64;
          bottom = 28;
        };
      };

      mode.main.binding = {
        # Disable macOS "Hide Others" so cmd-alt-h is fully available
        # See: https://nikitabobko.github.io/AeroSpace/goodies#disable-hide-app
        "cmd-alt-h" = [ ];

        # Focus movement (vim-style) — matches SUPER+h/j/k/l on Hyprland
        "cmd-h" = "focus left";
        "cmd-j" = "focus down";
        "cmd-k" = "focus up";
        "cmd-l" = "focus right";

        # Focus back-and-forth — matches ALT+Tab in Hyprland
        "cmd-backtick" = "focus-back-and-forth";

        # Move windows — matches SUPER+SHIFT+h/j/k/l
        "cmd-shift-h" = "move left";
        "cmd-shift-j" = "move down";
        "cmd-shift-k" = "move up";
        "cmd-shift-l" = "move right";

        # Resize — matches SUPER+ALT+h/j/k/l
        "cmd-ctrl-h" = "resize width -50";
        "cmd-ctrl-l" = "resize width +50";
        "cmd-ctrl-k" = "resize height -50";
        "cmd-ctrl-j" = "resize height +50";

        # Layouts
        "cmd-slash" = "layout tiles horizontal vertical";
        "cmd-comma" = "layout accordion horizontal vertical";
        "cmd-p" = "layout horizontal vertical";

        # Maximize within AeroSpace (no macOS Space) — matches SUPER+F in Hyprland
        "cmd-f" = "fullscreen";
        # macOS native fullscreen (creates its own Space) — matches SUPER+SHIFT+F
        "cmd-shift-f" = "macos-native-fullscreen";

        # Close window — matches SUPER+Q
        "cmd-q" = "close";

        # Launch terminal (kitty) with tmux — matches SUPER+Return on Hyprland.
        # Attaches to an existing unattached session or creates a new one.
        "cmd-enter" = "exec-and-forget open -na kitty --args -e sh -c 'tmux new-session -A -s main'";

        # Clipboard history via Raycast
        "cmd-shift-v" =
          "exec-and-forget open \"raycast://extensions/raycast/clipboard-history/clipboard-history\"";

        # Workspace switching — matches SUPER+1-5
        "cmd-1" = "workspace 1";
        "cmd-2" = "workspace 2";
        "cmd-3" = "workspace 3";
        "cmd-4" = "workspace 4";
        "cmd-5" = "workspace 5";

        # Move window to workspace — matches SUPER+CTRL+1-5
        "cmd-shift-1" = "move-node-to-workspace 1";
        "cmd-shift-2" = "move-node-to-workspace 2";
        "cmd-shift-3" = "move-node-to-workspace 3";
        "cmd-shift-4" = "move-node-to-workspace 4";
        "cmd-shift-5" = "move-node-to-workspace 5";

        # Workspace cycling — matches SUPER+left/right
        "cmd-left" = "workspace prev";
        "cmd-right" = "workspace next";

        # Move window to adjacent workspace
        "cmd-shift-left" = "move-node-to-workspace prev";
        "cmd-shift-right" = "move-node-to-workspace next";

        # Enter service mode
        "cmd-shift-semicolon" = "mode service";
      };

      mode.service.binding = {
        esc = [
          "reload-config"
          "mode main"
        ];
        r = [
          "flatten-workspace-tree"
          "mode main"
        ];
        f = [
          "layout floating tiling"
          "mode main"
        ];
        backspace = [
          "close-all-windows-but-current"
          "mode main"
        ];
      };

      # Window auto-assignment — mirrors Hyprland windowrule workspace assignments
      on-window-detected = [
        {
          "if"."app-id" = "net.kovidgoyal.kitty";
          run = "move-node-to-workspace 2";
        }
        {
          "if"."app-id" = "org.mozilla.firefox";
          run = "move-node-to-workspace 2";
        }
        {
          "if"."app-id" = "com.tinyspeck.slackmacgap";
          run = "move-node-to-workspace 3";
        }
        {
          "if"."app-id" = "com.spotify.client";
          run = "move-node-to-workspace 3";
        }
        {
          "if"."app-id" = "us.zoom.xos";
          run = "move-node-to-workspace 4";
        }
        {
          "if"."app-id" = "com.google.Chrome";
          run = "move-node-to-workspace 4";
        }
      ];
    };
  };
}
