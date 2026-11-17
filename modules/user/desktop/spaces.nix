# modules/user/desktop/spaces.nix
#
# Native macOS Spaces as "workspaces" — no third-party window manager.
#
#   * cmd-1..N switch desktops, cmd-left/right move between them
#     (Mission Control shortcuts in com.apple.symbolichotkeys, merged with
#     -dict-add so other shortcuts such as Spotlight's stay untouched).
#   * Apps are pinned to desktops (the Dock's "Options → Assign To" setting,
#     stored in com.apple.spaces app-bindings).
#
# macOS can't create desktops declaratively. Create them once in Mission Control
# (ctrl-up → "+"), then re-run `darwin-rebuild switch` or `spaces-bind`.
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.spaces;

  # Virtual key codes for 1..9 on ANSI keyboards.
  digitKeyCodes = [18 19 20 21 23 22 26 28 25];
  cmdMask = 1048576;
  fnMask = 8388608; # arrow keys carry the fn/numeric-pad flag

  hotkey = id: params: ''
    /usr/bin/defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add ${toString id} \
      "{enabled = 1; value = { parameters = (${concatMapStringsSep ", " toString params}); type = standard; }; }"
  '';

  # 118.. = "Switch to Desktop N"; 79/81 = "Move left/right a space".
  hotkeyScript =
    concatStrings (genList (i: hotkey (118 + i) [65535 (elemAt digitKeyCodes i) cmdMask]) cfg.count)
    + hotkey 79 [65535 123 (cmdMask + fnMask)]
    + hotkey 81 [65535 124 (cmdMask + fnMask)]
    + ''
      # ctrl-cmd-drag anywhere in a window moves it (used by `wm send N`).
      # Apps read this at launch, so already-running apps need a relaunch.
      /usr/bin/defaults write -g NSWindowShouldDragOnGesture -bool true
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    '';

  spacesBind = pkgs.writeShellApplication {
    name = "spaces-bind";
    runtimeInputs = [pkgs.jq];
    text = ''
      # Desktop UUIDs (regular desktops only, in order) for the display with the
      # most desktops. Desktop 1 on the main display has an empty UUID.
      mapfile -t uuids < <(/usr/bin/defaults export com.apple.spaces - \
        | /usr/bin/plutil -convert json -o - - \
        | jq -r '[.SpacesDisplayConfiguration["Management Data"].Monitors[]
                  | [(.Spaces // [])[] | select(.type == 0) | (.uuid // "")]]
                 | max_by(length) | .[]')

      changed=0
      bind() {
        local bundle=$1 n=$2 uuid current
        if (( n > ''${#uuids[@]} )); then
          echo "spaces-bind: only ''${#uuids[@]} desktop(s) exist; create desktop $n in Mission Control to pin $bundle" >&2
          return
        fi
        uuid=''${uuids[n-1]}
        if [[ -z $uuid ]]; then
          echo "spaces-bind: desktop $n has no UUID (main desktop); skipping $bundle" >&2
          return
        fi
        current=$(/usr/bin/defaults export com.apple.spaces - \
          | /usr/bin/plutil -convert json -o - - \
          | jq -r --arg b "$bundle" '."app-bindings"[$b] // ""')
        if [[ $current != "$uuid" ]]; then
          /usr/bin/defaults write com.apple.spaces app-bindings -dict-add "$bundle" "$uuid"
          changed=1
        fi
      }

      ${concatStrings (mapAttrsToList (bundle: n: "bind ${escapeShellArg bundle} ${toString n}\n") cfg.apps)}

      if (( changed )); then
        /usr/bin/killall Dock
        echo "spaces-bind: updated app bindings, restarted Dock"
      fi
    '';
  };
in {
  options.modules.desktop.spaces = mkOption {
    description = "Native macOS Spaces as workspaces (hotkeys + app pinning)";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "native Spaces workspaces";
              count = mkOption {
                type = ints.between 1 9;
                default = 5;
                description = "Number of desktops to bind cmd-1..N to.";
              };
              apps = mkOption {
                type = attrsOf (ints.between 1 9);
                default = {};
                example = {"com.tinyspeck.slackmacgap" = 3;};
                description = "Bundle ID → desktop number (desktop 1 can't be pinned).";
              };
            };
          }
        ];
      });
    default = {};
  };

  config = mkIf (cfg.enable && pkgs.stdenv.hostPlatform.isDarwin) {
    home.packages = [spacesBind];

    home.activation.macosSpaces = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run ${pkgs.writeShellScript "spaces-hotkeys" hotkeyScript}
      run ${spacesBind}/bin/spaces-bind || true
    '';
  };
}
