{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.hammerspoon;
in {
  options.modules.desktop.hammerspoon = mkOption {
    description = "Hammerspoon macOS automation — provides focus-follows-mouse.";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "hammerspoon";
          }
        ];
      });
    default = {};
  };

  config = mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
    # Hammerspoon reads ~/.hammerspoon/init.lua on startup and on reload.
    home.file.".hammerspoon/init.lua".text = ''
      -- Focus follows mouse — equivalent to Hyprland's follow_mouse = 1
      -- Focuses the window under the cursor with a 50ms debounce to avoid
      -- accidental focus changes while the cursor moves across window borders.
      local focusTimer = nil

      hs.eventtap.new({ hs.eventtap.event.types.mouseMoved }, function()
        if focusTimer then focusTimer:stop() end
        focusTimer = hs.timer.doAfter(0.05, function()
          local pos = hs.mouse.absolutePosition()
          for _, win in ipairs(hs.window.orderedWindows()) do
            if win:isStandard() and win:isVisible()
               and hs.geometry(pos):inside(win:frame()) then
              if win ~= hs.window.focusedWindow() then
                win:focus()
              end
              break
            end
          end
        end)
      end):start()
    '';

    # Autostart Hammerspoon at login. Installed via homebrew cask — binary lives
    # at the standard cask path. KeepAlive restarts it if it crashes.
    # Logs go to ~/Library/Logs/Hammerspoon/ — the canonical macOS location,
    # visible in Console.app, and where Hammerspoon itself logs by default.
    launchd.agents.hammerspoon = {
      enable = true;
      config = {
        ProgramArguments = ["/Applications/Hammerspoon.app/Contents/MacOS/Hammerspoon"];
        ProcessType = "Interactive";
        RunAtLoad = true;
        KeepAlive = {SuccessfulExit = false;};
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/Hammerspoon/hammerspoon.out.log";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/Hammerspoon/hammerspoon.err.log";
      };
    };
  };
}
