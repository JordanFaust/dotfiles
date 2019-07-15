-- awesome modules
local awful = require("awful")
-- custom modules

-- @module globals
local globals = {}

globals.terminal = "urxvt -e /usr/bin/zsh"
globals.browser = "firefox --new-tab"
globals.filemanager = "thunar"
globals.tmux = globals.terminal .. " -e tmux new "
globals.editor = "emacs"
globals.slack = "slack"
globals.editor_cmd = globals.terminal.." -e "..globals.editor.." "
-- Get screen geometry
globals.screen_width = awful.screen.focused().geometry.width
globals.screen_height = awful.screen.focused().geometry.height

return globals
