local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local helpers = require("helpers")

local widget = require("lib.widget")

-- User is a widget that displays users info in the start screen.
-- @module user
local user = {}

local function username_widget(config)
    local _config = { font = config.username_font, text = os.getenv("USER") }
    local textbox = widget.textbox(_config, "centered")

    return textbox
end

local function hostname_widget(config)
    local _config = { font = config.username_font }
    local hostname = widget.textbox(_config, "centered")
    awful.spawn.easy_async_with_shell("hostname", function(out)
        -- Remove trailing whitespaces
        out = out:gsub('^%s*(.-)%s*$', '%1')
        hostname.markup = helpers.colorize_text("@"..out, beautiful.xcolor8)
    end)

    return hostname
end

-- Creates a widget with details about a user.
-- @return a wibox.widget with details about a user
function user.widget(config)
    local username = username_widget(config)
    local hostname = hostname_widget(config)

    return wibox.widget {
        username,
        hostname,
        layout = wibox.layout.fixed.vertical
    }
end

return user
