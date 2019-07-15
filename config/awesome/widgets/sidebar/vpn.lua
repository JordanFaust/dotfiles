-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local naughty = require("naughty")
local beautiful = require("beautiful")
-- custom modules
local util = require("widgets.util")
local libwidget = require("lib.widget")
local registry = require("widgets.registry")
local helpers = require("helpers")
local pad = helpers.pad


-- @module vpn
local vpn = {}

-- The command to connect to the VPN
-- @return the vpn connect command
local function command()
    return "vpn-connect"
end

-- The vpn check status command
-- @return the check status command
local function check_status()
    return "nmcli --fields GENERAL.STATE connection show VPN | grep 'activated' | wc -l"
end

-- Create a wibox widget icon
-- @return a configured wibox widget icon
local function icon_widget(config)
    return libwidget.icon({
        icon = config.icon.image,
        forced_width = config.icon.size,
        forced_height = config.icon.size
    })
end

local function progressbar_widget(config)
    local widget = wibox.widget{
        max_value     = config.progressbar.max,
        value         = config.progressbar.value,
        forced_height = config.progressbar.forced_height,
        margins       = {
            top = config.progressbar.margins.top,
            bottom = config.progressbar.margins.bottom,
        },
        forced_width  = config.progressbar.forced_width,
        shape         = gears.shape.rounded_bar,
        bar_shape     = gears.shape.rounded_bar,
        color         = config.progressbar.color,
        background_color = config.progressbar.background,
        border_width  = 0,
        border_color  = beautiful.border_color,
        widget        = wibox.widget.progressbar,
    }
    widget.forced_width = config.widget.forced_width
    return widget
end

local function start_watch()
    gears.timer {
        timeout   = 10,
        call_now  = true,
        autostart = true,
        callback  = function()
            awful.spawn.easy_async_with_shell(check_status(), function(out)
                local widget = registry.get("sidebar::vpn::progressbar")
                if string.match(out, "1") then
                    widget.value = 100
                else
                    widget.value = 0
                end
                widget:emit_signal("widget::redraw_needed")
            end)
        end
    }
end

-- Configures button click interactions
-- @return configured awful button configuration
local function buttons()
    return gears.table.join(
        awful.button({ }, 1, function()
            awful.spawn.easy_async_with_shell(command(), function(out)
                if string.match(out, "connected") then
                    naughty.notify({
                        preset = naughty.config.presets.normal,
                        title = "VPN",
                        text = "VPN Connected"
                    })
                end
            end)
        end)
    )
end

-- Creates a configured wibox widget
-- @return The configured wibox widget
function vpn.widget(defaults, overrides)
    local config = util.merge(defaults, overrides)
    local icon = icon_widget(config)
    local progressbar = progressbar_widget(config)

    local widget = wibox.widget {
        nil,
        {
            icon,
            pad(1),
            progressbar,
            pad(1),
            layout = wibox.layout.fixed.horizontal
        },
        nil,
        expand = "none",
        layout = wibox.layout.align.horizontal
    }

    registry.add("sidebar::vpn::progressbar", progressbar)
    registry.add("sidebar::vpn", widget)

    start_watch()
    widget:buttons(buttons())

    return widget
end

return vpn
