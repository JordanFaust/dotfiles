-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
-- custom modules
local util = require("widgets.util")
local registry = require("widgets.registry")

-- @module time
local time = {}

function time.widget(defaults, overrides)
    local config = util.merge(defaults, overrides)
    local widget = wibox.widget.textclock("%H %M")

    widget.align = "center"
    widget.valign = "center"
    widget.font = config.time.font

    widget:buttons(gears.table.join(
        awful.button({ }, 1, function ()
            -- calendar_toggle()
        end),
        awful.button({ }, 3, function ()
            -- calendar_toggle()
        end)
    ))

    registry.add("sidebar::time", widget)

    return widget
end

return time
