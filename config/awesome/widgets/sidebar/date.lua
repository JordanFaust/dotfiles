-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
-- custom modules
local util = require("widgets.util")
local registry = require("widgets.registry")

-- @module date
local date = {}

function date.widget(defaults, overrides)
    local config = util.merge(defaults, overrides)
    local widget = wibox.widget.textclock("%A, %B %d")

    widget.align = "center"
    widget.valign = "center"
    widget.font = config.date.font

    widget:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                -- calendar_toggle()
            end),
            awful.button({ }, 3, function ()
                -- calendar_toggle()
            end)
        )
    )

    registry.add("sidebar::date", widget)

    return widget
end

return date
