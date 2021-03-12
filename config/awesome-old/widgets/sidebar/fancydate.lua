
-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
-- custom modules
local util = require("widgets.util")
local registry = require("widgets.registry")

-- @module fancydate
local fancydate = {}

function fancydate.widget(defaults, overrides)
    local config = util.merge(defaults, overrides)
    local widget = wibox.widget.textclock("%-j days around the sun")

    widget.align = "center"
    widget.valign = "center"
    widget.font = config.fancydate.font

    widget:buttons(gears.table.join(
        awful.button({ }, 1, function ()
                -- calendar_toggle()
        end),
        awful.button({ }, 3, function ()
                -- calendar_toggle()
        end)
    ))

    registry.add("sidebar::fancydate", widget)

    return widget
end

return fancydate
