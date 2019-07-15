-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
-- custom modules
local util = require("widgets.util")
local libwidget = require("lib.widget")
local registry = require("widgets.registry")

-- @module search
local search = {}

local function icon_widget(config)
    return libwidget.icon({
        icon = config.icon.image,
        forced_width = config.icon.forced_width,
        forced_height = config.icon.forced_height
    })
end

function search.widget(defaults, overrides)
    local config = util.merge(defaults, overrides)
    local icon = icon_widget(config)
    local textbox = libwidget.textbox({ font = config.search.font }, "default")
    textbox.text = "Search"

    local widget = wibox.widget{
        icon,
        textbox,
        layout = wibox.layout.fixed.horizontal
    }

    registry.add("sidebar::search", widget)

    widget:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                registry.emit("sidebar", "sidebar::hide")
                -- FIXME: sleep is required due to the delayed signal stealing focus
                -- and closing rofi
                awful.spawn.with_shell("sleep 0.1; rofi-apps")
            end)
        )
    )

    return widget
end

return search
