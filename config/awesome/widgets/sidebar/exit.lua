-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local libwidget = require("lib.widget")
local registry = require("widgets.registry")

-- @module exit
local exit = {}

function exit.widget()
    local icon = libwidget.icon({ icon = beautiful.poweroff_icon })
    local textbox = libwidget.textbox({ font = "sans 14", "default"})
    textbox.text = "Exit"

    local widget = wibox.widget{
        icon,
        textbox,
        layout = wibox.layout.fixed.horizontal
    }
    registry.add("sidebar::exit", widget)

    widget:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                -- exit_screen_show()
                registry.emit("exitscreen", "exitscreen::show")
                registry.emit("sidebar", "sidebar::hide")
                -- sidebar.visible = false
            end)
        )
    )

    return widget
end

return exit
