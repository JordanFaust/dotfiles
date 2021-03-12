-- awesome modules
local awful = require("awful")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local sidebar = require("layout.sidebar")
local taskbar = require("layout.taskbar")
-- local tagbar = require("layout.tagbar")
local nordic = require("nordic")
local gears = require("gears")

local bar = require("layout.bar")

-- use a single instance of the sidebar across screens

-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(
    function(screen)
        if screen.index == 1 then
            -- s.taskbar = taskbar.create(s, false)
            local offset = dpi(400)
            -- screen.taskbar = bar {
            --     screen = screen,
            --     offset = offset,
            --     vpn = {
            --         icon = beautiful.delete_lock_icon,
            --         size = dpi(24)
            --     },
            --     power = {
            --         icon = beautiful.power_icon,
            --         size = dpi(24)
            --     },
            --     bluetooth = {
            --         icon = beautiful.bluetooth_icon,
            --         size = dpi(24)
            --     }
            -- }
            local agenda_width = dpi(400)
            screen.taskbar = nordic.components.bar {
                screen = screen,
                name = "bar",
                offset = agenda_width,
                agenda_name = "agenda"
            }
            screen.taskbar.visible = true
            screen.agenda = nordic.components.agenda {
                screen = screen,
                name = "agenda",
                width = agenda_width
            }
        else
            screen.taskbar = taskbar.create(screen, false)
            screen.taskbar.visible = true
        end
    end
)
