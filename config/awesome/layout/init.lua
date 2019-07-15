-- awesome modules
local awful = require("awful")
-- custom modules
local sidebar = require("layout.sidebar")
local taskbar = require("layout.taskbar")
local tagbar = require("layout.tagbar")

-- use a single instance of the sidebar across screens

-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(
    function(s)
        if s.index == 1 then
            s.tagbar = tagbar.create(s, sidebar.dashboard())
            s.tagbar.visible = true

            s.taskbar = taskbar.create(s, true)
            s.taskbar.visible = true
        else
            s.taskbar = taskbar.create(s, false)
            s.taskbar.visible = true
        end
    end
)
