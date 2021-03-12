-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
-- custom modules

-- @module layout.bar.agenda
local notifications = { mt = {} }

local function buttons()
    return gears.table.join(
        awful.button(
            {},
            1,
            nil,
            function()
                -- registry.emit("dashboard", "dashboard::toggle", { search = false })
                -- gears.debug.dump(naughty.notifications)
            end
        )
    )
end

function notifications.new(args)
    local widget = wibox.widget {
        {
            {
                image = beautiful.menu_icon,
                forced_width = dpi(8),
                forced_height = dpi(8),
                resize = true,
                widget = wibox.widget.imagebox
            },
            margins = dpi(10),
            widget = wibox.container.margin
        },
        forced_width = dpi(48),
        forced_height = dpi(48),
        bg = beautiful.aurora_1,
        widget = wibox.container.background
    }


    widget:buttons(buttons())

    return widget
end

function notifications.mt:__call(...) --luacheck: no unused args
    return notifications.new(...)
end

return setmetatable(notifications, notifications.mt)
