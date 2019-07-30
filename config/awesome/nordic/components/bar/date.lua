-- awesome modules
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
-- custom modules

-- @module layout.bar.date
local date = { mt = {} }

function date.new(args) --luacheck: no unused args
    local widget = wibox.widget {
        {
            {
                format = "%A, %B %d, %H %M",
                align = "center",
                valign = "center",
                widget = wibox.widget.textclock
            },
            top = dpi(16),
            bottom = dpi(12),
            left = dpi(12),
            right = dpi(12),
            widget = wibox.container.margin
        },
        id = "background_role",
        forced_height = dpi(48),
        forced_width = dpi(220),
        bg = beautiful.frost_1,
        fg = beautiful.snow_storm_3,
        widget = wibox.container.background
    }

    -- Center the widget
    widget.point = function(geo, params)
        local tag = awful.screen.focused().selected_tag
        local clients = tag:clients()
        -- The space taken up by the task bar
        local count = 0 + (#clients * 0.5)
        -- Calculate the center of the bar within the given layout
        -- * Divide the available space in two and subtract half of the width of the date widget
        -- * Subtract the space for each taskbar, starting at 0 and removing 1 + (#clients * 0.5) from the center
        -- * Add 48 as an offset (not sure how the math works on this, honestly)
        local center_x = ((params.parent.width / 2) - (geo.width / 2)) - (count * dpi(96)) + 48
        return {
            x = center_x,
            y = 0
        }
    end

    return widget
end

function date.mt:__call(...)  --luacheck: no unused args
    return date.new(...)
end

return setmetatable(date, date.mt)
