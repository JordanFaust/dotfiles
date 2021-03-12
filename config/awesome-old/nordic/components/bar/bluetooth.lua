-- awesome modules
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local registry = require("widgets.registry")
local nordic = {
    core = require("nordic.core")
}

-- @module layout.bar.bluetooth
local bluetooth = { mt = {} }

local function buttons()
    return gears.table.join(
        awful.button({}, 1, function(...)
                -- gears.debug.dump(...)
        end)
    )
end

function bluetooth.new(args)
    local size = args.size or dpi(24)
    local icon = args.icon or beautiful.bluetooth_icon

    local widget = wibox.widget {
        {

            {
                id = "icon",
                image = icon,
                forced_height = dpi(size),
                forced_width = dpi(size),
                widget = wibox.widget.imagebox
            },
            top = dpi(12),
            bottom = dpi(12),
            left = dpi(36),
            right = dpi(24),
            layout = wibox.container.margin
        },
        bg = beautiful.frost_4,
        forced_width = dpi(96),
        forced_height = dpi(48),
        widget = wibox.container.background
    }

    widget:buttons(buttons())

    -- Add highlight on mouse enter/leave
    widget:connect_signal('mouse::enter', function()
        if widget.bg ~= beautiful.frost_4 then
            widget.backup     = widget.bg
            widget.has_backup = true
        end
        widget.bg = nordic.core.color.lighten(beautiful.frost_4, 20)
    end)
    widget:connect_signal('mouse::leave', function()
        if widget.has_backup then widget.bg = widget.backup end
    end)

    registry.add("tagbar::bluetooth", widget)

    return widget
end

function bluetooth.mt:__call(...) --luacheck: no unused args
    return bluetooth.new(...)
end

return setmetatable(bluetooth, bluetooth.mt)
