-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local registry = require("widgets.registry")

-- @module layout.bar.agenda
local agenda = { mt = {} }

local function buttons(target, signal)
    return gears.table.join(
        awful.button(
            {},
            1,
            nil,
            function()
                registry.emit(target, signal, { search = false })
            end
        )
    )
end

function agenda.new(args)
    local target = args.target
    assert(target, "args.target is nil")
    local signal = args.signal
    assert(signal, "args.signal is nil")

    local widget = wibox.widget {
        {
            {
                id = 'icon',
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

    widget:connect_signal("dashboard::opened", function(_)
        local icon = widget:get_children_by_id('icon')[1]
        icon:set_image(beautiful.close_icon)
    end)
    widget:connect_signal("dashboard::closed", function(_)
        local icon = widget:get_children_by_id('icon')[1]
        icon:set_image(beautiful.menu_icon)
    end)

    registry.add("agenda", widget)

    widget:buttons(buttons(target, signal))

    return widget
end

function agenda.mt:__call(...) --luacheck: no unused args
    return agenda.new(...)
end

return setmetatable(agenda, agenda.mt)
