-- awesome modules
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local registry = require("widgets.registry")
local nordic = {
    color = require("nordic.color"),
    util = require("nordic.util")
}

-- @module layout.bar.power
local power = { mt = {} }

local function popup()
    local widget = wibox {
        width = dpi(96),
        height = dpi(166),
        visible = false,
        ontop = true
    }

    widget:setup {
        {
            {
                {
                    markup = nordic.util.colorize_text("Suspend", beautiful.snow_storm_3),
                    widget = wibox.widget.textbox
                },
                top = dpi(20),
                bottom = dpi(20),
                left = dpi(12),
                right = dpi(12),
                widget = wibox.container.margin
            },
            id = "suspend",
            bg = beautiful.frost_4,
            widget = wibox.container.background,
        },
        {
            {
                {
                    markup = nordic.util.colorize_text("Restart", beautiful.snow_storm_3),
                    widget = wibox.widget.textbox
                },
                top = dpi(20),
                bottom = dpi(20),
                left = dpi(12),
                right = dpi(12),
                widget = wibox.container.margin
            },
            id = "restart",
            bg = beautiful.frost_4,
            widget = wibox.container.background
        },
        {
            {
                {
                    markup = nordic.util.colorize_text("Shutdown", beautiful.snow_storm_3),
                    widget = wibox.widget.textbox
                },
                top = dpi(20),
                bottom = dpi(20),
                left = dpi(12),
                right = dpi(12),
                widget = wibox.container.margin
            },
            id = "shutdown",
            bg = beautiful.frost_4,
            widget = wibox.container.background
        },
        layout = wibox.layout.fixed.vertical
    }

    local options = {
        suspend = widget:get_children_by_id("suspend")[1],
        restart = widget:get_children_by_id("restart")[1],
        shutdown = widget:get_children_by_id("shutdown")[1]
    }

    options.suspend:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                awful.spawn.with_shell("systemctl suspend")
            end)
        )
    )

    options.restart:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                awful.spawn.with_shell("reboot")
            end)
        )
    )

    options.shutdown:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                awful.spawn.with_shell("poweroff")
            end)
        )
    )

    for _, option in pairs(options) do
        option:connect_signal("mouse::enter", function(_)
            if option.bg ~= beautiful.frost_4 then
                option.backup     = option.bg
                option.has_backup = true
            end
            option.bg = nordic.color.lighten(beautiful.frost_4, 20)
        end)

        option:connect_signal("mouse::leave", function(_)
            if option.has_backup then option.bg = option.backup end
        end)
    end

    widget:connect_signal("mouse::leave", function(_)
        widget.visible = false
    end)

    return widget
end

local function buttons(dropdown)
    return gears.table.join(
        awful.button({}, 1, function(widget)
            local x = widget.x
            dropdown.x = dpi(x) + dpi(234)
            dropdown.y = 16 + 48 + 2
            dropdown.visible = not dropdown.visible
        end)
    )
end

function power.new(args)
    local size = args.size or dpi(24)
    local icon = args.icon or beautiful.power_icon
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

    local popup_widget = popup()

    widget:buttons(buttons(popup_widget))

    -- Add highlight on mouse enter/leave
    widget:connect_signal('mouse::enter', function()
        if widget.bg ~= beautiful.frost_4 then
            widget.backup     = widget.bg
            widget.has_backup = true
        end
        widget.bg = nordic.color.lighten(beautiful.frost_4, 20)
    end)
    widget:connect_signal('mouse::leave', function()
        if widget.has_backup then widget.bg = widget.backup end
    end)

    return widget
end

function power.mt:__call(...) --luacheck: no unused args
    return power.new(...)
end

return setmetatable(power, power.mt)
