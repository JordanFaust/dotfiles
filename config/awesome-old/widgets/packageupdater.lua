-- awesome modules
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local clickable_container = require('widgets.material.clickable-container')
local libwidget = require("lib.widget")
local registry = require("widgets.registry")

-- @module package-updater
local packageupdater = {}

local count_command = [[pacman -Qu | wc -l]]

local function icon_widget(config)
    local icon = libwidget.icon({
        icon = config.icon,
        forced_height = config.size,
        forced_width = config.size,
    })

    return wibox.widget {
        {
            id = "icon",
            widget = icon
        },
        right = dpi(11),
        left = dpi(11),
        layout = wibox.container.margin
    }
end


local function container_widget(widget)
    local container = clickable_container(
        widget,
        dpi(10),
        dpi(10),
        dpi(8),
        dpi(8)
    )

    container:buttons(
        gears.table.join(
            awful.button({}, 1, function()
                if 0 < widget._private.updates then
                    awful.spawn("urxvt -name package-updater -e sudo apt upgrade --yes", {
                        floating = true,
                        tag = _G.mouse.screen.selected_tag,
                        width = awful.screen.focused().geometry.width * 0.50,
                        height = awful.screen.focused().geometry.width * 0.50
                    })
                end
            end)
        )
    )

    return container
end

local function set_watch()
    gears.timer {
        timeout = 300,
        call_now = true,
        autostart = true,
        callback = function()
            awful.spawn.easy_async_with_shell(count_command, function(stdout)
                local value = string.gsub(stdout, "\n", "")
                registry.emit("tagbar::packages", "update::value", { value = tonumber(value) })
            end)
        end
    }
end

local function tooltip(widget)
    awful.tooltip({
        objects = {widget},
        mode = 'outside',
        align = 'right',
        timer_function = function()
            if widget._private.updates then
                return widget._private.updates .. ' updates are available'
            else
                return 'We are up-to-date!'
            end
        end,
        preferred_positions = {'right', 'left', 'top', 'bottom'}
    })
end

local function set_signals(widget)
    widget:connect_signal("update::value", function(_, config)
        widget._private.updates = config.value
        local icon = widget.icon
        if 0 < config.value then
            icon:set_image(beautiful.package_up_icon)
        else
            icon:set_image(beautiful.package_icon)
        end
    end)
end

function packageupdater.create()
    local widget = icon_widget({ icon = beautiful.package_icon, size = dpi(24) })
    local container = container_widget(widget)

    registry.add("tagbar::packages", widget)

    set_watch()
    tooltip(widget)
    set_signals(widget)

    return container
end

return packageupdater
