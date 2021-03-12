
-- awesome modules
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local clickable_container = require('widgets.material.clickable-container')
local registry = require("widgets.registry")

-- @module package-updater
local vpn = {}

-- The command to connect to the VPN
-- @return the vpn connect command
local function command()
    return "vpn-connect"
end

-- The vpn check status command
-- @return the check status command
local function check_status()
    return "nmcli --fields GENERAL.STATE connection show VPN | grep 'activated' | wc -l"
end

local function icon_widget(params)
    local icon = wibox.widget.imagebox(params.icon, true)
    icon.forced_height = dpi(params.size)
    icon.forced_widht = dpi(params.size)

    return wibox.widget {
        {
            id = "icon",
            widget = icon
        },
        right = dpi(11),
        left = dpi(11),
        bottom = dpi(8),
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
                if not widget._private.connected then
                    awful.spawn.easy_async_with_shell(command(), function(out)
                        if string.match(out, "connected") then
                            naughty.notify({
                                preset = naughty.config.presets.normal,
                                title = "VPN",
                                text = "VPN Connected"
                            })
                            registry.emit("tagbar::vpn", "update::value", { value = true })
                        end
                    end)
                end
            end)
        )
    )

    return container
end

local function set_watch()
    gears.timer {
        timeout = 10,
        call_now = true,
        autostart = true,
        callback = function()
            awful.spawn.easy_async_with_shell(check_status(), function(stdout)
                local value = false
                if string.match(stdout, "1") then
                    value = true
                end
                registry.emit("tagbar::vpn", "update::value", { value = value })
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
            if widget._private.connected then
                return 'Connected'
            else
                return 'Disconnected'
            end
        end,
        preferred_positions = {'right', 'left', 'top', 'bottom'}
    })
end

local function set_signals(widget)
    widget:connect_signal("update::value", function(_, config)
        if widget._private.connected == config.value then
            return
        end

        widget._private.connected = config.value
        local icon = widget.icon
        if config.value then
            icon:set_image(beautiful.checked_lock_icon)
        else
            icon:set_image(beautiful.delete_lock_icon)
        end
    end)
end

function vpn.create()
    local widget = icon_widget({ icon = beautiful.delete_lock_icon, size = dpi(24) })
    local container = container_widget(widget)

    registry.add("tagbar::vpn", widget)

    set_watch()
    tooltip(widget)
    set_signals(widget)

    return container
end

return vpn
