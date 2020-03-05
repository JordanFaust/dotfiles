-- awesome modules
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local registry = require("widgets.registry")
local nordic = require("nordic")

-- @module layout.bar.vpn
local vpn = { mt = {} }

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

local function buttons(widget)
    return gears.table.join(
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
        local icon = widget:get_children_by_id('icon')[1]
        if config.value then
            icon:set_image(beautiful.checked_lock_icon)
        else
            icon:set_image(beautiful.delete_lock_icon)
        end
    end)
end

function vpn.new(args)
    local widget = wibox.widget {
        {

            {
                id = "icon",
                image = args.icon,
                forced_height = dpi(args.size),
                forced_width = dpi(args.size),
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
    widget:buttons(buttons(widget))
    registry.add("tagbar::vpn", widget)

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

    set_watch()
    set_signals(widget)

    return widget
end

function vpn.mt:__call(...) --luacheck: no unused args
    return vpn.new(...)
end

return setmetatable(vpn, vpn.mt)
