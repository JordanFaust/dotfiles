-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local helpers = require("helpers")
local pad = helpers.pad
-- custom modules
local libwidget = require("lib.widget")
local util = require("widgets.util")
local registry = require("widgets.registry")

-- @module volume
local volume = {}

local volume_script = [[
    bash -c '
    pactl subscribe 2> /dev/null | grep --line-buffered "sink #0"
    ']]

local function update(config)
    awful.spawn.easy_async({"sh", "-c", "pactl list sinks"},
        function(stdout)
            local value = stdout:match('(%d+)%% /')
            local muted = stdout:match('Mute:(%s+)[yes]')
            local fill_color
            local bg_color
            if muted ~= nil then
                fill_color = config.progressbar.muted_color
                bg_color = config.progressbar.muted_background
            else
                fill_color = config.progressbar.color
                bg_color = config.progressbar.background
            end
            local widget = registry.get("sidebar::volume::progressbar")
            widget.value = tonumber(value)
            widget.color = fill_color
            widget.background_color = bg_color
        end
    )
end

local function start_watch(config)
    awful.spawn.with_line_callback(volume_script, {
        stdout = function(_)
            update(config)
        end
    })
end

local function icon_widget(config)
    return libwidget.icon({
            icon = config.icon.image,
            forced_width = config.icon.size,
            forced_height = config.icon.size
    })
end

local function progressbar_widget(config)
    local widget = wibox.widget{
        max_value     = config.progressbar.max,
        value         = config.progressbar.value,
        forced_height = config.progressbar.forced_height,
        margins       = {
            top = config.progressbar.margins.top,
            bottom = config.progressbar.margins.bottom,
        },
        forced_width  = config.progressbar.forced_width,
        shape         = gears.shape.rounded_bar,
        bar_shape     = gears.shape.rounded_bar,
        color         = config.progressbar.color,
        background_color = config.progressbar.background,
        border_width  = 0,
        border_color  = beautiful.border_color,
        widget        = wibox.widget.progressbar,
    }
    widget.forced_width = config.widget.forced_width
    return widget
end

function volume.widget(defaults, overrides)
    local config = util.merge(defaults, overrides)
    local icon = icon_widget(config)
    local progressbar = progressbar_widget(config)

    local widget = wibox.widget{
        nil,
        {
            icon,
            pad(1),
            progressbar,
            pad(1),
            layout = wibox.layout.fixed.horizontal
        },
        nil,
        expand = "none",
        layout = wibox.layout.align.horizontal
    }

    registry.add("sidebar::volume::progressbar", progressbar)
    registry.add("sidebar::volume", widget)
    start_watch(config)

    widget:buttons(
        gears.table.join(
            -- Scroll - Increase / Decrease volume
            awful.button({ }, 4, function ()
                    awful.spawn.with_shell("amixer -D pulse sset Master '5%+'")
            end),
            awful.button({ }, 5, function ()
                    awful.spawn.with_shell("amixer -D pulse sset Master '5%-")
            end)
        )
    )

    return widget
end

return volume
