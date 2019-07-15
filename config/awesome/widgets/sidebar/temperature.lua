-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local helpers = require("helpers")
local pad = helpers.pad
-- custom modules
local globals = require("globals")
local libwidget = require("lib.widget")
local util = require("widgets.util")
local registry = require("widgets.registry")

-- @module temperature
local temperature = {}

local command = [[
  bash -c "
  sensors | grep Package | awk '{print $4}' | cut -c 2-3
  "]]

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

    -- register component
    registry.add("sidebar::temperature::progressbar", widget)

    -- setup signals
    widget:connect_signal("update::value", function(progressbar, conf)
        progressbar.value = conf.value
    end)

    return widget
end

local function start_watch()
    awful.widget.watch(command, 15, function(_, output)
        registry.emit("sidebar::temperature::progressbar", "update::value", { value = tonumber(output) })
    end)
end

function temperature.widget(defaults, overrides)
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

    registry.add("sidebar::temperature", widget)

    widget:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                awful.spawn(globals.terminal .. " -e 'watch sensors'", {floating = true})
            end)
        )
    )

    start_watch()

    return widget
end

return temperature
