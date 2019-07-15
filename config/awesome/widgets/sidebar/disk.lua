-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local globals = require("globals")
local libwidget = require("lib.widget")
local util = require("widgets.util")
local registry = require("widgets.registry")
local helpers = require("helpers")
local pad = helpers.pad

-- @module disk
local disk = {}

local command = [[
    bash -c "
    df -k -h /dev/sda3 | tail -1 | awk '{print $5}'
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
        max_value     = 100,
        value         = 50,
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
    if widget == nil then
        helpers.debug("creation of progressbar failed")
    end
    widget.forced_width = config.widget.forced_width

    -- register widget
    registry.add("sidebar::disk::progressbar", widget)

    -- setup signals
    widget:connect_signal("update::value", function(progressbar, conf)
        progressbar.value = conf.value
    end)

    return widget
end

local function start_watch()
    awful.widget.watch(command, 120, function(_, stdout)
        local disk_space = stdout
        -- Remove trailing white space
        disk_space = string.gsub(disk_space, '%%', '')
        disk_space = string.gsub(disk_space, '^%s*(.-)%s*$', '%1')
        registry.emit("sidebar::disk::progressbar", "update::value", { value = tonumber(disk_space) })
    end)
end

function disk.widget(defaults, overrides)
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

    -- registry.add("sidebar::disk::progressbar", progressbar)
    registry.add("sidebar::disk", widget)
    start_watch()

    widget:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                awful.spawn(globals.filemanager, {floating = true})
            end)
        )
    )

    return widget
end

return disk
