-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local helpers = require("helpers")
local pad = helpers.pad
local widget = require("lib.widget")
local globals = require("globals")
local registry = require("widgets.registry")

-- @module bookmarks
local bookmarks = {}

local function create_bookmark(config, name, path)
    local original_color = beautiful.xcolor1
    local hover_color = beautiful.xcolor9

    local bookmark = widget.textbox({ font = config.font }, "centered")
    bookmark.markup = helpers.colorize_text(name, original_color)

    -- Buttons
    bookmark:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                awful.spawn.with_shell(globals.filemanager.." "..path)
                registry.emit("splashscreen", "splashscreen::hide")
            end),
            awful.button({ }, 3, function ()
                awful.spawn.with_shell(globals.terminal.." -e 'ranger' "..path)
                registry.emit("splashscreen", "splashscreen::hide")
            end)
    ))

    -- Hover effect
    bookmark:connect_signal("mouse::enter", function ()
        bookmark.markup = helpers.colorize_text(name, hover_color)
    end)
    bookmark:connect_signal("mouse::leave", function ()
        bookmark.markup = helpers.colorize_text(name, original_color)
    end)

    helpers.add_clickable_effect(bookmark)

    return bookmark
end

function bookmarks.widget(config)
    local _bookmarks = wibox.widget {
        create_bookmark(config, "HOME", ""),
        pad(1),
        create_bookmark(config, "DOWNLOADS", "~/Downloads"),
        pad(1),
        create_bookmark(config, "MUSIC", "~/Music"),
        pad(1),
        create_bookmark(config, "PICTURES", "~/Pictures"),
        pad(1),
        create_bookmark(config, "WALLPAPERS", "~/Pictures/Wallpapers"),
        layout = wibox.layout.fixed.vertical
    }

    return _bookmarks
end

return bookmarks
