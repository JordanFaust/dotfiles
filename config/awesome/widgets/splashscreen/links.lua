-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local globals = require("globals")
local registry = require("widgets.registry")
local helpers = require("helpers")
local pad = helpers.pad

local widget = require("lib.widget")

-- @module links
local links = {}

local function create_url(config, name, path)
    local original_color = beautiful.xcolor4
    local hover_color = beautiful.xcolor12

    local url = widget.textbox({ font = config.font }, "centered")
    url.markup = helpers.colorize_text(name, original_color)

    -- Buttons
    url:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                awful.spawn(globals.browser.." "..path)
                registry.emit("splashscreen", "splashscreen::hide")
            end),
            awful.button({ }, 3, function ()
                awful.spawn(globals.browser.." -new-window "..path)
                registry.emit("splashscreen", "splashscreen::hide")
            end)
    ))

    -- Hover effect
    url:connect_signal("mouse::enter", function ()
        url.markup = helpers.colorize_text(name, hover_color)
    end)
    url:connect_signal("mouse::leave", function ()
        url.markup = helpers.colorize_text(name, original_color)
    end)

    helpers.add_clickable_effect(url)

    return url
end

function links.widget(config)
    local urls = wibox.widget {
        create_url(config, "REDDIT", "reddit.com"),
        pad(1),
        create_url(config, "GITHUB", "github.com/JordanFaust"),
        pad(1),
        create_url(config, "GITHUB WORK", "github.bamtech.co"),
        layout = wibox.layout.fixed.vertical
    }

    return urls
end

return links
