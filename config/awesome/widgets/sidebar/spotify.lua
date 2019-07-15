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

-- @module spotify
local spotify = {}

local title_color =  beautiful.mpd_song_title_color or beautiful.wibar_fg
local artist_color = beautiful.mpd_song_artist_color or beautiful.wibar_fg

-- Creates a wibox widget textbox
-- @return a configured wibox widget textbox
local function textbox_widget(config)
    return libwidget.textbox({ font = config.spotify.font }, "centered")
end

-- Create a wibox widget icon
-- @return a configured wibox widget icon
local function icon_widget(config)
    return libwidget.icon({
        icon = config.icon.image,
        forced_width = config.icon.size,
        forced_height = config.icon.size,
        clickable = config.icon.clickable
    })
end

local function pause_buttons()
    return gears.table.join(
        awful.button({ }, 1, function ()
            awful.spawn.with_shell("spotify-toggle")
        end)
    )
end

local function previous_buttons()
    return gears.table.join(
        awful.button({ }, 1, function ()
            awful.spawn.with_shell("spotify-previous")
        end)
    )
end

local function next_buttons()
    return gears.table.join(
        awful.button({ }, 1, function ()
            awful.spawn.with_shell("spotify-next")
        end)
    )
end

-- The command to get the current song and artist
-- @return the spotify-info command
local function info_command()
    return "spotify-info"
end

-- Start timers used to update widgets
-- @param title The title textbox
-- @param artist the artist textbox
local function start_timers(title, artist)
    gears.timer {
        timeout   = 5,
        call_now  = true,
        autostart = true,
        callback  = function()
            awful.spawn.easy_async_with_shell(info_command(), function(output)
                output = output or "Not Started@@@Spotify@@/tmp/spotify/cover.png"

                local current_title = output:match('@@@(.*)@@')
                -- local title_color = colors["title_color"]
                current_title = string.gsub(current_title, '^%s*(.-)%s*$', '%1')
                current_title = string.gsub(current_title, "&", "&amp;")
                title.markup = "<span foreground='" .. title_color .."'>" .. current_title .. "</span>"

                local current_artist = output:match('^(.*)@@@')
                -- local artist_color = colors["artist_color"]
                current_artist = string.gsub(current_artist, '^%s*(.-)%s*$', '%1')
                current_artist = string.gsub(current_artist, "&", "&amp;")
                artist.markup = "<span foreground='" .. artist_color .."'>" .. current_artist .. "</span>"
            end)
        end
    }
end

-- Creates a configured wibox widget
-- @return The configured wibox widget
function spotify.now_playing(defaults, overrides)
    local config = util.merge(defaults, overrides)
    -- local title = textbox_widget({text = "---------", align = "center", valign = "center",})
    -- local artist = self:textbox({text = "---------", align = "center", valign = "center",})
    local title = textbox_widget(config)
    local artist = textbox_widget(config)

    local widget = wibox.widget {
        title,
        artist,
        layout = wibox.layout.fixed.vertical
    }

    start_timers(title, artist)
    registry.add("sidebar::spotify::playing", widget)

    return widget
end

function spotify.controls(defaults, overrides)
    local config = util.merge(defaults, overrides)

    local toggle_config = util.merge(config, { icon = { image = beautiful.playerctl_toggle_icon } })
    local toggle_icon = icon_widget(toggle_config)

    local previous_config = util.merge(config, { icon = { image = beautiful.playerctl_prev_icon } })
    local previous_icon = icon_widget(previous_config)

    local next_config = util.merge(config, { icon = { image = beautiful.playerctl_next_icon } })
    local next_icon = icon_widget(next_config)

    toggle_icon:buttons(pause_buttons())
    previous_icon:buttons(previous_buttons())
    next_icon:buttons(next_buttons())

    local widget = wibox.widget {
        nil,
        {
            previous_icon,
            pad(1),
            toggle_icon,
            pad(1),
            next_icon,
            layout  = wibox.layout.fixed.horizontal
        },
        nil,
        expand = "none",
        layout = wibox.layout.align.horizontal,
    }

    registry.add("sidebar::spotify::controls", widget)

    return widget
end

return spotify
