-- awesome modules
local awful = require("awful")
local gears = require("gears")
-- local surface = require("gears.surface")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
-- local util = require("widgets.util")
-- local registry = require("widgets.registry")
local naughty = require("naughty")
local helpers = require("helpers")
local lgi = require("lgi")
local proxy = require("dbus_proxy")
-- local nordic = {
--     core = require("nordic.core")
-- }

-- @module player
local player = {
    current_artist = nil,
    current_title = nil,
    current_cover_url = nil,
    running = false,
    mt = {}
}

-- Client for communicating with the spotifyd daemon. This provides extended
-- dbus support for linux
local function spotifyd_client()
    return proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.mpris.MediaPlayer2.spotifyd",
        path = "/org/mpris/MediaPlayer2",
        interface = "org.mpris.MediaPlayer2.Player"
    })
end

-- Client for the default spotifyd application. This has limited support
-- and uses the application
local function spotify_client()
    return proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.mpris.MediaPlayer2.spotify",
        path = "/org/mpris/MediaPlayer2",
        interface = "org.mpris.MediaPlayer2.spotify"
    })
end

-- local function properties_changed(...) --luacheck: no unused args
--     local client = spotify_client()
--     local metadata, err = client:Get("org.mpris.MediaPlayer2.Player", "Metadata")
--     if not err == nil then
--         naughty.notification {
--             urgency = "critical",
--             title   = "Spotify Control",
--             message = "failed to get metadata"
--         }
--     end

--     if metadata == nil then
--         naughty.notification {
--             urgency = "critical",
--             title   = "Spotify Control",
--             message = "metadata nil"
--         }
--         -- nordic.core.util.debug("failed to get player metadata")
--         return
--     end

--     if metadata["mpris:artUrl"] == nil then
--         return
--     end

--     -- Linux Spotify currently retunrs a broken link. Extract the image ID and use the appropriate
--     -- CDN link.
--     -- from: https://community.spotify.com/t5/Desktop-Linux/MPRIS-cover-art-url-file-not-found/td-p/4920104
--     local imageId = string.sub(metadata["mpris:artUrl"], 32)
--     local url = "https://i.scdn.co/image/" .. imageId
--     local artist = metadata["xesam:artist"][1]
--     local title = metadata["xesam:title"]
--     local length_in_microoseconds = metadata["mpris:length"]
--     local length = length_in_microoseconds / 1000 / 1000
--     if title and artist and url then
--         update(artist, title, url, length)
--     end
-- end

-- -- Leverage DBUS player signals and methods to watch and update now playing details.
-- --
-- -- <p> Leverages the dbus_proxy module for watching signals and calling methods for
-- -- player. The PropertiesChanged signal is monitored for any changes to the player.
-- -- Due to the limitation in specifying specific things to watch aggressive caching must
-- -- be but inplace to limit the number of changes actually performed due to the large number
-- -- of triggered callbacks per change.
-- local function watch_player()
--     local Gio = lgi.Gio
--     local changes = proxy.Proxy:new({
--         bus = proxy.Bus.SESSION,
--         name = "org.mpris.MediaPlayer2.spotify",
--         path = "/org/mpris/MediaPlayer2",
--         interface = "org.freedesktop.DBus.Properties",
--         flags = Gio.DBusSignalFlags.NONE
--     })

--     changes:connect_signal(properties_changed, "PropertiesChanged")
-- end

-- -- Wait for Player to start before registering proxies to watch for changes.
-- --
-- -- <p> If a proxy is created for a dbus service that doesn't exist, it will crash.
-- -- Due to this limitation we must watch for a NameOwnerChanged event for player.
-- -- When this happens we will connect the signal watch for player changes and react
-- -- to changes as they happen in player. </p>
-- local function watch()
--     local Gio = lgi.Gio
--     local watch_proxy = proxy.Proxy:new({
--         bus = proxy.Bus.SESSION,
--         name = "org.freedesktop.DBus",
--         path = "/org/freedesktop/DBus",
--         interface = "org.freedesktop.DBus",
--         flags = Gio.DBusSignalFlags.NONE
--     })
--     watch_proxy:connect_signal(
--         function(_, changed, _)
--             if changed == "org.mpris.MediaPlayer2.spotify" then
--                 watch_player()
--             end
--         end,
--         "NameOwnerChanged"
--     )

--     local services = proxy.Proxy:new({
--         bus = proxy.Bus.SESSION,
--         name = "org.freedesktop.DBus",
--         path = "/org/freedesktop/DBus",
--         interface = "org.freedesktop.DBus",
--     })
--     local connected, _ = services:ListNames()

--     -- check to see if player is currently running. If it is start the watches
--     -- on that service.
--     for _, name in pairs(connected) do
--         if name == "org.mpris.MediaPlayer2.spotify" then

--             watch_player()
--             properties_changed()
--         end
--     end
-- end

local function create_button(symbol, color, command, playpause)
    local icon = wibox.widget {
        markup = helpers.colorize_text(symbol, color),
        font = "FiraCode Nerd Font Mono 20",
        align = "center",
        valigin = "center",
        widget = wibox.widget.textbox()
    }

    local button = wibox.widget {
        icon,
        forced_height = dpi(30),
        forced_width = dpi(30),
        widget = wibox.container.background
    }

    button:buttons(gears.table.join(
                       awful.button({}, 1, function() command() end)))

    button:connect_signal("mouse::enter", function()
        icon.markup = helpers.colorize_text(icon.text, beautiful.xforeground)
    end)

    button:connect_signal("mouse::leave", function()
        icon.markup = helpers.colorize_text(icon.text, color)
    end)

    return button
end

local function update_album_art(url, f)
    local command = [[
        bash -c '
        url="]]..url..[["
        mkdir -p /tmp/spotify
        cover=/tmp/spotify/cover.jpeg
        touch $cover
        curl $url --output $cover &> /dev/null
        convert /tmp/spotify/cover.jpeg /tmp/spotify/cover.png
        ']]

    awful.spawn.easy_async_with_shell(command, function()
        f()
    end)
end

function player.new(args) --luacheck: no unused args
    local art = wibox.widget {
        image = gears.filesystem.get_configuration_dir() .. "images/default.png",
        resize = true,
        forced_height = dpi(80),
        forced_width = dpi(80),
        clip_shape = helpers.rrect(beautiful.border_radius - 5),
        widget = wibox.widget.imagebox
    }

    local title_widget = wibox.widget {
        markup = 'Nothing Playing',
        align = 'center',
        valign = 'center',
        ellipsize = 'middle',
        widget = wibox.widget.textbox
    }

    local artist_widget = wibox.widget {
        markup = 'Nothing Playing',
        align = 'center',
        valign = 'center',
        ellipsize = 'middle',
        wrap = 'word_char',
        widget = wibox.widget.textbox
    }

    local slider = wibox.widget {
        forced_height = dpi(5),
        bar_shape = helpers.rrect(beautiful.border_radius),
        shape = helpers.rrect(beautiful.border_radius),
        background_color = beautiful.xcolor0,
        color = {
            type = 'linear',
            from = {0, 0},
            to = {200, 50},
            stops = {{0, beautiful.xcolor0}, {0.75, beautiful.xcolor5}}
        },
        value = 25,
        max_value = 100,
        widget = wibox.widget.progressbar
    }

    local play_command = function()
      local client = spotify_client()
      client:PlayPause()
    end
    local prev_command = function()
      local client = spotify_client()
      -- reset slider on track change
      slider.value = 0
      client:Previous()
    end
    local next_command = function()
      local client = spotify_client()
      -- reset slider on track change
      slider.value = 0
      client:Next()
    end

    local playerctl_play_symbol = create_button("", beautiful.xcolor4, play_command, true)
    local playerctl_prev_symbol = create_button("玲", beautiful.xcolor4, prev_command, false)
    local playerctl_next_symbol = create_button("怜", beautiful.xcolor4, next_command, false)

    local widget = wibox.widget {
        {
            art,
            left = dpi(22),
            top = dpi(17),
            bottom = dpi(17),
            layout = wibox.container.margin
        },
        {
            {
                {
                    {
                        title_widget,
                        artist_widget,
                        layout = wibox.layout.fixed.vertical
                    },
                    top = 10,
                    left = 25,
                    right = 25,
                    widget = wibox.container.margin
                },
                {
                    nil,
                    {
                        playerctl_prev_symbol,
                        playerctl_play_symbol,
                        playerctl_next_symbol,
                        spacing = dpi(40),
                        layout = wibox.layout.fixed.horizontal
                    },
                    nil,
                    expand = "none",
                    layout = wibox.layout.align.horizontal
                },
                {
                    slider,
                    top = dpi(10),
                    left = dpi(25),
                    right = dpi(25),
                    widget = wibox.container.margin
                },
                layout = wibox.layout.align.vertical
            },
            top = dpi(0),
            bottom = dpi(10),
            widget = wibox.container.margin
        },
        layout = wibox.layout.align.horizontal
    }

    _G.awesome.connect_signal("evil::spotifyd::track", function(track)
        title_widget:set_markup_silently(
            '<span foreground="' .. beautiful.xcolor5 .. '">' .. track.title .. '</span>')
        artist_widget:set_markup_silently(
            '<span foreground="' .. beautiful.xcolor6 .. '">' .. track.artist .. '</span>')
        -- textbox.markup = helpers.colorize_text(text, beautiful.snow_storm_3)
    end)

    _G.awesome.connect_signal("evil::spotifyd::position", function(track)
        slider.value = (track.position / track.length) * 100
    end)

    _G.awesome.connect_signal("evil::spotifyd::album_art", function(track)
        art:set_image(gears.surface.load_uncached(track.album_art_path))
    end)

    return widget
end

function player.mt:__call(...) --luacheck: no unused args
    return player.new(...)
end

return setmetatable(player, player.mt)
