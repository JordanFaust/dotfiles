-- awesome modules
local awful = require("awful")
local gears = require("gears")
local surface = require("gears.surface")
local wibox = require("wibox")
local beautiful = require("beautiful")
local helpers = require("helpers")
local pad = helpers.pad
-- custom modules
local libwidget = require("lib.widget")
local util = require("widgets.util")
local registry = require("widgets.registry")
local lgi = require("lgi")
local proxy = require("dbus_proxy")

-- @module spotify
local spotify = {
    current_artist = nil,
    current_title = nil,
    current_cover_url = nil,
    running = false
}

local title_color =  beautiful.mpd_song_title_color or beautiful.wibar_fg

-- Creates a wibox widget textbox
-- @return a configured wibox widget textbox
local function textbox_widget(config)
    local widget = libwidget.textbox({ font = config.spotify.font }, "centered")

    widget:connect_signal("update::value", function(_, conf)
        widget.markup = helpers.colorize_text(conf.markup, title_color)
    end)

    return widget
end

-- Create a wibox widget icon
-- @return a configured wibox widget icon
local function icon_widget(config, kind)
    local icon = config.icon[kind].icon
    return libwidget.icon({
        icon = icon,
        forced_width = config.icon.size,
        forced_height = config.icon.size,
        clickable = config.icon.clickable
    })
end

local function image_widget(config)
    local widget = wibox.widget {
        image = surface.load_uncached("/tmp/spotify/cover.jpeg"),
        resize = config.image.resize,
        forced_height = config.image.size,
        forced_width = config.image.size,
        widget = wibox.widget.imagebox
    }

    widget:connect_signal("update::value", function(_, conf)
        local command =  [[
            bash -c '
              mkdir -p /tmp/spotify
              wget -q -O /tmp/spotify/cover.jpeg ]] .. conf.url .. [[ &> /dev/null;
            '
            ]]
        awful.spawn.easy_async_with_shell(command, function(_)
            widget.image = surface.load_uncached("/tmp/spotify/cover.jpeg")
        end)
    end)

    return widget
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

local function update(artist, title, url)
    if spotify.current_title == title then
        return
    end

    registry.emit("splashscreen::spotify::title", "update::value", { markup = title })
    registry.emit("sidebar::spotify::title", "update::value", { markup = title })
    registry.emit("splashscreen::spotify::artist", "update::value", { markup = artist })
    registry.emit("sidebar::spotify::artist", "update::value", { markup = artist })
    registry.emit("splashscreen::spotify::cover", "update::value", { url = url })

    spotify.current_title = title
    spotify.current_artist = artist
    spotify.current_cover_url = url
end

local function properties_changed(...)
    local client = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.mpris.MediaPlayer2.spotify",
        path = "/org/mpris/MediaPlayer2",
        interface = "org.mpris.MediaPlayer2.spotify"
    })

    local metadata, err = client:Get("org.mpris.MediaPlayer2.Player", "Metadata")
    if not err == nil then
        helpers.debug("something went wrong")
    end

    local url = metadata["mpris:artUrl"]
    local artist = metadata["xesam:artist"][1]
    local title = metadata["xesam:title"]
    if title and artist and url then
        update(artist, title, url)
    end
end

-- Leverage DBUS spotify signals and methods to watch and update now playing details.
--
-- <p> Leverages the dbus_proxy module for watching signals and calling methods for
-- spotify. The PropertiesChanged signal is monitored for any changes to the player.
-- Due to the limitation in specifying specific things to watch aggressive caching must
-- be but inplace to limit the number of changes actually performed due to the large number
-- of triggered callbacks per change.
local function watch_spotify()
    local Gio = lgi.Gio
    local changes = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.mpris.MediaPlayer2.spotify",
        path = "/org/mpris/MediaPlayer2",
        interface = "org.freedesktop.DBus.Properties",
        flags = Gio.DBusSignalFlags.NONE
    })

    changes:connect_signal(properties_changed, "PropertiesChanged")
end

-- Wait for Spotify to start before registering proxies to watch for changes.
--
-- <p> If a proxy is created for a dbus service that doesn't exist, it will crash.
-- Due to this limitation we must watch for a NameOwnerChanged event for spotify.
-- When this happens we will connect the signal watch for spotify changes and react
-- to changes as they happen in spotify. </p>
local function watch()
    local Gio = lgi.Gio
    local watch = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.freedesktop.DBus",
        path = "/org/freedesktop/DBus",
        interface = "org.freedesktop.DBus",
        flags = Gio.DBusSignalFlags.NONE
    })
    watch:connect_signal(
        function(_, changed, _)
            if changed == "org.mpris.MediaPlayer2.spotify" then
                watch_spotify()
            end
        end,
        "NameOwnerChanged"
    )

    local services = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.freedesktop.DBus",
        path = "/org/freedesktop/DBus",
        interface = "org.freedesktop.DBus",
    })
    local connected, err = services:ListNames()

    -- check to see if spotify is currently running. If it is start the watches
    -- on that service.
    for _, name in pairs(connected) do
        if name == "org.mpris.MediaPlayer2.spotify" then
            watch_spotify()
            properties_changed()
        end
    end
end

-- Creates a configured wibox widget
-- @return The configured wibox widget
function spotify.now_playing(defaults, overrides)
    local config = util.merge(defaults, overrides)
    local title = textbox_widget(config)
    local artist = textbox_widget(config)

    local widget = wibox.widget {
        title,
        artist,
        layout = wibox.layout.fixed.vertical
    }

    registry.add("sidebar::spotify::title", title)
    registry.add("sidebar::spotify::artist", artist)
    registry.add("sidebar::spotify::playing", widget)
    -- start_timers(title, artist)

    return widget
end

function spotify.controls(defaults, overrides)
    local config = util.merge(defaults, overrides)

    local toggle_icon = icon_widget(config, "toggle")

    local previous_icon = icon_widget(config, "previous")

    local next_icon = icon_widget(config, "nxt")

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

function spotify.splashscreen(defaults, overrides)
    local config = util.merge(defaults, overrides)
    local title = textbox_widget(config)
    local artist = textbox_widget(config)
    local image = image_widget(config)

    local toggle = icon_widget(config, "toggle")
    local previous = icon_widget(config, "previous")
    local nxt = icon_widget(config, "nxt")

    toggle:buttons(pause_buttons())
    previous:buttons(previous_buttons())
    nxt:buttons(next_buttons())

    local widget = wibox.widget {
        -- cover
        {
            pad(9),
            image,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        -- artist details
        {
            pad(1),
            title,
            artist,
            expand = "none",
            layout = wibox.layout.align.vertical
        },
        -- controls
        pad(1),
        {
            pad(10), -- add padding to the left of the controls
            {
                previous,
                pad(1),
                toggle,
                pad(1),
                nxt,
                layout = wibox.layout.fixed.horizontal
            },
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        layout = wibox.layout.fixed.vertical
    }

    registry.add("splashscreen::spotify::cover", image)
    registry.add("splashscreen::spotify::artist", artist)
    registry.add("splashscreen::spotify::title", title)
    registry.add("splashscreen::spotify", widget)

    watch()

    return widget
end

-- function spotify.register(config)
--     registry.add("splashscreen::spotify::title", textbox_widget(config["spotify"]["splashscreen"]["title"]))
--     registry.add("splashscreen::spotify::artist", textbox_widget(config["spotify"]["splashscreen"]["artist"]))
-- end

return spotify
