-- awesome modules
local awful = require("awful")
local gears = require("gears")
-- local surface = require("gears.surface")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
-- local util = require("widgets.util")
local registry = require("widgets.registry")
local lgi = require("lgi")
local proxy = require("dbus_proxy")
local nordic = {
    util = require("nordic.util")
}

-- @module player
local player = {
    current_artist = nil,
    current_title = nil,
    current_cover_url = nil,
    running = false,
    mt = {}
}

local function update(artist, title, url)
    if player.current_title == title then
        return
    end

    registry.emit("bar::player", "update::value", { artist = artist, title = title })

    player.current_title = title
    player.current_artist = artist
    player.current_cover_url = url
end

local function media_client()
    return proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.mpris.MediaPlayer2.spotify",
        path = "/org/mpris/MediaPlayer2",
        interface = "org.mpris.MediaPlayer2.spotify"
    })
end

local function pause_buttons()
    return gears.table.join(
        awful.button({ }, 1, function ()
            local client = media_client()
            client:PlayPause()
        end)
    )
end

local function previous_buttons()
    return gears.table.join(
        awful.button({ }, 1, function ()
            local client = media_client()
            client:Previous()
        end)
    )
end

local function next_buttons()
    return gears.table.join(
        awful.button({ }, 1, function ()
            local client = media_client()
            client:Next()
        end)
    )
end

local function properties_changed(...) --luacheck: no unused args
    local client = media_client()
    -- local meetings = proxy.Proxy:new({
    --         bus = proxy.Bus.SESSION,
    --         name = "org.eventable.service",
    --         path = "/org/eventable/Calendar",
    --         interface = "org.eventable.CalendarInterface"
    -- })
    -- meetings:ChangeDate("2019-08-09T00:00:00Z", "2019-08-10T00:00:00Z")

    -- for _, events in pairs(meetings:Metadata()) do
    --     gears.debug.dump("Meeting Event:")
    --     for _, event in pairs(events) do
    --         gears.debug.dump(event)
    --     end
    -- end
    -- meetings:connect_signal(function(...) gears.debug.dump("properties changed") end, "PropertiesChanged")

    local metadata, err = client:Get("org.mpris.MediaPlayer2.Player", "Metadata")
    if not err == nil then
        nordic.util.debug("something went wrong")
    end

    if metadata == nil then
        nordic.util.debug("failed to get player metadata")
        return
    end

    local url = metadata["mpris:artUrl"]
    local artist = metadata["xesam:artist"][1]
    local title = metadata["xesam:title"]
    if title and artist and url then
        update(artist, title, url)
    end
end

-- Leverage DBUS player signals and methods to watch and update now playing details.
--
-- <p> Leverages the dbus_proxy module for watching signals and calling methods for
-- player. The PropertiesChanged signal is monitored for any changes to the player.
-- Due to the limitation in specifying specific things to watch aggressive caching must
-- be but inplace to limit the number of changes actually performed due to the large number
-- of triggered callbacks per change.
local function watch_player()
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

-- Wait for Player to start before registering proxies to watch for changes.
--
-- <p> If a proxy is created for a dbus service that doesn't exist, it will crash.
-- Due to this limitation we must watch for a NameOwnerChanged event for player.
-- When this happens we will connect the signal watch for player changes and react
-- to changes as they happen in player. </p>
local function watch()
    local Gio = lgi.Gio
    local watch_proxy = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.freedesktop.DBus",
        path = "/org/freedesktop/DBus",
        interface = "org.freedesktop.DBus",
        flags = Gio.DBusSignalFlags.NONE
    })
    watch_proxy:connect_signal(
        function(_, changed, _)
            if changed == "org.mpris.MediaPlayer2.spotify" then
                watch_player()
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
    local connected, _ = services:ListNames()

    -- check to see if player is currently running. If it is start the watches
    -- on that service.
    for _, name in pairs(connected) do
        if name == "org.mpris.MediaPlayer2.spotify" then
            watch_player()
            properties_changed()
        end
    end
end

function player.new(args) --luacheck: no unused args
    local widget = wibox.widget {
        {
            {
                {
                    {
                        id = "previous",
                        forced_width = dpi(20),
                        forced_height = dpi(20),
                        image = beautiful.media_back_icon,
                        widget = wibox.widget.imagebox
                    },
                    top = dpi(14),
                    bottom = dpi(14),
                    left = dpi(12),
                    right = dpi(1),
                    widget = wibox.container.margin
                },
                {
                    {
                        id = "pause",
                        forced_width = dpi(20),
                        forced_height = dpi(20),
                        image = beautiful.media_pause_icon,
                        widget = wibox.widget.imagebox
                    },
                    top = dpi(14),
                    bottom = dpi(14),
                    left = dpi(1),
                    right = dpi(1),
                    widget = wibox.container.margin
                },
                {
                    {
                        id = "next",
                        forced_width = dpi(20),
                        forced_height = dpi(20),
                        image = beautiful.media_end_icon,
                        widget = wibox.widget.imagebox
                    },
                    top = dpi(14),
                    bottom = dpi(14),
                    left = dpi(1),
                    right = dpi(12),
                    widget = wibox.container.margin
                },
                layout = wibox.layout.fixed.horizontal
            },
            bg = beautiful.frost_3,
            widget = wibox.container.background
        },
        {
            {
                {
                    {
                        id = "now_playing",
                        markup = nordic.util.colorize_text("[not connected]", beautiful.snow_storm_3),
                        align = 'center',
                        valign = 'center',
                        forced_width = dpi(200),
                        ellipsize = 'end',
                        widget = wibox.widget.textbox
                    },
                    top = dpi(18),
                    bottom = dpi(14),
                    widget = wibox.container.margin
                },
                layout = wibox.layout.fixed.horizontal
            },
            bg = beautiful.frost_3,
            widget = wibox.container.background
        },
        layout = wibox.layout.fixed.horizontal
    }

    local previous = widget:get_children_by_id("previous")[1]
    local pause = widget:get_children_by_id("pause")[1]
    local next = widget:get_children_by_id("next")[1]

    widget:connect_signal("update::value", function(_, params)
        local textbox = widget:get_children_by_id("now_playing")[1]
        local text = params.title .. " - " .. params.artist
        textbox.markup = nordic.util.colorize_text(text, beautiful.snow_storm_3)
    end)

    previous:buttons(previous_buttons())
    pause:buttons(pause_buttons())
    next:buttons(next_buttons())

    registry.add("bar::player", widget)
    watch()

    return widget
end

function player.mt:__call(...) --luacheck: no unused args
    return player.new(...)
end

return setmetatable(player, player.mt)
