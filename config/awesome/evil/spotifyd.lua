-- Provides:
-- evil::spotifyd
--      artist (string)
--      song (string)
--      status (string) [playing | paused | stopped]
--      position (int)
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

local lgi = require("lgi")
local proxy = require("dbus_proxy")

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

-- An object containing details about the currently playing song
-- TODO: Fix this because this is referenced from different functions
local track = {
  -- The name of the artist
  artist = nil,
  -- The name of the album
  album = nil,
  -- The title of the song
  title = nil,
  -- The url of the album art
  url = nil,
  -- The local filesystem path the artwork was written to
  album_art_path = nil,
  -- The length of the song
  length = nil,
  -- The current possition of the song
  position = nil
}

-- Download and convert the album artwork into a format we can use in awesome
local function update_album_art(url)
    local command = [[
        bash -c '
        url="]]..url..[["
        mkdir -p /tmp/spotify
        cover=/tmp/spotify/cover.jpeg
        touch $cover
        curl $url --output $cover &> /dev/null
        convert /tmp/spotify/cover.jpeg /tmp/spotify/cover.png
        ']]
    track.album_art_path = "/tmp/spotify/cover.png"
    awful.spawn.easy_async_with_shell(command, function()
        _G.awesome.emit_signal("evil::spotifyd::album_art", track)
    end)
end


-- Periodically grab the position details for the current track
-- Returns 0 when no track is currently playing
local function position_tracker_timer()
    naughty.notification {
        urgency = "critical",
        title   = "Spotify Control",
        message = "Starting timer"
    }
    gears.timer {
        timeout = 5,
        call_now = true,
        autostart = true,
        callback = function()
            local client = spotifyd_client()

            -- Get metadata to get duration of track
            local position, err = client:Get("org.mpris.MediaPlayer2.Player", "Position")
            if not err == nil then
                -- naughty.notification {
                --     urgency = "critical",
                --     title   = "Spotify Control",
                --     message = "failed to get metadata for slider"
                -- }
                return
            end
            if position == nil then
                -- naughty.notification {
                --     urgency = "critical",
                --     title   = "Spotify Control",
                --     message = "metadata nil used for slider"
                -- }
                -- nordic.core.util.debug("failed to get player metadata")
                return
            end
            -- convert from microseconds to seconds
            local value = position / 1000 / 1000
            track.position = value
            _G.awesome.emit_signal("evil::spotifyd::position", track)
        end
    }
end

-- Respond to changes in properties for the spotify dbus
local function properties_changed(...) --luacheck: no unused args
    local client = spotify_client()
    local metadata, err = client:Get("org.mpris.MediaPlayer2.Player", "Metadata")
    if not err == nil then
        naughty.notification {
            urgency = "critical",
            title   = "Spotify Control",
            message = "failed to get metadata"
        }
    end

    if metadata == nil then
        naughty.notification {
            urgency = "critical",
            title   = "Spotify Control",
            message = "metadata nil"
        }
        -- nordic.core.util.debug("failed to get player metadata")
        return
    end

    -- Don't emit updates if required info is missing
    if metadata["mpris:artUrl"] == nil then
        return
    end

    -- Don't emit updates if nothing has changed
    if track.title == metadata["xesam:title"] then
        return
    end

    -- Linux Spotify currently retunrs a broken link. Extract the image ID and use the appropriate
    -- CDN link.
    -- from: https://community.spotify.com/t5/Desktop-Linux/MPRIS-cover-art-url-file-not-found/td-p/4920104
    local imageId = string.sub(metadata["mpris:artUrl"], 32)
    track.url = "https://i.scdn.co/image/" .. imageId

    track.artist = metadata["xesam:artist"][1]
    track.title = metadata["xesam:title"]
    track.length = metadata["mpris:length"] / 1000 / 1000

    if track.title and track.artist and track.url and track.length then
        update_album_art(track.url)
        _G.awesome.emit_signal("evil::spotifyd::track", track)
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
    naughty.notification {
        urgency = "critical",
        title   = "Spotify Control",
        message = "Setting up spotifyd watch"
    }
    watch_proxy:connect_signal(
        function(_, changed, _)
            -- The spotify dbus is used for player interactions since this isn't
            -- fully features in spotifyd
            if changed == "org.mpris.MediaPlayer2.spotify" then

                naughty.notification {
                    urgency = "critical",
                    title   = "Spotify Control",
                    message = "Connect Signal Changed starting player watch"
                }
                watch_player()
            end
            if changed == "org.mpris.MediaPlayer2.spotifyd" then
                naughty.notification {
                    urgency = "critical",
                    title   = "Spotify Control",
                    message = "Connect Signal Changed starting position tracker timer"
                }
                position_tracker_timer()
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
        if name == "org.mpris.MediaPlayer2.spotifyd" then
            position_tracker_timer()
        end
    end
end

-- Don't start timers until a device is connected on dbus that it can proxy and watch
watch()
