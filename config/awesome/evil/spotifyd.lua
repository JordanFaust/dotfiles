-- Provides:
-- evil::spotifyd
--      artist (string)
--      song (string)
--      status (string) [playing | paused | stopped]
--      position (int)


-- AwesomeWM dependencies
local awful = require("awful")
local gears = require("gears")

-- LuaRocks dependencies
local Gio = require("lgi").Gio
local proxy = require("dbus_proxy")

-- Custom dependencies
local logger = require("logger")

-- Const Variables
local SPOTIFYD_TRACK_SIGNAL = "evil::spotifyd::track"
local SPOTIFY_ASSET_URL_PREFIX = "https://i.scdn.co/image/"

local spotifyd = {
    -- The current track from spotify
    current_track = nil,
}

-- An object containing details about the currently playing song
-- TODO: Fix this because this is referenced from different functions
local track = {
    -------------
    -- Properties
    -------------

    -- The name of the artist
    _artist = nil,
    -- The name of the album
    _album = nil,
    -- The title of the song
    _title = nil,
    -- The url of the album art
    _url = nil,
    -- The local filesystem path the artwork was written to
    _album_art_path = nil,
    -- The length of the song
    _length = nil,
    -- The current possition of the song
    _position = nil,
    -- The metadata of the track
    _metadata = nil,
    -- The PlaybackStatus of the current track. Can be 'Playing', 'Paused', or 'Stopped'
    _playback_status = nil,

    -- The timer tracking the progress of the song
    _track_timer = nil
}

-- Update the metadata of the track and it's associated properties
function track:new(metadata)
    logger.debug("[spotifyd] creating a new track")
    self._metadata = metadata
    self._artist = metadata["xesam:artist"][1]
    self._title = metadata["xesam:title"]
    self._length = metadata["mpris:length"] / 1000 / 1000
    -- default to playing since the track must be playing for the constructor to be called
    self._playback_status = "Playing"
    -- Linux Spotify currently retunrs a broken link. Extract the image ID and use the appropriate
    -- CDN link.
    -- from: https://community.spotify.com/t5/Desktop-Linux/MPRIS-cover-art-url-file-not-found/td-p/4920104
    local imageId = string.sub(metadata["mpris:artUrl"], 32)
    self._url = SPOTIFY_ASSET_URL_PREFIX .. imageId

    return self
end

-- Update the current position of the track
function track:update_position(position)
    self._position = position
end

-- Update the PlaybackStatus of the track
function track:update_playback_status(status)
    logger.debug("setting status to " .. status .. " was " .. self._playback_status)
    self._playback_status = status

    if not self._track_timer then
        logger.warn("[spotifyd] timer not created")
        return
    end

    if status == "Paused" then
        self._track_timer:stop()
    else
        self._track_timer:start()
    end
end

function track:create_position_timer()
    local timer = gears.timer {
        timeout = 2,
        call_now = true,
        autostart = true,
        callback = function()
            local position = self._position or 0
            self._position = position + 2
            logger.debug("[spotifyd] track position " .. self._position)
            _G.awesome.emit_signal(SPOTIFYD_TRACK_SIGNAL, spotifyd.current_track:clone())
        end
    }
    self._track_timer = timer
end

-- Download and convert the album artwork into a format we can use in awesome
function track:update_album_art()
    self._album_art_path = "/tmp/spotify/cover.png"
    local command = [[
        bash -c '
        url="]]..self._url..[["
        mkdir -p /tmp/spotify
        cover=/tmp/spotify/cover.jpeg
        touch $cover
        curl $url --output $cover &> /dev/null
        convert /tmp/spotify/cover.jpeg /tmp/spotify/cover.png
        ']]
    awful.spawn.easy_async_with_shell(command, function()
        _G.awesome.emit_signal("evil::spotifyd::album_art", track:clone())
    end)
end

function track:same_track(new_title)
    return self._title == new_title
end

-- Clone properties into a new table that can be passed externally
function track:clone()
    return {
        artist = self._artist,
        album = self._album,
        title = self._title,
        url = self._url,
        album_art_path = self._album_art_path,
        length = self._length or 100,
        position = self._position or 25,
        playback_status = self._playback_status
    }
end

function track:destroy()
    logger.debug("[spotifyd] old timer destroyed")
    self._track_timer:stop()
    self._position = 0
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


-- Respond to changes in properties for the spotify dbus
local function properties_changed(...) --luacheck: no unused args
    local function change(...) --luacheck: no unused args
        local client, interface = ...
        if not client then
            client = spotify_client()
        end
        local metadata, err = client:Get("org.mpris.MediaPlayer2.Player", "Metadata")
        if not err == nil then
            logger.warn("[spotifyd] failed to get player metadata")
        end

        if metadata == nil then
            logger.warn("[spotifyd] failed to get player metadata")
            return
        end

        -- Don't emit updates if required info is missing
        if metadata["mpris:artUrl"] == nil then
            return
        end
        if metadata["xesam:title"] == nil then
            return
        end

        -- Only rerender album art on new song
        local update_album_art = false

        -- If the track has not been created or this is a brand new track create it
        if spotifyd.current_track == nil then
            logger.debug("[spotifyd] Initializing new track")
            spotifyd.current_track = track:new(metadata)
            -- Start the position tracker timer
            logger.debug("[spotifyd] creating timer")
            spotifyd.current_track:create_position_timer()
            -- Render album art on next pass
            update_album_art = true
        end

        logger.debug("CHECKING TRACK EQUALITY")
        if not spotifyd.current_track:same_track(metadata["xesam:title"]) then
            -- Free any timers from the existing track
            logger.debug("[spotifyd] Track changed, destroying old track")
            spotifyd.current_track:destroy()
            logger.debug("[spotifyd] Track changed, creating new track")
            spotifyd.current_track = track:new(metadata)
            -- Start the position tracker timer
            logger.debug("[spotifyd] creating timer")
            spotifyd.current_track:create_position_timer()
            -- Don't rerender the album art if it hasn't changed
            update_album_art = true
        end

        local status, serr = client:Get("org.mpris.MediaPlayer2.Player", "PlaybackStatus")
        if not serr == nil then
            logger.warn("[spotifyd] failed to get PlaybackStatus")
        end

        -- Update the playback status
        spotifyd.current_track:update_playback_status(status)

        if update_album_art then
            spotifyd.current_track:update_album_art()
        end

        logger.debug("[spotifyd] emitting track event")
        _G.awesome.emit_signal(SPOTIFYD_TRACK_SIGNAL, spotifyd.current_track:clone())
    end
    logger.debug("[spotifyd] properties changed")
    -- Run watch asynchronously
    Gio.Async.start(change)(...)
end

-- Leverage DBUS player signals and methods to watch and update now playing details.
--
-- <p> Leverages the dbus_proxy module for watching signals and calling methods for
-- player. The PropertiesChanged signal is monitored for any changes to the player.
-- Due to the limitation in specifying specific things to watch aggressive caching must
-- be but inplace to limit the number of changes actually performed due to the large number
-- of triggered callbacks per change.
local watch_player_registered = false
local function watch_player(override)
    if watch_player_registered == false then
        logger.debug("[spotifyd] player watch not setup, registering watch")
        watch_player_registered = true
    end

    --  Don't double register the player dbus watch
    if watch_player_registered == true and override == false then
        logger.debug("[spotifyd] attempt to double register player watch")
        -- return
    end

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
    local watch_proxy = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.freedesktop.DBus",
        path = "/org/freedesktop/DBus",
        interface = "org.freedesktop.DBus",
        flags = Gio.DBusSignalFlags.NONE
    })
    logger.debug("[spotifyd] setting up watch")
    watch_proxy:connect_signal(
        function(_, changed, _)
            -- The spotify dbus is used for player interactions since this isn't
            -- fully features in spotifyd
            if changed == "org.mpris.MediaPlayer2.spotify" then
                logger.debug("[spotifyd] connect signal changed for spotify dbus name")
                watch_player(true)
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
            watch_player(false)
            properties_changed()
        end
    end
end

-- Don't start timers until a device is connected on dbus that it can proxy and watch
watch()
