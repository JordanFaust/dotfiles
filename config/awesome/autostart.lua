local awful = require("awful")
local filesystem = require("gears.filesystem")

-- STARTUP APPS
local function run_once(name, cmd)
    local command = string.format("pgrep -u $USER -x %s > /dev/null || (%s)", name, cmd)

    awful.spawn.easy_async_with_shell(command, function() end)
end

local function app_run_once(name, cmd)
    local command = string.format("xdotool search --class %s > /dev/null || (%s)", name, cmd)

    awful.spawn.easy_async_with_shell(command, function() end)
end

local process = {
    -- Startup Processes
    feh = {
        name = "feh",
        command = "feh --bg-fill " .. filesystem.get_configuration_dir() .. "wallpaper.jpg"
    },
    picom =  {
        name = "picom",
        command = "picom"
    },
}

local apps = {
    emacs = {
        name = "Doom",
        command = "emacs"
    },
    firefox = {
        name = "Firefox",
        command = "firefox"
    },
    twitch = {
        name = "twitch",
        command = "firefox --no-remote -P default --class twitch"
    },
    kitty = {
        name = "Terminal",
        command = "kitty -e /usr/bin/zsh"
    },
    slack = {
        name = "Slack",
        command = "slack"
    },
    spotify = {
        name = "Spotify",
        command = "spotify"
    },
}

-- Autostart processes if they are not already running
for _, config in pairs(process) do
    run_once(config.name, config.command)
end

-- Autostart default applications if they are not already running
for app, config in pairs(apps) do
    app_run_once(app, config.command)
end
