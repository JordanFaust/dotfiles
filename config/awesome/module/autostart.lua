-- awesome modules
local awful = require("awful")
-- custom modules
local apps = require("config.apps")

local function run_once(name, cmd)
    local command = string.format("pgrep -u $USER -x %s > /dev/null || (%s)", name, cmd)

    awful.spawn.easy_async_with_shell(command, function() end)
end

-- Autostart processes if they are not already running
for _, config in pairs(apps.startup) do
    run_once(config.name, config.command)
end

-- Autostart default applications if they are not already running
for app, config in pairs(apps.default) do
    run_once(app, config.command)
end
