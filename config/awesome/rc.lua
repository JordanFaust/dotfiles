--[[
    ___       ___       ___       ___       ___       ___       ___
   /\  \     /\__\     /\  \     /\  \     /\  \     /\__\     /\  \
  /::\  \   /:/\__\   /::\  \   /::\  \   /::\  \   /::L_L_   /::\  \
 /::\:\__\ /:/:/\__\ /::\:\__\ /\:\:\__\ /:/\:\__\ /:/L:\__\ /::\:\__\
 \/\::/  / \::/:/  / \:\:\/  / \:\:\/__/ \:\/:/  / \/_/:/  / \:\:\/  /
   /:/  /   \::/  /   \:\/  /   \::/  /   \::/  /    /:/  /   \:\/  /
   \/__/     \/__/     \/__/     \/__/     \/__/     \/__/     \/__/
--]]

----------------------------------------------
-- awesome modules
local beautiful = require("beautiful")
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")

-- ~~ Noodle Cleanup Script ~~
-- Some of my widgets (mpd, volume) rely on scripts that have to be
-- run persistently in the background.
-- They sleep until mpd/volume state changes, in an infinite loop.
-- As a result when awesome restarts, they keep running in background, along with the new ones that
-- are created after the restart.
-- This script cleans up the old processes.
awful.spawn.with_shell("~/.config/awesome/awesome-cleanup.sh")

local awesome = _G.awesome
local client = _G.client
local mouse = _G.mouse
local tag = _G.tag
local root = _G.root

local helpers = require("helpers")

-- theme
local theme_dir = os.getenv("HOME") .. "/.config/awesome/themes/"
beautiful.init( theme_dir .. "skyfall" .. "/theme.lua" )

-- Layout
require("layout")

-- Init all modules
require("module.notifications")
require("module.autostart")
require("module.titlebars")
require("module.exitscreen")
require("module.splashscreen")
require("module.hotkeys")
require("module.menu")

-- Setup all configuration
require("config.client")
require("config.tags")
root.keys(require("config.keys.global"))
root.buttons(require("config.buttons.global"))

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set every new window as a slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
        not c.size_hints.user_position
        and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end

end)


-- Hide titlebars if required by the theme
client.connect_signal("manage", function (c)
    if not beautiful.titlebars_enabled then
        awful.titlebar.hide(c)
    end
end)

-- Rounded corners
if beautiful.border_radius ~= 0 then
    client.connect_signal("manage", function (c, _)
        if not c.fullscreen then
            c.shape = helpers.rrect(beautiful.border_radius)
        end
    end)

    -- Fullscreen clients should not have rounded corners
    client.connect_signal("property::fullscreen", function (c)
        if c.fullscreen then
            c.shape = helpers.rect()
        else
            c.shape = helpers.rrect(beautiful.border_radius)
        end
    end)
end

-- When a client starts up in fullscreen, resize it to cover the fullscreen a short moment later
-- Fixes wrong geometry when titlebars are enabled
--client.connect_signal("property::fullscreen", function(c)
client.connect_signal("manage", function(c)
    if c.fullscreen then
        gears.timer.delayed_call(function()
            if c.valid then
                c:geometry(c.screen.geometry)
            end
        end)
    end
end)

-- Apply shapes
-- beautiful.notification_shape = helpers.infobubble(beautiful.notification_border_radius)
beautiful.notification_shape = helpers.rrect(beautiful.notification_border_radius)
beautiful.snap_shape = helpers.rrect(beautiful.border_radius * 2)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Set mouse resize mode (live or after)
awful.mouse.resize.set_mode("live")

-- Floating: restore geometry
tag.connect_signal('property::layout',
    function(t)
        for _, c in ipairs(t:clients()) do
            if awful.layout.get(mouse.screen) == awful.layout.suit.floating then
                -- Geometry x = 0 and y = 0 most probably means that the
                -- clients have been spawned in a non floating layout, and thus
                -- they don't have their floating_geometry set properly.
                -- If that is the case, don't change their geometry
                local cgeo = awful.client.property.get(c, 'floating_geometry')
                if cgeo ~= nil then
                    if not (cgeo.x == 0 and cgeo.y == 0) then
                        c:geometry(awful.client.property.get(c, 'floating_geometry'))
                    end
                end
                --c:geometry(awful.client.property.get(c, 'floating_geometry'))
            end
        end
    end
)

client.connect_signal('manage',
    function(c)
        if awful.layout.get(mouse.screen) == awful.layout.suit.floating then
            awful.client.property.set(c, 'floating_geometry', c:geometry())
        end
    end
)

client.connect_signal('property::geometry',
    function(c)
        if awful.layout.get(mouse.screen) == awful.layout.suit.floating then
            awful.client.property.set(c, 'floating_geometry', c:geometry())
        end
    end
)

-- Make rofi able to unminimize minimized clients
client.connect_signal("request::activate",
    function(c, context, hints)
        if not awesome.startup then
            if c.minimized then
                c.minimized = false
            end
            awful.ewmh.activate(c, context, hints)
        end
    end
)
