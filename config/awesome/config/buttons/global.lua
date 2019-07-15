-- awesome modules
local awful = require("awful")
local naughty = require("naughty")
local gears = require("gears")
-- custom modules
local helpers = require("helpers")
local registry = require("widgets.registry")

local keys = gears.table.join(
    awful.button({ }, 1, function ()
        registry.emit("menu", "menu::hide")
        registry.emit("dashboard", "dashboard::close")
        -- registry.emit("sidebar", "sidebar::hide")
        naughty.destroy_all_notifications()

        local function double_tap()
          local uc = awful.client.urgent.get()
          -- If there is no urgent client, go back to last tag
          if uc == nil then
            awful.tag.history.restore()
          else
            awful.client.urgent.jumpto()
          end
        end
        helpers.single_double_tap(function() end, double_tap)
    end),

    awful.button({ }, 3, function ()
        registry.emit("menu", "menu::toggle")
    end),

    -- Middle button - Toggle start scren
    awful.button({ }, 2, function ()
        registry.emit("splashscreen", "splashscreen::show")
    end),

    -- Scrolling - Switch tags
    awful.button({ }, 4, awful.tag.viewprev),
    awful.button({ }, 5, awful.tag.viewnext),

    -- Side buttons - Control volume
    awful.button({ }, 9, function () awful.spawn.with_shell("volume-control.sh up") end),
    awful.button({ }, 8, function () awful.spawn.with_shell("volume-control.sh down") end)
)

return keys
