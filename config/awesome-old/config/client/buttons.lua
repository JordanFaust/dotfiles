-- awesome modules
local awful = require("awful")
local gears = require("gears")
-- custom modules
local mod = require("config.keys.mod")

local client = _G.client

local superkey = mod.superkey

local buttons = gears.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ superkey }, 1, awful.mouse.client.move),
    awful.button({ superkey }, 2, function (c) c:kill() end),
    awful.button({ superkey }, 3, function(c)
            awful.mouse.client.resize(c)
    end)
)

return buttons
