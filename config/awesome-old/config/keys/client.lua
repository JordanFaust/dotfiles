-- awesome modules
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
-- custom modules
local globals = require("globals")
local mod = require("config.keys.mod")

local ctrlkey = mod.ctrlkey
local superkey = mod.superkey
local shiftkey = mod.shiftkey

local keys =  gears.table.join(
    awful.key({ superkey }, "t",
        function (c)
            -- Don't toggle if titlebars are used as borders
            if not beautiful.titlebars_imitate_borders then
                awful.titlebar.toggle(c)
            end
        end,
        {description = "toggle titlebar", group = "client"}
    ),

    -- Toggle titlebar (for all visible clients in selected tag)
    awful.key({ superkey, shiftkey }, "t",
        function (_)
            --local s = awful.screen.focused()
            local clients = awful.screen.focused().clients
            for _, c in pairs(clients) do
                -- Don't toggle if titlebars are used as borders
                if not beautiful.titlebars_imitate_borders then
                    awful.titlebar.toggle(c)
                end
            end
        end,
        {description = "toggle titlebar", group = "client"}
    ),

    -- N for normal window
    awful.key({ superkey, ctrlkey  }, "n",
        function (c)
          c.width = globals.screen_width * 0.45
          c.height = globals.screen_height * 0.5
          c.floating = true
          awful.placement.centered(c,{honor_workarea=true})
          c:raise()
        end,
        {description = "normal mode", group = "client"}
    ),

    awful.key({ superkey, shiftkey   }, "q",
        function (c)
            c:kill()
        end,
        {description = "close", group = "client"}
    ),

    -- Toggle floating
    awful.key({ superkey, ctrlkey }, "space",
        function(_)
            local current_layout = awful.layout.getname(awful.layout.get(awful.screen.focused()))
            if current_layout ~= "floating" then
                awful.client.floating.toggle()
            end
            --c:raise()
        end,
        {description = "toggle floating", group = "client"}
    ),

    awful.key({ superkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}
    ),

    awful.key({ superkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "(un)maximize", group = "client"}
    ),

    awful.key({ superkey, ctrlkey }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = "(un)maximize vertically", group = "client"}
    ),

    awful.key({ superkey, shiftkey   }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        {description = "(un)maximize horizontally", group = "client"}
    )
)

return keys
