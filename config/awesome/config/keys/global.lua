-- awesome modules
local awful = require("awful")
local naughty = require("naughty")
local gears = require("gears")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
-- custom modules
local registry = require("widgets.registry")
local mod = require("config.keys.mod")
local apps = require('config.apps')

local awesome = _G.awesome
local client = _G.client

local ctrlkey = mod.ctrlkey
local superkey = mod.superkey
local altkey = mod.altkey
local shiftkey = mod.shiftkey

-- helpers.debug("loading global keys")

-- @module keys
local keys = gears.table.join(

    -- Restart Awesome
    awful.key({ superkey, ctrlkey }, "r", awesome.restart,
        {description = "restart awesome", group = "awesome"}
    ),

    -- Show Hotkeys
    awful.key({ superkey }, "s",
        function()
            -- for _, data in pairs(awful.key.hotkeys) do
            --     helpers.debug("key " .. data.description)
            -- end
            registry.emit("hotkeys", "hotkeys::show")
        end,
        {description = "show help", group = "awesome"}
    ),

    -- Sidebar
    awful.key({ superkey }, "b",
        function()
            registry.emit("dashboard", "dashboard::toggle", { search = false })
        end,
        {description = "show dashboard", group = "awesome"}
    ),

    -- Sidebar Search
    awful.key({ superkey }, "space",
        function()
            -- awful.spawn.with_shell("rofi-apps")
            registry.emit("dashboard", "dashboard::toggle", { search = true })
        end,
        {description = "rofi apps", group = "launcher"}
    ),

    -- {{{ Window Movement

    -- Place window to the right
    awful.key({ superkey }, "Right",
        function()
            local c = client.focus
            c:geometry({
                x = dpi(1860),
                y = dpi(80),
                width = dpi(1728),
                height = dpi(1400)
            })
            c.floating = true
            -- awful.placement.centered(c,{honor_workarea=true})
            c:raise()
        end,
        {description = "move window right", group = "client"}
    ),

    -- Place window to the left
    awful.key({ superkey }, "Left",
        function()
            local c = client.focus
            c:geometry({
                x = dpi(80),
                y = dpi(80),
                width = dpi(1728),
                height = dpi(1400)
            })
            c.floating = true
            c:raise()
        end,
        {description = "move window right", group = "client"}
    ),
    -- }}}

    -- {{{ Common workflow focus windows

    -- Focus Firefox
    awful.key({ superkey, ctrlkey }, "f",
        function()
            apps:launch_or_focus("firefox")
        end,
        {description = "focus firefox", group = "client"}
    ),

    -- Focus Emacs
    awful.key({ superkey, ctrlkey }, "e",
        function()
            apps:launch_or_focus("emacs")
        end,
        {description = "focus emacs", group = "client"}
    ),

    -- Focus Terminal
    awful.key({ superkey, ctrlkey }, "t",
        function()
            apps:launch_or_focus("urxvt")
        end,
        {description = "focus terminal", group = "client"}
    ),

    -- Focus Terminal
    awful.key({ superkey, ctrlkey }, "s",
        function()
            apps:launch_or_focus("slack")
        end,
        {description = "focus slack", group = "client"}
    ),

    -- }}}

    -- Focus client by index (cycle through clients)
    awful.key({ superkey }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ superkey }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),

    -- Kill all visible clients for the current tag
    awful.key({ superkey, altkey }, "q",
        function ()
            local clients = awful.screen.focused().clients
            for _, c in pairs(clients) do
               c:kill()
            end
        end,
        {description = "kill all visible clients for tag", group = "gaps"}
    ),

    -- Main menu
    awful.key({ superkey, shiftkey  }, "v",
        function ()
            registry.emit("menu", "menu::show")
            -- mymainmenu:show()
        end,
        {description = "show main menu", group = "awesome"}
    ),

    -- Logout, Shutdown, Restart, Suspend, Lock
    awful.key({ superkey }, "Escape",
        function ()
            registry.emit("exitscreen", "exitscreen::show")
        end,
        {description = "exit", group = "awesome"}
    ),

    awful.key({ superkey, shiftkey }, "n",
        function ()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                client.focus = c
                c:raise()
            end
        end,
        {description = "restore minimized", group = "client"}
    ),

    -- Dismiss notifications
    awful.key( { ctrlkey }, "space", function()
        naughty.destroy_all_notifications()
    end,
              {description = "dismiss notification", group = "notifications"}),

    -- Screenshots
    awful.key( { }, "Print",
        function()
            awful.spawn.with_shell("screenshot")
        end,
        {description = "take full screenshot", group = "screenshots"}
    ),

    awful.key( { superkey, shiftkey }, "c",
        function()
            awful.spawn.with_shell("screenshot -s")
        end,
        {description = "select area to capture", group = "screenshots"}
    ),

    awful.key( { superkey, ctrlkey }, "c",
        function()
            awful.spawn.with_shell("screenshot -c")
        end,
        {description = "select area to copy to clipboard", group = "screenshots"}
    ),

    awful.key( { superkey }, "Print",
        function()
            awful.spawn.with_shell("screenshot -b")
        end,
        {description = "browse screenshots", group = "screenshots"}
    ),

    awful.key( { superkey, shiftkey }, "Print",
        function()
            awful.spawn.with_shell("screenshot -e")
        end,
        {description = "edit most recent screenshot with gimp", group = "screenshots"}
    ),

    -- Set max layout
    awful.key({ superkey }, "w",
        function()
            awful.layout.set(awful.layout.suit.max)
        end,
        {description = "set max layout", group = "tag"}
    ),

    -- Set tiled layout
    awful.key({ superkey }, "s",
        function()
            awful.layout.set(awful.layout.suit.tile)
        end,
        {description = "set tiled layout", group = "tag"}
    ),

    -- Set floating layout
    awful.key({ superkey, shiftkey }, "s",
        function()
            awful.layout.set(awful.layout.suit.floating)
        end,
        {description = "set floating layout", group = "tag"}
    ),

    -- Toggle sidebar
    awful.key({ superkey }, "grave",
        function()
            -- sidebar.visible = not sidebar.visible
        end,
        {description = "show or hide sidebar", group = "awesome"}
    ),

    -- Toggle wibar
    awful.key({ superkey, shiftkey }, "b",
        function()
            local s = awful.screen.focused()
            s.mywibox.visible = not s.mywibox.visible
            if beautiful.wibar_detached then
                s.useless_wibar.visible = not s.useless_wibar.visible
            end
        end,
        {description = "show or hide wibar", group = "awesome"}
    )
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
local ntags = 10
for i = 1, ntags do

    -- Hack to only show tags 1 and 9 in the shortcut window (mod+s)
    local view_description, toggle_description, move_description
    if i == 1 or i == 9 then
        view_description = {description = "view tag #", group = "tag"}
        toggle_description = {description = "toggle tag #", group = "tag"}
        move_description = {description = "move focused client to tag #", group = "tag"}
    end

    keys = gears.table.join(keys,
        -- View tag only.
        awful.key({ superkey }, "#" .. i + 9,
            function ()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                local current_tag = screen.selected_tag
                -- Tag back and forth:
                -- If you try to focus the same tag you are at,
                -- go back to the previous tag.
                -- Useful for quick switching after for example
                -- checking an incoming chat message at tag 2
                -- and coming back to your work at tag 1
                if tag then
                    if tag == current_tag then
                        awful.tag.history.restore()
                    else
                        tag:view_only()
                    end
                end
                -- Simple tag view
                --if tag then
                    --tag:view_only()
                --end
            end,
            view_description
        ),
        -- Toggle tag display.
        awful.key({ superkey, ctrlkey }, "#" .. i + 9,
            function ()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            toggle_description
        ),

        -- Move client to tag.
        awful.key({ superkey, shiftkey }, "#" .. i + 9,
            function ()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            move_description
        )
    )
end

return keys
