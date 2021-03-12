local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local helpers = require("helpers")
local pad = helpers.pad
local registry = require("widgets.registry")

-- @module exitscreen
local exitscreen = {}

local awesome = _G.awesome

-- Common configuration across all sidebar components
-- @name config
local config = {
    text = {
        font = beautiful.exit_screen_font or "sans 14"
    },
    icon = {
        size = beautiful.exit_screen_icon_size or 140
    }
}

-- Keygrabber reference
-- @name grabber
local grabber = nil

-- -- Appearance
-- local icon_size = beautiful.exit_screen_icon_size or 140
-- local text_font = beautiful.exit_screen_font or "sans 14"

-- Commands
local function poweroff_command()
    awful.spawn.with_shell("poweroff")
    awful.keygrabber.stop(grabber)
end

local function reboot_command()
    awful.spawn.with_shell("reboot")
    awful.keygrabber.stop(grabber)
end

local function suspend_command()
    awful.spawn.with_shell("systemctl suspend")
    -- awful.spawn.with_shell("i3lock & systemctl suspend")
    registry.emit("exitscreen", "exitscreen::hide")
end

local function exit_command()
    awesome.quit()
end

local function lock_command()
    awful.spawn.with_shell("i3lock")
    registry.emit("exitscreen", "exitscreen::hide")
end

local function goodbye_widget()
    local username = os.getenv("USER")
    -- Capitalize username
    local widget = wibox.widget.textbox("Goodbye " .. username:sub(1,1):upper()..username:sub(2))
    widget.font = "sans 70"

    return widget
end

local function poweroff_widget()
    local icon = wibox.widget.imagebox(beautiful.poweroff_icon)
    icon.resize = true
    icon.forced_width = config.icon.size
    icon.forced_height = config.icon.size
    local textbox = wibox.widget.textbox("Poweroff")
    textbox.font = config.text.font

    local widget = wibox.widget{
        {
            nil,
            icon,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        {
            pad(1),
            textbox,
            pad(1),
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        -- forced_width = 100,
        layout = wibox.layout.fixed.vertical
    }

    widget:buttons(
        gears.table.join(
            awful.button({ }, 1, function ()
                poweroff_command()
            end)
        )
    )

    return widget
end

local function reboot_widget()
    local icon = wibox.widget.imagebox(beautiful.reboot_icon)
    icon.resize = true
    icon.forced_width = config.icon.size
    icon.forced_height = config.icon.size

    local textbox = wibox.widget.textbox("Reboot")
    textbox.font = config.text.font

    local widget = wibox.widget{
        {
            nil,
            icon,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        {
            nil,
            textbox,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        -- forced_width = 100,
        layout = wibox.layout.fixed.vertical
    }
    widget:buttons(gears.table.join(
        awful.button({ }, 1, function ()
            reboot_command()
        end)
    ))

    return widget
end

local function suspend_widget()
    local icon = wibox.widget.imagebox(beautiful.suspend_icon)
    icon.resize = true
    icon.forced_width = config.icon.size
    icon.forced_height = config.icon.size
    local textbox = wibox.widget.textbox("Suspend")
    textbox.font = config.text.font

    local widget = wibox.widget{
        {
            nil,
            icon,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        {
            nil,
            textbox,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        -- forced_width = 100,
        layout = wibox.layout.fixed.vertical
    }

    widget:buttons(gears.table.join(
        awful.button({ }, 1, function ()
            suspend_command()
        end)
    ))

    return widget
end

local function exit_widget()
    local icon = wibox.widget.imagebox(beautiful.exit_icon)
    icon.resize = true
    icon.forced_width = config.icon.size
    icon.forced_height = config.icon.size

    local textbox = wibox.widget.textbox("Exit")
    textbox.font = config.text.font

    local widget = wibox.widget{
        {
            nil,
            icon,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        {
            nil,
            textbox,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        -- forced_width = 100,
        layout = wibox.layout.fixed.vertical
    }

    widget:buttons(gears.table.join(
        awful.button({ }, 1, function ()
            exit_command()
        end)
    ))

    return widget
end

local function lock_widget()
    local icon = wibox.widget.imagebox(beautiful.lock_icon)
    icon.resize = true
    icon.forced_width = config.icon.size
    icon.forced_height = config.icon.size
    local textbox = wibox.widget.textbox("Lock")
    textbox.font = config.text.font

    local widget = wibox.widget{
        {
            nil,
            icon,
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        {
            pad(1),
            textbox,
            pad(1),
            expand = "none",
            layout = wibox.layout.align.horizontal
        },
        -- forced_width = 100,
        layout = wibox.layout.fixed.vertical
    }

    widget:buttons(gears.table.join(
        awful.button({ }, 1, function ()
            lock_command()
        end)
    ))

    return widget
end

-- Get screen geometry
local screen_width = awful.screen.focused().geometry.width
local screen_height = awful.screen.focused().geometry.height

local function hide()
    awful.keygrabber.stop(grabber)
    local widget = registry.get("exitscreen")
    widget.visible = false
end

local function show()
    grabber = awful.keygrabber.run(function(_, key, event)
        if event == "release" then return end

        if     key == 's'    then
            suspend_command()
            -- 'e' for exit
        elseif key == 'e'    then
            exit_command()
        elseif key == 'l'    then
            lock_command()
        elseif key == 'p'    then
            poweroff_command()
        elseif key == 'r'    then
            reboot_command()
        elseif key == 'Escape' or key == 'q' or key == 'x' then
            hide()
        end
    end)
    local widget = registry.get("exitscreen")
    widget.visible = true
end

local function buttons()
    return gears.table.join(
        -- Middle click - Hide exit_screen
        awful.button({ }, 2, function ()
                hide()
        end),
        -- Right click - Hide exit_screen
        awful.button({ }, 3, function ()
                hide()
        end)
    )
end

local function set_signals(widget)
    widget:connect_signal("exitscreen::show", function()
        show()
    end)
    widget:connect_signal("exitscreen::hide", function()
        hide()
    end)
end

function exitscreen.create()
    local widget = wibox({
        x = 0,
        y = 0,
        visible = false,
        ontop = true,
        type = "dock",
        height = screen_height,
        width = screen_width
    })

    widget.bg = beautiful.exit_screen_bg or beautiful.wibar_bg or "#111111"
    widget.fg = beautiful.exit_screen_fg or beautiful.wibar_fg or "#FEFEFE"

    registry.add("exitscreen", widget)
    widget:buttons(buttons())
    set_signals(widget)

    local goodbye = goodbye_widget()
    local poweroff = poweroff_widget()
    local reboot = reboot_widget()
    local suspend = suspend_widget()
    local exit = exit_widget()
    local lock = lock_widget()

    widget:setup {
        nil,
        {
            {
                nil,
                goodbye,
                nil,
                expand = "none",
                layout = wibox.layout.align.horizontal
            },
            {
                nil,
                {
                    -- {
                    poweroff,
                    pad(3),
                    reboot,
                    pad(3),
                    suspend,
                    pad(3),
                    exit,
                    pad(3),
                    lock,
                    layout = wibox.layout.fixed.horizontal
                    -- },
                    -- widget = exit_screen_box
                },
                nil,
                expand = "none",
                layout = wibox.layout.align.horizontal
                -- layout = wibox.layout.fixed.horizontal
            },
            layout = wibox.layout.fixed.vertical
        },
        nil,
        expand = "none",
        layout = wibox.layout.align.vertical
    }

    return widget
end

exitscreen.create()
