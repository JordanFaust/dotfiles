-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local naughty = require("naughty")
local keygrabber = require("awful.keygrabber")
local helpers = require("helpers")
-- custom modules
-- local Widget = require("lib.widget")
local registry = require("widgets.registry")
local user = require("widgets.splashscreen.user")
local calendar = require("widgets.splashscreen.calendar")
local time = require("widgets.splashscreen.time")
local date = require("widgets.splashscreen.date")
local bookmarks = require("widgets.splashscreen.bookmarks")
local links = require("widgets.splashscreen.links")
local notifications = require("widgets.splashscreen.notifications")
local screenshot = require("widgets.splashscreen.screenshot")
-- local email = require("widgets.splashscreen.email")
local spotify = require("widgets.shared.spotify")

-- Static configuration of the splash screen
-- @name config
local config = {
    -- The radius used for corner roundness
    -- @name box_radius
    box_radius = beautiful.start_screen_box_border_radius or dpi(12),
    -- The spacing between widget box containers in the start screen
    -- @name box_gap
    box_gap = dpi(6),
    -- TODO: Move to globals
    -- The width of the current focused screen
    -- @name screen_width
    screen_width = awful.screen.focused().geometry.width,
    -- The height of the current focused screen
    -- @name screen_height
    screen_height = awful.screen.focused().geometry.height
}

--- @module component.splashscreen
local splashscreen = {}

-- The awful.keygrabber function used to capture key input for closing the splashscreen
-- @name screen_grabber
local screen_grabber = nil

-- Helper function that puts a widget inside a box with a specified background color
-- Invisible margins are added so that the boxes created with this function are evenly separated
-- The widget_to_be_boxed is vertically and horizontally centered inside the box

-- Places the widget inside a box with the specified dimensions and background color.
--
-- <p> Creates a boxed widget using the specified dimensions and background color.
-- Invisible margins are added so that the boxes created are evenly separated. The boxed
-- widget is vertically and horizontally centered inside the box.
--
-- @param widget the wibox.widget to box
-- @param width the width of the box container
-- @param height the height of the box container
-- @param bg_color the background color of the containing box container
local function create_boxed_widget(widget, width, height, bg_color)
    local container = wibox.container.background()
    container.bg = bg_color
    container.forced_height = height
    container.forced_width = width
    container.shape = helpers.rrect(config.box_radius)

    return wibox.widget {
        -- Add margins
        {
            -- Add background color
            {
                -- Center widget_to_be_boxed horizontally
                nil,
                {
                    -- Center widget_to_be_boxed vertically
                    nil,
                    -- The actual widget goes here
                    widget,
                    nil,
                    layout = wibox.layout.align.vertical,
                    expand = "none"
                },
                nil,
                layout = wibox.layout.align.horizontal,
                expand = "none"
            },
            widget = container,
        },
        margins = config.box_gap,
        color = "#FF000000",
        widget = wibox.container.margin
    }
end

local function user_widget()
    local widget = user.widget({
            picture_height = dpi(130),
            picture_enabled = true,
            username_font = "sans bold 18",
            hostname_font = "sans italic 18"
    })
    registry.add("splashscreen::user", widget)
    return create_boxed_widget(widget, dpi(300), dpi(150), beautiful.xbackground)
end

local function calendar_widget()
    local styles = {
        ["month"] = {
            padding      = 20,
            fg_color     = beautiful.xcolor7,
            bg_color     = beautiful.xbackground.."00",
            border_width = 0,
        },
        ["focus"] = {
            fg_color = beautiful.xcolor1,
            bg_color = beautiful.xcolor5.."00",
            markup   = function(t) return '<b>' .. t .. '</b>' end,
        },
        ["header"] = {
            fg_color = beautiful.xcolor7,
            bg_color = beautiful.xcolor1.."00",
            markup   = function(t) return '<span font_desc="sans bold 24">' .. t .. '</span>' end,
        },
        ["weekday"] = {
            fg_color = beautiful.xcolor7,
            bg_color = beautiful.xcolor1.."00",
            padding  = 3,
            markup   = function(t) return '<b>' .. t .. '</b>' end,
        }
    }
    local widget = calendar.widget({styles = styles})
    registry.add("splashscreen::calendar", widget)

    return create_boxed_widget(widget, dpi(300), dpi(400), beautiful.xbackground)
end

local function time_widget()
    local widget = time.widget({ hours_font = "sans bold 30", minutes_font = "sans 30" })
    registry.add("splashscreen::time", widget)

    return create_boxed_widget(widget, dpi(150), dpi(150), beautiful.xbackground)
end

-- TODO: find a better way for signal dependency management
local function date_widget()
    local widget = date.widget({ weekday_font = "sans italic 20", day_font = "sans bold 30"})
    registry.add("splashscreen::date", widget)

    return create_boxed_widget(widget, dpi(150), dpi(150), beautiful.xbackground)
end

local function bookmarks_widget()
    local widget = bookmarks.widget({ font = "sans bold 16" })
    registry.add("splashscreen::bookmarks", widget)

    return create_boxed_widget(widget, dpi(200), dpi(300), beautiful.xbackground)
end

local function links_widget()
    local widget = links.widget({ font = "sans bold 16" })
    registry.add("splashscreen::links", widget)

    return create_boxed_widget(widget, dpi(200), dpi(180), beautiful.xbackground)
end

local function notifications_widget()
    local widget = notifications.widget({})
    registry.add("splashscreen::notifications", widget)

    local boxed = create_boxed_widget(widget, dpi(150), dpi(78), beautiful.xbackground)
    boxed:buttons(gears.table.join(
        -- Left click - Toggle notification state
        awful.button({ }, 1, function ()
            naughty.toggle()
            notifications:update_state()
        end)
    ))

    helpers.add_clickable_effect(boxed)

    return boxed
end

local function screenshot_widget()
    local widget = screenshot.widget()

    local boxed = create_boxed_widget(widget, dpi(150), dpi(78), beautiful.xbackground)
    boxed:buttons(gears.table.join(
        -- Left click - Take screenshot
        awful.button({ }, 1, function ()
            awful.spawn.with_shell("screenshot.sh")
        end),
        -- Right click - Take screenshot in 5 seconds
        awful.button({ }, 3, function ()
            naughty.notify({
                title = "Say cheese!",
                text = "Taking shot in 5 seconds",
                timeout = 4,
                icon = beautiful.screenshot_icon
            })
            awful.spawn.with_shell("sleep 5 && screenshot.sh")
        end)
    ))

    return boxed
end

local function spotify_widget()
    local overrides = {
        icon = {
            size = dpi(48),
            toggle = {
                icon = beautiful.playerctl_toggle_icon
            },
            previous = {
                icon = beautiful.playerctl_prev_icon
            },
            nxt = {
                icon = beautiful.playerctl_next_icon
            }
        },
        image = {
            size = dpi(150),
            resize = true
        },
        spotify = {
            font = "sans 11"
        }
    }
    local widget = spotify.splashscreen(overrides, {})
    return create_boxed_widget(widget, dpi(300), dpi(329), beautiful.xbackground)
end

--- Hide the splashscreen
local function hide()
    awful.keygrabber.stop(screen_grabber)
    local widget = registry.get("splashscreen")
    widget.visible = false
end

--- Make the splashscreen visible
local function show()
    -- Fix cursor sometimes turning into "hand1" right after showing the start_screen
    local original_cursor = "left_ptr"
    local w = _G.mouse.current_wibox
    if w then
        w.cursor = original_cursor
    end
    screen_grabber = keygrabber.run(function(_, key, event)
        if event == "release" then return end
        -- Press Escape or q or F1 to hide it
        if key == 'Escape' or key == 'q' or key == 'F1' then
            hide()
        end
    end)
    local widget = registry.get("splashscreen")
    widget.visible = true
end

--- Create the splashscreen widget and all child widgets.
--
-- @return a wibox.widget with all child widgets.
function splashscreen.create()
    local _user = user_widget()
    local _calendar = calendar_widget()
    local _time = time_widget()
    local _date = date_widget()
    local _bookmarks = bookmarks_widget()
    local _links = links_widget()
    local _notifications = notifications_widget()
    local _screenshot = screenshot_widget()
    local _spotify = spotify_widget()

    local container = wibox({
        x = 0,
        y = 0,
        visible = false,
        ontop = true,
        type = "dock",
        height = config.screen_height,
        width = config.screen_width
    })

    container.bg = beautiful.start_screen_bg or beautiful.exit_screen_bg or beautiful.wibar_bg or "#111111"
    container.fg = beautiful.start_screen_fg or beautiful.exit_screen_fg or beautiful.wibar_fg or "#FEFEFE"

    container:buttons(gears.table.join(
        -- Middle click - Hide start_screen
        awful.button({ }, 2, function ()
            hide()
        end)
    ))

    container:setup {
        -- Center boxes vertically
        nil,
        {
            -- Center boxes horizontally
            nil,
            {
                -- Column container
                {
                    -- Column 1
                    _user,
                    _spotify,
                    -- fortune_box,
                    layout = wibox.layout.fixed.vertical
                },
                {
                    -- Column 2
                    _time,
                    -- launcher_petals_box,
                    _notifications,
                    _screenshot,
                    _date,
                    layout = wibox.layout.fixed.vertical
                },
                {
                    -- Column 3
                    _bookmarks,
                    _links,
                    layout = wibox.layout.fixed.vertical
                },
                {
                    -- Column 4
                    _calendar,
                    -- brightness_box,
                    layout = wibox.layout.fixed.vertical
                },
                layout = wibox.layout.fixed.horizontal
            },
            nil,
            expand = "none",
            layout = wibox.layout.align.horizontal

        },
        nil,
        expand = "none",
        layout = wibox.layout.align.vertical
    }

    -- Add signals
    container:connect_signal("splashscreen::show", function ()
        show()
    end)
    container:connect_signal("splashscreen::hide", function ()
        hide()
    end)
    registry.add("splashscreen", container)

    return container
end

splashscreen.create()
