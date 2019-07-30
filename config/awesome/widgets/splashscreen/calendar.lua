-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- The calendar module creates a calendar widget.
-- @module calendar
local calendar = {}

local function set_buttons(widget)
    widget:buttons(
        gears.table.join(
            -- Left Click - Reset date to current date
            awful.button({ }, 1, function ()
                widget.date = os.date('*t')
                -- widget:emit_signal("button:press", {})
            end),
            -- Scroll - Move to previous or next month
            awful.button({ }, 4, function ()
                local time = os.time(widget.date)
                local yesterday = time - (60 * 60 * 24)
                widget.date = os.date("*t", yesterday)
            end),
            awful.button({ }, 5, function ()
                local time = os.time(widget.date)
                local yesterday = time + (60 * 60 * 24)
                widget.date = os.date("*t", yesterday)
            end)
        )
    )
end

-- Creates a widget with details about a user.
-- @return a wibox.widget with details about a user
function calendar.widget(config)
    local function decorate_cell(widget, flag, _)
        if flag == 'monthheader' and not config.styles.monthheader then
            flag = 'header'
        end

        -- nordic.util.log(widget)
        local props = config.styles[flag] or {}
        if props.markup and widget.get_text and widget.set_markup then
            widget:set_markup(props.markup(widget:get_text()))
        end

        local default_fg = beautiful.xcolor7
        local default_bg = beautiful.xcolor0.."00"

        return wibox.widget {
            {
                widget,
                margins = (props.padding or 2) + (props.border_width or 0),
                widget  = wibox.container.margin
            },
            shape              = props.shape,
            shape_border_color = props.border_color or beautiful.xbackground,
            shape_border_width = props.border_width or 0,
            fg                 = props.fg_color or default_fg,
            bg                 = props.bg_color or default_bg,
            widget             = wibox.container.background
        }
    end

    local widget = wibox.widget {
        date     = os.date('*t'),
        font     = "sans 16",
        long_weekdays = false,
        spacing  = dpi(3),
        fn_embed = decorate_cell,
        widget   = wibox.widget.calendar.month
    }
    set_buttons(widget)
    widget:connect_signal("date::update", function()
        widget.date = os.date('*t')
    end)

    return widget
end

return calendar
