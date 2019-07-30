-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- custom
local registry = require("widgets.registry")

-- The calendar module creates a calendar widget.
-- @module nordic.components.agenda.calendar
local calendar = {
    mt = {}
}

local function set_buttons(widget)
    widget:buttons(
        gears.table.join(
            -- Left Click - Reset date to current date
            awful.button({ }, 3, function ()
                widget.date = os.date("*t")
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

--- Creates an interactive calendar widget.
-- @return a wibox.widget calendar
function calendar.new(args)
    local styles = {
        ["month"] = {
            padding      = 20,
            fg_color     = beautiful.xcolor7,
            bg_color     = beautiful.polar_night_4 .. "00",
            border_width = 0,
        },
        ["focus"] = {
            fg_color = beautiful.xcolor1,
            bg_color = beautiful.xcolor5 .. "00",
            markup   = function(t) return '<b>' .. t .. '</b>' end,
        },
        ["header"] = {
            fg_color = beautiful.xcolor7,
            bg_color = beautiful.xcolor1 .. "00",
            markup   = function(t) return '<span font_desc="sans bold 24">' .. t .. '</span>' end,
        },
        ["weekday"] = {
            fg_color = beautiful.xcolor7,
            bg_color = beautiful.xcolor1 .. "00",
            padding  = 3,
            markup   = function(t) return '<b>' .. t .. '</b>' end,
        }
    }

    local function decorate_cell(widget, flag, _)
        if flag == 'monthheader' and not styles.monthheader then
            flag = 'header'
        end

        -- nordic.util.log(widget)
        local props = styles[flag] or {}
        if props.markup and widget.get_text and widget.set_markup then
            widget:set_markup(props.markup(widget:get_text()))
        end

        local default_fg = beautiful.xcolor7
        local default_bg = beautiful.xcolor0.."00"

        local cell = wibox.widget {
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

        cell:buttons(
            gears.table.join(
                awful.button({ }, 1, function()
                    if flag == "normal" and widget.get_text then
                        registry.emit("agenda::calendar", "update::value", { day = tonumber(widget:get_text()) })
                    end
                end)
            )
        )

        return cell
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

    -- Update the day of the date to the provided day
    widget:connect_signal("update::value", function(self, params)
        self.date = {
            year = self.date.year,
            yday = self.date.yday,
            month = self.date.month,
            wday = self.date.wday,
            day = params.day,
            hour = self.date.hour,
            min = self.date.min,
            sec = self.date.sec,
            isdst = self.date.isdst
        }
        registry.emit("agenda::meetings", "update::value", self.date)
    end)

    registry.add("agenda::calendar", widget)

    return widget
end

function calendar.mt:__call(...) --luacheck: no unused args
    return calendar.new(...)
end

return setmetatable(calendar, calendar.mt)
