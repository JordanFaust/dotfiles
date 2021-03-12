-- awesome modules
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local registry = require("widgets.registry")

-- @module date
local date = {}

local function weekday_widget(config)
    local weekday = wibox.widget.textclock("%A")
    weekday.font = config.weekday_font
    weekday.fg = beautiful.xcolor0
    weekday.align = "center"
    weekday.valign = "center"

    return weekday
end

local function day_widget(config)
    local day = wibox.widget.textclock("%d")
    day.font = config.day_font
    day.fg = beautiful.xcolor0
    day.align = "center"
    day.valign = "center"
    day.markup = "<span foreground='" .. beautiful.xcolor1 .."'>" .. day.text .. "</span>"
    day:connect_signal("widget::redraw_needed", function ()
        day.markup = "<span foreground='" .. beautiful.xcolor1 .."'>" .. day.text .. "</span>"
        -- Also update the calendar widget ;)
        registry.emit("splashscreen::calendar", "date::update")
    end)

    return day
end

-- Creates the date widget
--
-- @param config the config for the component
-- @param calendar The calendar widget displayed with the date
-- @return a wibox.widget to display the date
function date.widget(config)
    local weekday = weekday_widget(config)
    local day = day_widget(config)

    return wibox.widget {
        weekday,
        day,
        layout = wibox.layout.align.vertical
    }
end

-- function Date:_handle_signals(day)
-- end
return date
