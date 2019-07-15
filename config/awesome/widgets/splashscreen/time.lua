-- awesome modules
local wibox = require("wibox")
local beautiful = require("beautiful")

-- @module time
local time = {}

local function hours_widget(config)
    local hours = wibox.widget.textclock("%H  ")
    hours.font = config.hours_font
    hours.align = "center"
    hours.valign = "center"

    return hours
end

local function minutes_widget(config)
    local minutes = wibox.widget.textclock("  %M")
    minutes.font = config.minutes_font
    minutes.align = "center"
    minutes.valign = "center"
    minutes.markup = "<span foreground='" .. beautiful.xcolor14 .."'>" .. minutes.text .. "</span>"
    minutes:connect_signal("widget::redraw_needed", function ()
        minutes.markup = "<span foreground='" .. beautiful.xcolor14 .."'>" .. minutes.text .. "</span>"
    end)

    return minutes
end

function time.widget(config)
    local hours = hours_widget(config)
    local minutes = minutes_widget(config)

    return wibox.widget {
        hours,
        minutes,
        layout = wibox.layout.fixed.vertical
    }
end

return time
