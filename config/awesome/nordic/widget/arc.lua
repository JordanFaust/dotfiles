-- awesome modules
local beautiful = require("beautiful")
local wibox = require("wibox")
-- custom modules
local color = require("nordic.color")

-- @module nordic.widget.arch
local arc = { mt = {} }

--- Create a new arc widget.
-- @method new
-- @tparam string args.color The color of the arc chart
-- @tparam string args.background The background color of the arc chart
-- @tparam int args.max The max value of the arc chart
-- @tparam int args.min The min value of the arc chart
-- @tparam int args.default The default value of the arc chart
-- @tparam int args.thickness The thickness of the arc chart
-- @tparam widget args.wrapper The widget to wrap the arc chart in
-- @return A nordic arcchart
function arc.new(args)
    local colour = args.color or beautiful.xcolor6
    local background = color.darken(colour, 80) -- args.background or beautiful.xcolor4
    local max = args.max or 99
    local min = args.min or 0
    local default = args.default or 0
    local thickness = args.thickness or 30

    local wrapper = args.wrapper or nil

    local widget = wibox.widget {
        wrapper,
        colors = { colour },
        bg = background,
        max_value = max,
        min_value = min,
        value = default,
        rounded_edge = true,
        thickness = thickness,
        start_angle = math.pi + math.pi / 2,
        widget = wibox.container.arcchart,
    }

    return widget
end

function arc.mt:__call(...)
    return arc.new(...)
end

return setmetatable(arc, arc.mt)
