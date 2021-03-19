local awful = require("awful")
local gears = require("gears")
local wibox = require "wibox"
local start = require(... .. ".start")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local awestore = require("awestore")
local naughty = require("naughty")

--[[
awesome.connect_signal("widgets::dashboard::show",
function() dash_manager.dash_show() end)
--]]

--[[
awesome.connect_signal("widgets::notif_panel::show", function(s)
    notif.screen = s
    notif.visible = not notif.visible
    awesome.emit_signal("widgets::notif_panel::status", notif.visible)
end)
--]]

-- start.placement = awful.placement.bottom_left
start.x = dpi(-450)
-- start.y = dpi(awful.screen.focused().geometry.height - 1000 -
--                   beautiful.wibar_height + 2 - 48)
start.y = dpi(-1000)
start.visible = true

local panel_anim = awestore.tweened(-450, {
    duration = 200,
    easing = awestore.easing.circ_in_out
})

panel_anim:subscribe(function(x) start.x = x end)

local start_close = false
awesome.connect_signal("widgets::start::show", function(s)
    start.screen = s
    start_close = not start_close

    start.visible = start_close
    awesome.emit_signal("widgets::start::status", start.visible)
    -- if start.visible then
    --     start.visable = false
    -- else
    --     start.visable = true
    -- end



    -- if not start_close then
    --     start.visible = true
    --     panel_anim:set(-1)
    -- else
    --     panel_anim:set(-450)
    --     local timer = gears.timer {
    --         timeout = 0.21,
    --         single_shot = true,
    --         callback = function()
    --             naughty.notification {
    --                 urgency = "critical",
    --                 title   = "Side Panel",
    --                 message = "timer done for annimation"
    --             }
    --             start.visible = false
    --         end
    --     }:again()
    -- end

    -- awesome.emit_signal("widgets::start::status", start.visible)
end)
