-- awesome modules
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local wibox = require("wibox")
-- custom modules
local helpers = require("helpers")
local pad = helpers.pad
local registry = require("widgets.registry")
local libwidget = require("lib.widget")
local event = require("widgets.sidebar.meetings.event")
-- luarocks modules
local dkjson = require("dkjson")

-- @module meetings
local meetings = {}

local function set_children(widget, output)
    local data, _, err = dkjson.decode(output, 1, nil)
    if err then
        helpers.debug("failed to parse desktopevents/data.json")
    end

    -- Reset the widget before re-adding the child widgets
    widget:reset()
    local header = libwidget.textbox({ font = "sans bold 18"}, "centered")
    header.markup = "Meetings"
    widget:add(pad(1))
    widget:add(header)

    local events = {}

    for _, params in pairs(data["meetings"]) do
        local start_date = params["start"]["dateTime"]
        local end_date = params["end"]["dateTime"]
        local subject = params["subject"]
        local location = params["location"]["displayName"]

        local pattern = "(%d+)%-(%d+)%-(%d+)T(%d+):(%d+):(%d+)%.(%d+)"
        local _, _, _, start_hour, start_minute, _ = start_date:match(pattern)
        local _, _, _, end_hour, end_minute, _ = end_date:match(pattern)
        local start_time = start_hour .. ":" .. start_minute
        local end_time = end_hour .. ":" .. end_minute
        local now = os.date("*t")

        local muted = false
        if (start_hour * 60 + start_minute) < (now.hour * 60 + now.min) then
            muted = true
        end

        table.insert(events, #events + 1, event.new({
            description = subject,
            start_time = start_time,
            end_time = end_time,
            location = location,
            muted = muted,
            count = #events
        }))

        if (start_hour == now.hour) and (start_minute == now.min) then
            naughty.notify({
                preset = naughty.config.presets.normal,
                title = "Meeting",
                text = subject
            })
        end
    end

    local container = wibox.widget {
        layout = wibox.layout.manual,
        forced_width = 400,
        table.unpack(events)
    }

    widget:add(container)
end

local function update()
    local command = "jq '' /tmp/desktopevents/data.json"
    awful.spawn.easy_async_with_shell(command, function(output)
        local widget = registry.get("splashscreen::meetings")
        set_children(widget, output)
    end)
end

function meetings.widget()
    local widget = wibox.widget {
        layout = wibox.layout.fixed.vertical,
    }
    registry.add("splashscreen::meetings", widget)
    update()

    gears.timer {
        timeout   = 60,
        call_now  = true,
        autostart = true,
        callback  = function()
            update()
        end
    }
    return widget
end

return meetings
