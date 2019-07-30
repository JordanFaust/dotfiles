-- awesome modules
local gears = require("gears")
local naughty = require("naughty")
local wibox = require("wibox")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
-- custom modules
local lgi = require("lgi")
local proxy = require("dbus_proxy")
local registry = require("widgets.registry")
local nordic = {
    widget = require("nordic.widget")
}

-- @module meetings
local meetings = {
    -- Indicates the DBus service is connected
    connected = false,
    mt = {}
}

--- Send the ChangeDate message with a new date range to the CalendarInterface of
-- the eventable service. This updates the date range the calendar will return data on.
local function change_date(start_time, end_time)
    if meetings.connected then
        local client = proxy.Proxy:new({
            bus = proxy.Bus.SESSION,
            name = "org.eventable.service",
            path = "/org/eventable/Calendar",
            interface = "org.eventable.CalendarInterface"
        })

        client:ChangeDate(start_time, end_time)
    end
end

--- Triggered when the PropertiesChanged signal is sent from the Eventable service.
-- This signal is sent when any properties within the CalendarInterface changes, such
-- as the metadata for the calendar.
local function properties_changed(...) --luacheck: no unused args
    if meetings.connected then
        local client = proxy.Proxy:new({
            bus = proxy.Bus.SESSION,
            name = "org.eventable.service",
            path = "/org/eventable/Calendar",
            interface = "org.eventable.CalendarInterface"
        })

        local widget = registry.get("agenda::meetings")

        -- Reset the widget before re-adding the child widgets
        widget:reset()
        local header = wibox.widget {
            {
                font = "sans bold 18",
                markup = "Meetings",
                valign = "center",
                align = "center",
                widget = wibox.widget.textbox
            },
            top = 0,
            bottom = dpi(20),
            left = dpi(20),
            right = dpi(20),
            widget = wibox.container.margin
        }
        widget:add(header)

        local events = {}
        for _, event in pairs(client:Metadata()) do
            local start_date = event["start_time"]
            local end_date = event["end_time"]
            local subject = event["summary"]
            local location = event["location"]

            local pattern = "(%d+)%-(%d+)%-(%d+)T(%d+):(%d+):(%d+)-05:00"
            local _, _, start_day, start_hour, start_minute, _ = start_date:match(pattern)
            local _, _, _, end_hour, end_minute, _ = end_date:match(pattern)
            local start_time = start_hour .. ":" .. start_minute
            local end_time = end_hour .. ":" .. end_minute
            local now = os.date("*t")

            local muted = false
            -- mute the color of the event if it has already passed
            if (start_hour * 60 + start_minute) < (now.hour * 60 + now.min) then
                muted = true
            end
            -- if the start day is less then today mute the event
            if (tonumber(start_day) < tonumber(now.day)) then
                muted = true
            end

            table.insert(events, #events + 1, nordic.widget.event {
                description = subject,
                start_time = start_time,
                end_time = end_time,
                location = location,
                muted = muted,
                count = #events
            })

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
            forced_width = dpi(400),
            table.unpack(events)
        }

        widget:add(container)
    end
end

--- Sets up signal handling for the PropertiesChanged signal for the
-- org.eventable.CalendarInterface of the eventable service. This signal
-- is sent any time there is new data.
local function watch_calendar()
    local Gio = lgi.Gio
    local client = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.eventable.service",
        path = "/org/eventable/Calendar",
        interface = "org.eventable.CalendarInterface",
        flags = Gio.DBusSignalFlags.NONE
    })

    client:connect_signal(properties_changed, "PropertiesChanged")
end

--- Start watching the session dbus for the Eventable service.
-- When the eventable service is connected it starts watching for signals
-- from the service and enables sending message to the service. When
-- connection is lost, it marks the connection as lost to prevent crashes
-- from attempting to send a message to a non existent service.
local function dbus_watch()
    local Gio = lgi.Gio
    local watch_proxy = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.freedesktop.DBus",
        path = "/org/freedesktop/DBus",
        interface = "org.freedesktop.DBus",
        flags = Gio.DBusSignalFlags.NONE
    })

    -- Detect when the eventable service is started
    watch_proxy:connect_signal(
        function(_, changed, last)
            if changed == "org.eventable.service" then
                -- Mark the connection as connected or not. If last is equal to "" then
                -- the service was just connected. If it has an ID and the string is not
                -- empty then the connection was just lost.
                meetings.connected = last == ""
                watch_calendar()
            end
        end,
        "NameOwnerChanged"
    )

    -- Check if the eventable service is already connected
    local services = proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.freedesktop.DBus",
        path = "/org/freedesktop/DBus",
        interface = "org.freedesktop.DBus",
    })
    local connected, _ = services:ListNames()

    -- check to see if player is currently running. If it is start the watches
    -- on that service.
    for _, name in pairs(connected) do
        if name == "org.eventable.service" then
            -- Mark the service as connected if the name is found within the established
            -- list of connected dbus services.
            meetings.connected = true
            watch_calendar()
            properties_changed()
        end
    end
end

--- Create the panel that the agenda will be contained in
-- @method new
-- @return The panel
function meetings.new(args) --luacheck: no unused args
    local widget = wibox.widget {
        layout = wibox.layout.fixed.vertical
    }

    -- Trigger a date change within the eventable service when a new
    -- date is selected within the calendar
    widget:connect_signal("update::value", function(_, params)
        local start_time = os.date("%Y-%m-%dT00:00:00-05:00", os.time(params))
        local end_time = os.date("%Y-%m-%dT23:59:59-05:00", os.time(params))
        change_date(start_time, end_time)
    end)

    registry.add("agenda::meetings", widget)

    dbus_watch()

    return widget
end

function meetings.mt:__call(...) --luacheck: no unused args
    return meetings.new(...)
end

return setmetatable(meetings, meetings.mt)
