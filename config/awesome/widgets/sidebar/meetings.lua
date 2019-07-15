-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local globals = require("globals")
local helpers = require("helpers")
local pad = helpers.pad
local registry = require("widgets.registry")
local libwidget = require("lib.widget")
-- luarocks modules
local dkjson = require("dkjson")

-- @module meetings
local meetings = {}

-- Create an agenda that supports sorting
--
-- <p> Creates a table that includes details about today.
-- The table uses an index to support sorting. </p>
--
-- @return a table with meeting times in intervals of 30 minutes.
local function create_agenda()
    return {
        ["08:00"] = {
            index = 1,
            text = "------------------------------------------------"
        },
        ["08:30"] = {
            index = 2,
            text = "------------------------------------------------"
        },
        ["09:00"] = {
            index = 3,
            text = "------------------------------------------------"
        },
        ["09:30"] = {
            index = 4,
            text = "------------------------------------------------"
        },
        ["10:00"] = {
            index = 5,
            text = "------------------------------------------------"
        },
        ["10:30"] = {
            index = 6,
            text = "------------------------------------------------"
        },
        ["11:00"] = {
            index = 7,
            text = "------------------------------------------------"
        },
        ["11:30"] = {
            index = 8,
            text = "------------------------------------------------"
        },
        ["12:00"] = {
            index = 9,
            text = "------------------------------------------------"
        },
        ["12:30"] = {
            index = 10,
            text = "------------------------------------------------"
        },
        ["13:00"] = {
            index = 11,
            text = "------------------------------------------------"
        },
        ["13:30"] = {
            index = 12,
            text = "------------------------------------------------"
        },
        ["14:00"] = {
            index = 13,
            text = "------------------------------------------------"
        },
        ["14:30"] = {
            index = 14,
            text = "------------------------------------------------"
        },
        ["15:00"] = {
            index = 15,
            text = "------------------------------------------------"
        },
        ["15:30"] = {
            index = 16,
            text = "------------------------------------------------"
        },
        ["16:00"] = {
            index = 17,
            text = "------------------------------------------------"
        },
        ["16:30"] = {
            index = 18,
            text = "------------------------------------------------"
        },
        ["17:00"] = {
            index = 19,
            text = "------------------------------------------------"
        }
    }
end

local function spairs(t, order)
    -- collect the keys
    local keys = {}
    for k in pairs(t) do keys[#keys+1] = k end

    -- if order function given, sort by it by passing the table and keys a, b,
    -- otherwise just sort the keys
    if order then
        table.sort(keys, function(a,b) return order(t, a, b) end)
    else
        table.sort(keys)
    end

    -- return the iterator function
    local i = 0
    return function()
        i = i + 1
        if keys[i] then
            return keys[i], t[keys[i]]
        end
    end
end

local function sort(tbl, a, b)
    return tbl[b].index > tbl[a].index
end

local function split(str, separator)
    if separator == nil then
        separator = "%s"
    end

    local strings = {}
    for s in string.gmatch(str, "([^"..separator.."]+)") do
        table.insert(strings, s)
    end

    return strings
end

local function set_children(widget, output)
    local data, _, err = dkjson.decode(output, 1, nil)
    if err then
        helpers.debug("failed to parse desktopevents/data.json")
    end
    local agenda = create_agenda()
    local length = 30

    for _, meeting in ipairs(data["meetings"]) do
        local hover = beautiful.xcolor14

        local timestamp = meeting["start"]["dateTime"]
        local subject = meeting["subject"]
        local location = meeting["location"]["displayName"]

        local pattern = "(%d+)%-(%d+)%-(%d+)T(%d+):(%d+):(%d+)%.(%d+)"
        local _, _, _, hour, minute, _ = timestamp:match(pattern)
        local now = os.date("*t")

        local line =  hour .. ":" .. minute .. "  " .. subject .. "        "
        local text = string.sub(line, 1, length)
        local textbox = libwidget.textbox({ font = "Fira Mono 13" }, "centered")

        -- If the meeting time has already passed, visually indicate that
        -- local overdue = false
        if (hour * 60 + minute) < (now.hour * 60 + now.min) then
            -- overdue = true
            -- textbox.markup = helpers.strikethrough_text(text)
            textbox.markup = text
            -- helpers.debug(helpers.strikethrough_text(text))
        else
            textbox.markup = text
        end

        -- Hover effect
        textbox:connect_signal("mouse::enter", function ()
            textbox.markup = helpers.colorize_text(text, hover)
        end)
        textbox:connect_signal("mouse::leave", function ()
            -- if overdue then
                -- textbox.markup = helpers.strikethrough_text(text)
            -- else
            textbox.markup = text
            -- end
        end)

        -- Buttons
        if string.find(location, "http?") then
            local location_parts = split(location)
            textbox:buttons(
                gears.table.join(
                    awful.button({ }, 1, function ()
                        -- helpers.debug(location_parts[1])
                        awful.spawn(globals.browser.." "..location_parts[1])
                        registry.emit("splashscreen", "splashscreen::hide")
                    end)
                )
            )
        end

        agenda[hour..":"..minute].textbox = textbox
    end

    -- Reset the widget before re-adding the child widgets
    widget:reset()
    local header = libwidget.textbox({ font = "sans bold 18"}, "centered")
    header.markup = "Meetings"
    widget:add(header)
    widget:add(pad(1))

    for time, config in spairs(agenda, sort) do
        local textbox
        if config.textbox == nil then
            textbox = libwidget.textbox({ font = "Fira Mono 13" }, "centered")
            textbox.markup = string.sub(time .. "  ----------------------------------", 1, length)
        else
            textbox = config.textbox
        end
        textbox.forced_width = 25
        textbox.forced_height = 25
        widget:add(textbox)
    end
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
        layout = wibox.layout.fixed.vertical
    }
    registry.add("splashscreen::meetings", widget)
    update()

    gears.timer {
        timeout   = 300,
        call_now  = true,
        autostart = true,
        callback  = function()
            update()
        end
    }
    return widget
end

return meetings
