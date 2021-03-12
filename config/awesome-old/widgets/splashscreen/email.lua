-- awesome modules
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local helpers = require("helpers")
local pad = helpers.pad
local registry = require("widgets.registry")
local libwidget = require("lib.widget")
-- luarocks modules
local dkjson = require("dkjson")

-- @module email
local email = {}

local function create_children(output)
    local data, _, err = dkjson.decode(output, 1, nil)
    if err then
        helpers.debug("failed to parse desktopevents/data.json")
    end

    local widgets = {}
    local header = libwidget.textbox({ font = "sans bold 18"}, "centered")
    header.markup = "Email"
    table.insert(widgets, header)
    table.insert(widgets, pad(1))
    -- local icon = libwidget.icon({
    --         icon = beautiful.mail_icon,
    --         forced_height = dpi(50),
    --         forced_width = dpi(50)
    -- })
    -- table.insert(widgets, icon)

    local count = 0
    for _, _ in ipairs(data["unread_emails"]) do
        count = count + 1
    end

    local textbox = libwidget.textbox({ font = "sans bold 30"}, "centered")
    local work_text = helpers.colorize_text(tostring(count), beautiful.xcolor14)
    local personal_text = helpers.colorize_text(tostring(0), beautiful.xcolor1)
    textbox.markup = work_text.." / "..personal_text
    table.insert(widgets, textbox)

    return widgets
end

local function update()
    local command = "jq '' /tmp/desktopevents/data.json"
    awful.spawn.easy_async_with_shell(command, function(output)
        local widget = registry.get("splashscreen::email")
        local children = create_children(output)
        widget:set_children(children)
    end)
end

function email.widget()
    local widget = wibox.widget {
        layout = wibox.layout.fixed.vertical
    }
    registry.add("splashscreen::email", widget)
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

return email
