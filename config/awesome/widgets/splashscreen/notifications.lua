local beautiful = require("beautiful")
local naughty = require("naughty")

local widget = require("lib.widget")

-- @module notifications
local notifications = {}

function notifications.widget()

    local icon = widget.icon({ icon = beautiful.alarm_icon })
    -- TODO fix
    notifications.icon = icon

    notifications.update_state()

    return icon
end

function notifications.update_state()
    if naughty.is_suspended() then
        notifications.icon.image = beautiful.alarm_off_icon
    else
        notifications.icon.image = beautiful.alarm_icon
    end
end

return notifications
