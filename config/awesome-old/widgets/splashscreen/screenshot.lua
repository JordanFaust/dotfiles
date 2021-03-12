-- awesome modules
local beautiful = require("beautiful")
-- custom modules
local libwidget = require("lib.widget")

-- @module screenshot
local screenshot = {}

function screenshot.widget()
    return libwidget.icon({ icon = beautiful.screenshot_icon })
end

return screenshot
