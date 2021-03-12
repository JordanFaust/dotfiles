local awful = require('awful')
local beautiful = require('beautiful')
local wibox = require('wibox')
-- custom modules
local helpers = require("helpers")
local registry = require("widgets.registry")

local prompt = {}

-- Create a shortcut function
function prompt.run()
    local input = wibox.widget {
        forced_height = 20,
        forced_width = awful.screen.focused().geometry.width * 0.50,
        widget = wibox.widget.textbox
    }
    -- local output = wibox.widget {
    --     forced_height = 300,
    --     forced_width = _G.screen.geometry.width * 0.50,
    --     widget = wibox.widget.textbox
    -- }

    awful.prompt.run {
        prompt = "Run: ",
        textbox = input,
        -- evaluate lua code
        exe_callback = awful.util.eval
    }
end

return prompt
