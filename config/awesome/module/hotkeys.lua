-- awesome modules
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local hotkeys_popup = require("awful.hotkeys_popup").widget
local wibox = require("wibox")
-- custom modules
local registry = require("widgets.registry")

-- TODO: Create a better hotkeys popup that follows my general style. Wrapping for now

-- @module hotkeys
local hotkeys = {}

function hotkeys.init()
    local popup = hotkeys_popup.new({
        hide_without_description = true,
        merge_duplicates = true,
        width = dpi(1200),
        height = dpi(800),
        bg = beautiful.xbackground,
        fg = beautiful.xforeground,
        border_color = beautiful.xbackground,
        modifiers_fg = beautiful.xforeground,
        border_width = dpi(32),
        font = "Fira Mono 14",
        description_font = "Fira Mono Italic 10",
        group_margin = dpi(5)
    })

    -- Create a dummy widget for connecting a signal to the hotkeys popup
    local widget = wibox.widget {
        layout = wibox.layout.fixed.veritical
    }

    widget:connect_signal("hotkeys::show", function()
        popup:show_help()
    end)

    registry.add("hotkeys", widget)
end

hotkeys.init()
