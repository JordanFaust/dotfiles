-- awesome
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- custom
local registry = require("widgets.registry")
local core = require("nordic.core")

-- @module nordic.components.agenda.searchbar
local searchbar = { mt = {} }

--- Create the search bar widget
-- @method new
-- @tparam string args.target The name of the registered parent component
-- @tparam string args.icon The icon for the search bar
-- @tparam[opt=24] int args.icon_size The size of the icon
-- @tparam[opt=beautiful.xbackground] string args.background The background of the searchbox
-- @tparam[opt=Search Applications] string args.text The default text of the search bar
-- @tparam[opt=Roboto medium 13] string args.font The font of the search bar
-- @return The search bar widget
function searchbar.new(args)
    local target = args.target
    assert(target, "args.target is nil")
    local icon = args.icon or beautiful.magnify_icon
    assert(icon, "args.icon is nil")
    local icon_size = args.icon_size or 24
    local background = args.background or beautiful.xbackground
    local text = args.text or "Search Applications"
    local font = args.font or "Roboto medium 13"

    local widget = wibox.widget {
        {
            {
                {
                    image = beautiful.magnify_icon,
                    forced_height = dpi(icon_size),
                    forced_width = dpi(icon_size),
                    widget = wibox.widget.imagebox
                },
                top = dpi(12),
                bottom = dpi(12),
                left = dpi(24),
                right = dpi(20),
                widget = wibox.container.margin
            },
            {
                {
                    text = text,
                    font = font,
                    widget = wibox.widget.textbox
                },
                top = dpi(16),
                bottom = dpi(12),
                widget = wibox.container.margin
            },
            layout = wibox.layout.fixed.horizontal
        },
        bg = background,
        forced_height = dpi(48),
        forced_width = dpi(400),
        widget = wibox.container.background
    }

    widget:connect_signal("mouse::enter", function()
        widget.bg = core.color.lighten(background, 40)
    end)

    widget:connect_signal("mouse::leave", function()
        widget.bg = background
    end)

    widget:buttons(
        gears.table.join(
            awful.button({}, 1, function()
                gears.debug.dump("clicked search")
                registry.emit(target, "search")
            end)
        )
    )

    return widget
end

function searchbar.mt:__call(...) --luacheck: no unused args
    return searchbar.new(...)
end

return setmetatable(searchbar, searchbar.mt)
