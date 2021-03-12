-- awesome modules
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local gears = require('gears')
-- custom modules
local registry = require("widgets.registry")

-- @module nordic.components.bar
local bar = {
    mt = {},
    widgets = {
        tags = require("nordic.components.bar.tags"),
        tasks = require("nordic.components.bar.tasks"),
        date = require("nordic.components.bar.date"),
        vpn = require("nordic.components.bar.vpn"),
        power = require("nordic.components.bar.power"),
        bluetooth = require("nordic.components.bar.bluetooth"),
        player = require("nordic.components.bar.player"),
        buttons = {
            agenda = require("nordic.components.bar.agenda"),
            notifications = require("nordic.components.bar.notifications")
        }
    }
}

--- Creates the bar component
-- @method new
-- @tparam string args.name The name of the registered component
-- @tparam string args.screen The screen the component will be created on
-- @tparam[opt=400] int args.offset The offset from the edge of the screen
-- @tparam[opt=nil] table args.agenda_name The agenda component name
-- @return The bar component
function bar.new(args)
    local name = args.name
    assert(name, "args.name is nil")
    local screen = args.screen
    assert(screen, "args.screen is nil")
    local agenda_name = args.agenda_name or "agenda"
    local offset = args.offset or dpi(400)
    local width = args.screen.geometry.width - (offset * 2)

    local panel = wibox({
        ontop = true,
        screen = screen,
        height = dpi(48),
        width = width,
        x = args.screen.geometry.x + offset,
        y = args.screen.geometry.y + 16,
        stretch = false,
        bg = beautiful.frost_2,
        fg = beautiful.xforeground,
        struts = {
            top = dpi(48)
        }
    })

    local date = bar.widgets.date.new(args.date or {})
    panel:setup {
        -- Left
        {
            -- bar.widgets.buttons.agenda.new(args.buttons.agenda or {}),
            bar.widgets.buttons.agenda {
                -- TODO: MAKE THIS BETTER
                target = agenda_name,
                signal = "toggle"
            },
            bar.widgets.tags {
                screen = screen
            },
            bar.widgets.tasks {
                screen = screen,
                name = name
            },
            layout = wibox.layout.fixed.horizontal
        },
        -- Middle
        {
            date,
            layout = wibox.layout.manual
        },
        -- Right
        {
            bar.widgets.player.new({}),
            bar.widgets.bluetooth.new({}),
            bar.widgets.vpn.new({}),
            bar.widgets.power.new({}),
            bar.widgets.buttons.notifications.new({}),
            layout = wibox.layout.fixed.horizontal
        },
        forced_width = width,
        layout = wibox.layout.align.horizontal
    }
    -- force redrawing to center the date within the bar
    panel:connect_signal("redraw::needed", function(_)
        date:emit_signal("widget::layout_changed")
    end)
    registry.add("layout::bar", panel)

    return panel
end

function bar.mt:__call(...) --luacheck: no unused args
    return bar.new(...)
end

return setmetatable(bar, bar.mt)
