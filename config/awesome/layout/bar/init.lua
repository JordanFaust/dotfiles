-- awesome modules
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local gears = require('gears')
-- custom modules
local registry = require("widgets.registry")

-- @module layout.bar
local bar = {
    mt = {},
    widgets = {
        tags = require("layout.bar.tags"),
        tasks = require("layout.bar.tasks"),
        date = require("layout.bar.date"),
        vpn = require("layout.bar.vpn"),
        power = require("layout.bar.power"),
        bluetooth = require("layout.bar.bluetooth"),
        player = require("layout.bar.player"),
        buttons = {
            agenda = require("layout.bar.agenda"),
            notifications = require("layout.bar.notifications")
        }
    }
}


function bar.new(args)
    local offset = args.offset or dpi(400)
    local width = args.screen.geometry.width - (offset * 2)

    if args.buttons == nil then
        args.buttons = {}
    end
    args.tasks = gears.table.join(args.tasks or {}, { screen = args.screen })
    args.tags = gears.table.join(args.tags or {}, { screen = args.screen })
    args.date = gears.table.join(args.date or {}, { width = width })

    local panel = wibox({
        ontop = true,
        screen = args.screen,
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
            bar.widgets.buttons.agenda.new(args.buttons.agenda or {}),
            bar.widgets.tags.new(args.tags or {}),
            bar.widgets.tasks.new(args.tasks or {}),
            layout = wibox.layout.fixed.horizontal
        },
        -- Middle
        {
            date,
            layout = wibox.layout.manual
        },
        -- Right
        {
            bar.widgets.player.new(args.player),
            bar.widgets.bluetooth.new(args.bluetooth),
            bar.widgets.vpn.new(args.vpn),
            bar.widgets.power.new(args.power),
            bar.widgets.buttons.notifications.new(args.buttons.notifications or {}),
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
