-- awesome
local wibox = require("wibox")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- custom
-- local registry = require("widgets.registry")

-- @module nordic.components.agenda
local agenda = {
    mt = {},
    widgets = {
        panel = require("nordic.components.agenda.panel"),
        searchbar = require("nordic.components.agenda.searchbar"),
        monitoring = require("nordic.components.agenda.monitoring"),
        calendar = require("nordic.components.agenda.calendar"),
        meetings = require("nordic.components.agenda.meetings"),
    },
    config = require("nordic.components.agenda.config")
}

--- Creates the agenda component
-- @method new
-- @tparam string args.name The name of the registered component
-- @tparam string args.screen The screen the component will be created on
-- @tparam int args.width The width of the component
-- @tparam string args.background The background color of the component
-- @return The agenda component
function agenda.new(args)
    local screen = args.screen
    local name = args.name or "agenda"
    local width = args.width or dpi(400)
    local background = args.background

    local panel = agenda.widgets.panel {
        name = name,
        screen = screen,
        width = width,
        background = background,
        widget_template = {
            {
                agenda.widgets.searchbar {
                    target = name,
                },
                layout = wibox.layout.fixed.vertical
            },
            {
                {
                    {
                        {
                            {
                                widget = wibox.container.mirror,
                                reflection = { horizontal = true, vertical = false },
                                agenda.widgets.monitoring()
                            },
                            margins = 50,
                            widget = wibox.container.margin,
                        },
                        layout = wibox.layout.fixed.vertical
                    },
                    bg = "#ff008000",
                    fg = "#ffffff",
                    widget = wibox.container.background
                },
                {
                    agenda.widgets.calendar.new({}),
                    left = dpi(36),
                    right = dpi(48),
                    widget = wibox.container.margin
                },
                agenda.widgets.meetings {
                    timeout = 60
                },
                layout = wibox.layout.fixed.vertical
            },
            layout = wibox.layout.align.vertical,
        }
    }

    agenda.config.register({
        name = name
    })

    return panel
end

function agenda.mt:__call(...) --luacheck: no unused args
    return agenda.new(...)
end

return setmetatable(agenda, agenda.mt)
