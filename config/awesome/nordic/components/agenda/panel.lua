-- awesome
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local base = require("wibox.widget.base")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- custom
local apps = require("config.apps")
local registry = require("widgets.registry")

-- @module nordic.components.agenda.panel
local panel = { mt = {} }

--- Registered signals

--- Close the panel.
-- @signal close

--- Open the panel.
-- @signal open
-- @tparam[opt=nil] boolean search Open the panel with search activated

--- Toggle the panel
-- @signal toggle

--- Start the rofi search.
-- @signal search

--- Create the panel that the agenda will be contained in
-- @method new
-- @tparam string args.name The name of the registered component
-- @tparam string args.screen The screen the panel will be created on
-- @tparam[opt=400] int args.width The width of the opened panel
-- @tparam[opt=beautiful.xbackground] string args.background The background color
-- @tparam[opt={}] table string args.widget_template The custom widget to place within the panel
-- @return The panel
function panel.new(args)
    local name = args.name
    assert(name, "args.name is nil")
    local screen = args.screen
    assert(screen, "args.screen is nil")
    local width = args.width or dpi(400)
    local background = args.background or beautiful.xbackground
    local widget_template = args.widget_template or {}

    local widget = wibox {
        screen = args.screen,
        height = args.screen.geometry.height,
        x = args.screen.geometry.x,
        y = args.screen.geometry.y,
        ontop = true,
        bg = background,
        fg = beautiful.xforeground
    }

    widget.opened = false

    local backdrop = wibox {
        screen = args.screen,
        height = args.screen.geometry.height,
        x = args.screen.geometry.x,
        y = args.screen.geometry.y,
        ontop = true,
        type = 'dock',
        bg = background,
    }

    widget:connect_signal("search", function()
        _G.awesome.spawn(
            apps.rofi.command,
            false,
            false,
            false,
            false,
            function()
                registry.emit(name, "toggle")
            end
        )
    end)

    widget:connect_signal("open", function(self, params)
        self.opened = true
        self.width = width
        backdrop.visible = true
        -- toggle visibility to force panel to the top
        self.visible = false
        self.visible = true
        self:get_children_by_id("panel_content")[1].visible = true
        if params.search then
            self:emit_signal("search")
        end
        self:emit_signal("opened")
        registry.emit(name, "opened")
    end)

    widget:connect_signal("close", function(self)
        self.opened = false
        self.width = 1
        self:get_children_by_id("panel_content")[1].visible = false
        backdrop.visible = false
        self:emit_signal("closed")
        registry.emit(name, "closed")
    end)

    widget:connect_signal("toggle", function(self, params)
        self.opened = not self.opened
        if self.opened then
            self:emit_signal("open", params)
        else
            self:emit_signal("close")
        end
    end)

    backdrop:buttons(
        gears.table.join(
            awful.button({}, 1, function()
                registry.emit(name, "toggle", { search = false })
            end)
        )
    )

    registry.add(name, widget)

    local custom_widget = base.make_widget_from_value(widget_template)

    widget:setup {
        {
            id = "panel_content",
            bg = background,
            widget = wibox.container.background,
            visible = false,
            forced_width = width,
            {
                custom_widget,
                layout = wibox.layout.stack
            }
        },
        layout = wibox.layout.align.horizontal,
    }

    return widget
end

function panel.mt:__call(...) --luacheck: no unused args
    return panel.new(...)
end

return setmetatable(panel, panel.mt)
