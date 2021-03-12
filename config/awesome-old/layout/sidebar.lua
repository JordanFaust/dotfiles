local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
-- custom modules
local helpers = require("helpers")
local mat_icon = require("widgets.material.icon")
local mat_list_item = require('widgets.material.list-item')
local pad = helpers.pad
local registry = require("widgets.registry")
local time = require("widgets.sidebar.time")
local date = require("widgets.sidebar.date")
local fancydate = require("widgets.sidebar.fancydate")
local meetings = require("widgets.sidebar.meetings")
local monitoring = require("widgets.sidebar.monitoring")

local apps = require("config.apps")

-- @module sidebar
local sidebar = {}

local function searchbar_widget()
    local widget = wibox.widget {
        wibox.widget {
            icon = beautiful.magnify_icon,
            size = dpi(24),
            widget = mat_icon
        },
        wibox.widget {
            text = 'Search Applications',
            font = 'Roboto medium 13',
            widget = wibox.widget.textbox
        },
        clickable = true,
        widget = mat_list_item
    }

    widget:buttons(
        gears.table.join(
            awful.button({}, 1, function()
                registry.emit("dashboard", "dashboard::search")
            end)
        )
    )

    return widget
end

function sidebar.create(screen)
    local panel_content_width = dpi(400)

    local panel = wibox {
        screen = screen,
        height = screen.geometry.height,
        x = screen.geometry.x,
        y = screen.geometry.y,
        ontop = true,
        bg = beautiful.xbackground,
        fg = beautiful.xforeground
    }

    panel.opened = false

    local backdrop = wibox {
        screen = screen,
        height = screen.geometry.height,
        x = screen.geometry.x,
        y = screen.geometry.y,
        ontop = true,
        type = 'dock',
        bg = beautiful.xbackground,
    }

    panel:connect_signal("dashboard::search", function()
        _G.awesome.spawn(
            apps.rofi.command,
            false,
            false,
            false,
            false,
            function()
                registry.emit("dashboard", "dashboard::toggle")
            end
        )
    end)

    panel:connect_signal("dashboard::open", function(self, params)
        self.opened = true
        self.width = panel_content_width
        backdrop.visible = true
        -- toggle visibility to force panel to the top
        self.visible = false
        self.visible = true
        self:get_children_by_id("panel_content")[1].visible = true
        if params.search then
            self:emit_signal("dashboard::search")
        end
        self:emit_signal("dashboard::opened")
        registry.emit("agenda", "dashboard::opened")
    end)

    panel:connect_signal("dashboard::close", function(self)
        self.opened = false
        self.width = 1
        self:get_children_by_id("panel_content")[1].visible = false
        backdrop.visible = false
        self:emit_signal("dashboard::closed")
        registry.emit("agenda", "dashboard::closed")
    end)

    panel:connect_signal("dashboard::toggle", function(self, params)
        self.opened = not self.opened
        if self.opened then
            self:emit_signal("dashboard::open", params)
        else
            self:emit_signal("dashboard::close")
        end
    end)

    backdrop:buttons(
        gears.table.join(
            awful.button({}, 1, function()
                registry.emit("dashboard", "dashboard::toggle", { search = false })
            end)
        )
    )

    registry.add("dashboard", panel)

    local _searchbar = searchbar_widget()

    local agenda = wibox.widget {
        { ----------- TOP GROUP -----------
            _searchbar,
            layout = wibox.layout.fixed.vertical
        },
        { ----------- MIDDLE GROUP -----------
            {

                widget = wibox.container.background,
                bg = "#ff008000",
                fg = "#ffffff",
                {
                    layout = wibox.layout.fixed.vertical,
                    {
                        widget = wibox.container.margin,
                        margins = 50,
                        {
                            widget = wibox.container.mirror,
                            reflection = { horizontal = true, vertical = false },
                            monitoring.new()
                        }
                    }
                }
            },
            meetings.widget(),
            layout = wibox.layout.fixed.vertical
        },
        layout = wibox.layout.align.vertical,
    }

    panel:setup {
        {
            id = 'panel_content',
            bg = beautiful.xbackground,
            widget = wibox.container.background,
            visible = false,
            forced_width = panel_content_width,
            {
                agenda,
                layout = wibox.layout.stack
            }
        },
        layout = wibox.layout.align.horizontal,
    }

    registry.add("sidebar", agenda)

    return agenda
end

return sidebar
